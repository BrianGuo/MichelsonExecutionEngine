open Account
open Tezos_data_encoding
open Tezos_crypto
open Tezos_error_monad
open Error_monad
open Context_type
open Tezos_stdlib


type t = Context_type.contract_type

include Compare.Make(struct
    type nonrec t = t
    let compare l1 l2 =
      match l1, l2 with
        | Implicit pkh1, Implicit pkh2 ->
            Signature.Public_key_hash.compare pkh1 pkh2
        | Originated h1, Originated h2 ->
            Contract_hash.compare h1 h2
        | Implicit _, Originated _ -> -1
        | Originated _, Implicit _ -> 1
  end)

type big_map_diff_item = {
  diff_key : Script.expr;
  diff_key_hash : Script.expr_hash;
  diff_value : Script.expr option;
}

let implicit_contract id = Implicit id 

let origination_nonce_encoding =
  let open Data_encoding in
  conv
    (fun { operation_hash ; origination_index } ->
       (operation_hash, origination_index))
    (fun (operation_hash, origination_index) ->
       { operation_hash ; origination_index }) @@
  obj2
    (req "operation" Operation_hash.encoding)
    (dft "index" int32 0l)

let originated_contract nonce =
  let data =
    Data_encoding.Binary.to_bytes_exn origination_nonce_encoding nonce in
  Originated (Contract_hash.hash_bytes [data])


let init_contracts (n: int) =
  range 0 n

let initial_origination_nonce =
  { operation_hash=Operation_hash.zero ; origination_index = 0l }

type error += Invalid_contract_notation of string (* `Permanent *)
type error += Undefined_operation_nonce (* `Permanent *)


let to_b58check = function
| Implicit pbk -> Signature.Public_key_hash.to_b58check pbk
| Originated h -> Contract_hash.to_b58check h

let of_b58check s =
  match Base58.decode s with
  | Some (Ed25519.Public_key_hash.Data h) -> ok (Implicit (Signature.Ed25519 h))
  | Some (Secp256k1.Public_key_hash.Data h) -> ok (Implicit (Signature.Secp256k1 h))
  | Some (P256.Public_key_hash.Data h) -> ok (Implicit (Signature.P256 h))
  | Some (Contract_hash.Data h) -> ok (Originated h)
  | _ -> error (Invalid_contract_notation s)

  let encoding =
    let open Data_encoding in
    def "contract_id"
      ~title:
        "A contract handle"
      ~description:
        "A contract notation as given to an RPC or inside scripts. \
         Can be a base58 implicit contract hash \
         or a base58 originated contract hash." @@
    splitted
      ~binary:
        (union ~tag_size:`Uint8 [
            case (Tag 0)
              ~title:"Implicit"
              Signature.Public_key_hash.encoding
              (function Implicit k -> Some k | _ -> None)
              (fun k -> Implicit k) ;
            case (Tag 1) (Fixed.add_padding Contract_hash.encoding 1)
              ~title:"Originated"
              (function Originated k -> Some k | _ -> None)
              (fun k -> Originated k) ;
          ])
      ~json:
        (conv
           to_b58check
           (fun s ->
              match of_b58check s with
              | Ok s -> s
              | Error _ -> Json.cannot_destruct "Invalid contract notation.")
           string)


let exists (ctxt : Context.t) contract = 
  Lwt.return @@ 
  ok @@ 
  Context_type.Storage_map_mod.mem contract ctxt.storage_map

let get_script
  : Context.t -> 'b -> (Context.t * Script.t option) tzresult Lwt.t 
  = fun c _ ->
  return (c, None)

let increment_origination_nonce (ctxt : Context.t) =
  match ctxt.origination_nonce with
  | None -> error Undefined_operation_nonce
  | Some origination_nonce ->
  let origination_index = Int32.succ origination_nonce.origination_index in
  let new_nonce = {origination_nonce with origination_index} in
  ok ({ctxt with origination_nonce = Some new_nonce}, new_nonce)

let fresh_contract_from_current_nonce ctxt =
    Lwt.return (increment_origination_nonce ctxt) >>=? fun (c, nonce) ->
    return (c, originated_contract nonce)
    
let get_balance ctxt contract = 
  Lwt.return @@ ok @@ Tez.of_mutez_exn @@ Storage.get_balance ctxt contract

module Big_map = struct
  let mem ctxt _ _ = Lwt.return @@ ok (ctxt, false)

  let get_opt ctxt _ _ = Lwt.return @@ ok (ctxt, None)
end