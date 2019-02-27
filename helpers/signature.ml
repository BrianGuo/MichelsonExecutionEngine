open Tezos_data_encoding
open Data_encoding
open Tezos_crypto

type public_key_hash =
  | Ed25519 of Ed25519.Public_key_hash.t
  | Secp256k1
  | P256

type public_key =
  | Ed25519 of Ed25519.Public_key.t
  | Secp256k1
  | P256

type secret_key =
  | Ed25519
  | Secp256k1
  | P256

let generate_key () =
  ((Secp256k1 : public_key_hash), (Secp256k1 : public_key), (Ed25519 : secret_key))

type t = int

module Public_key = struct
  type t = public_key

  let name = "Signature.Public_key"
  let title = "A Ed25519, Secp256k1, or P256 public key"

  let of_b58check_opt s : (public_key option) =
    match Base58.decode s with
    | Some (Ed25519.Public_key.Data public_key) -> Some (Ed25519 public_key)
    | _ -> None

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Pervasives.failwith "Unexpected data (%s)" name

  let to_b58check (h : public_key) =
    match h with
    | Ed25519 pk -> Ed25519.Public_key.to_b58check pk
    | _ -> ""

  let raw_encoding =
      def "public_key" ~description:"A Ed25519, Secp256k1, or P256 public key" @@
        union [
          case (Tag 0) Ed25519.Public_key.encoding
            ~title:"Ed25519"
            (fun (x : public_key) ->
              match x with
              | Ed25519 x -> Some x
              | _ -> None)
            (function x -> Ed25519 x);
          (* case (Tag 1) Secp256k1.Public_key.encoding
            ~title:"Secp256k1"
            (function Secp256k1 x -> Some x | _ -> None)
            (function x -> Secp256k1 x) ;
          case
            ~title:"P256" (Tag 2) P256.Public_key.encoding
            (function P256 x -> Some x | _ -> None)
            (function x -> P256 x) *)
        ]

  let encoding : (public_key Data_encoding.t) =
    splitted
      ~binary:
        (obj1 (req name raw_encoding))
      ~json:
        (def name
           ~title: (title ^ " (Base58Check-encoded)") @@
         conv
           to_b58check
           (Data_encoding.Json.wrap_error of_b58check_exn)
           string)


  let hash (pk : public_key) : public_key_hash =
    match pk with
    | Ed25519 pk -> Ed25519 (Ed25519.Public_key.hash pk)
    | _ -> Secp256k1
end

module Public_key_hash = struct
  let encoding : (public_key_hash Data_encoding.t) =
    def "public_key_hash" ~description:"A Ed25519, Secp256k1, or P256 public key hash" @@
    union [
      case (Tag 0) Ed25519.Public_key_hash.encoding
        ~title:"Ed25519"
        (fun (x : public_key_hash) ->
              match x with
              | Ed25519 x -> Some x
              | _ -> None)
        (function x -> Ed25519 x);
      (* case (Tag 1) Secp256k1.Public_key_hash.encoding
        ~title:"Secp256k1"
        (function Secp256k1 x -> Some x | _ -> None)
        (function x -> Secp256k1 x) ;
      case (Tag 2)
        ~title:"P256" P256.Public_key_hash.encoding
        (function P256 x -> Some x | _ -> None)
        (function x -> P256 x) *)
    ]
  let of_b58check_opt _ : (public_key_hash option) = Some Secp256k1
  let to_b58check _ = ""
  let compare _ _ = 0
end

let encoding : (public_key_hash Data_encoding.t) = Public_key_hash.encoding
let of_b58check_opt _ : (public_key_hash option) = Some Secp256k1
let to_b58check _ = ""
let check _ _ _ = true