open Account
open Tezos_data_encoding
open Data_encoding
open Tezos_error_monad
open Error_monad

type t = int

type big_map_diff_item = {
  diff_key : Script.expr;
  diff_key_hash : Script.expr_hash;
  diff_value : Script.expr option;
}

let init_contracts (n: int) =
  range 0 n

let compare _ _ =
  0

let encoding = int8
let of_b58check _ = ok 0

let to_b58check _ = ""

let exists _ _ = Lwt.return @@ ok true

let get_script c _ : (Context_type.t * Script.t option) tzresult Lwt.t =
  return (c, None)

let fresh_contract_from_current_nonce ctxt = Lwt.return @@ ok (ctxt, 0)

let implicit_contract _ = 0

let get_balance _ _ = Lwt.return @@ ok @@ Tez.zero
module Big_map = struct
  let mem ctxt _ _ = Lwt.return @@ ok (ctxt, false)

  let get_opt ctxt _ _ = Lwt.return @@ ok (ctxt, None)
end