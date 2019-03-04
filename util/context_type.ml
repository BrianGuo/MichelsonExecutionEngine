open Tezos_crypto
open Gas

module Contract_hash = struct
  let contract_hash = "\002\090\121" 
  include Blake2b.Make(Base58)(struct
    let name = "Contract_hash"
    let title = "A contract ID"
    let b58check_prefix = contract_hash
    let size = Some 20
  end)
end

type contract_type = 
  | Implicit of Signature.Public_key_hash.t
  | Originated of Contract_hash.t


module Operation_hash = struct
  include Blake2b.Make (Base58) (struct
    let name = "Operation_hash"
    let title = "A Tezos operation ID"
    let b58check_prefix = Base58.Prefix.operation_hash
    let size = None
  end)
end

type origination_nonce =
  { operation_hash: Operation_hash.t ;
    origination_index: int32 }

module Storage_map_mod = Map.Make(struct 
  type nonrec t = contract_type
  let compare l1 l2 =
    match l1, l2 with
      | Implicit pkh1, Implicit pkh2 ->
          Signature.Public_key_hash.compare pkh1 pkh2
      | Originated h1, Originated h2 ->
          Contract_hash.compare h1 h2
      | Implicit _, Originated _ -> -1
      | Originated _, Implicit _ -> 1
end)

type contract_storage = {
  script: Script.t option;
  balance: int64;
}

type t =
  { 
    block: Block.t;
    origination_nonce: origination_nonce option;
    storage_map: contract_storage Storage_map_mod.t;
    gas: Gas.t;
    block_gas: Z.t;
  }

let default_context = {
  block = ([]);
  origination_nonce = None;
  storage_map = Storage_map_mod.empty;
  gas = Unaccounted;
  block_gas = Z.zero;
}
