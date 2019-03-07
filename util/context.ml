open Context_type

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

let with_block_gas gas_count ctxt =
  {ctxt with block_gas = Z.of_int gas_count}

let with_maximum_gas gas_limit ctxt =
  {ctxt with gas = Limited {remaining = (Z.of_int gas_limit)} }

let initialize_account_balance ~index ~balance ctxt =
  let bindings = Storage_map_mod.bindings ctxt.storage_map in
  let contract_hash = fst @@ List.nth bindings index in
  let update_balance_fun old_balance = 
    match old_balance with
    | Some _ -> Some balance
    | None -> None 
  in
  {ctxt with storage_map = Storage_map_mod.update contract_hash update_balance_fun ctxt.storage_map }