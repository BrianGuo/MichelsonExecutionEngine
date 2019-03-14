open Context_type
open Tezos_error_monad.Error_monad
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

let get_contracts_and_storage ctxt = 
  Storage_map_mod.bindings ctxt.storage_map
  
let with_block_gas gas_count ctxt =
  {ctxt with block_gas = Z.of_int gas_count}

let with_maximum_gas gas_limit ctxt =
  {ctxt with gas = Limited {remaining = (Z.of_int gas_limit)} }

let exists ctxt contract =
  return @@ Context_type.Storage_map_mod.mem contract ctxt.storage_map

let get_script ctxt contract =
  let script_opt = Storage_map_mod.find_opt contract ctxt.storage_map in
  match script_opt with
  | Some n -> return (ctxt, (n.script))
  | None -> return (ctxt, None)

let increment_origination_nonce ctxt =
  match ctxt.origination_nonce with
  | None -> error Contract.Undefined_operation_nonce
  | Some origination_nonce ->
  let origination_index = Int32.succ origination_nonce.origination_index in
  let new_nonce = {origination_nonce with origination_index} in
  ok ({ctxt with origination_nonce = Some new_nonce}, new_nonce)

let fresh_contract_from_current_nonce ctxt =
  Lwt.return (increment_origination_nonce ctxt) >>=? fun (c, nonce) ->
    return (c, Contract.originated_contract nonce)

let get_balance ctxt contract = 
  let balance = 
  match  Storage_map_mod.find_opt contract ctxt.storage_map with 
  | Some stored_data -> stored_data.balance
  | None -> Int64.zero
  in
  return @@ Tez.of_mutez_exn balance


let init_contracts n ctxt =
  let accounts = Account.generate_accounts n in
  let contracts = List.map (fun (a, _) ->
    Contract.implicit_contract Account.(a.pkh)) accounts in
  let map = Storage.init_contracts_storage contracts in
    {ctxt with storage_map = map}

let initialize_account_balance ~index ~balance ctxt =
  let bindings = Storage_map_mod.bindings ctxt.storage_map in
  let contract_hash = fst @@ List.nth bindings index in
  let update_balance_fun old_balance = 
    match old_balance with
    | Some _ -> Some balance
    | None -> None 
  in
  {ctxt with storage_map = Storage_map_mod.update contract_hash update_balance_fun ctxt.storage_map }

module Big_map = struct
  let mem ctxt contract hash =
    begin
    match Storage_map_mod.find_opt contract ctxt.storage_map with
    | Some contr ->
      begin
      match contr.data_map_opt with
      | Some storage -> 
        return @@ (ctxt, List.exists (fun big_map -> big_map.diff_key_hash = hash) storage)
      | None -> return (ctxt, false)
      end
    | None -> return (ctxt, false)
    end
  
  let get_opt ctxt contract hash = 
    begin
    match Storage_map_mod.find_opt contract ctxt.storage_map with
    | Some contr ->
      begin
      match contr.data_map_opt with
      | Some storage -> 
        let mapped_value_opt = 
          List.find_opt (fun big_map -> big_map.diff_key_hash = hash) storage
        in
        begin match mapped_value_opt with 
        | Some diff_item -> 
            return (ctxt, diff_item.diff_value)
        | None ->
            return (ctxt, None)
        end
      | None -> return (ctxt, None)
      end
    | None -> return (ctxt, None)
    end
end