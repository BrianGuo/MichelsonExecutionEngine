open Context_type

let storage_mapping = Storage_map_mod.empty

let init_contracts_storage contracts =
  let init_zero_params k m = 
    Storage_map_mod.add 
      k 
      {
        script=None;
        balance=Int64.zero
      }
      m
  in 
  List.fold_right (init_zero_params) contracts Storage_map_mod.empty

let get_balance _ contract = 
  match  Storage_map_mod.find_opt contract storage_mapping with 
  | Some stored_data -> stored_data.balance
  | None -> Int64.zero