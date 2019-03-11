open Context_type
let storage_mapping = Storage_map_mod.empty

let init_contracts_storage contracts =
  let init_zero_params k m = 
    Storage_map_mod.add 
      k 
      {
        script=None;
        balance=Int64.of_int 20
      }
      m
  in 
  List.fold_right (init_zero_params) contracts Storage_map_mod.empty
