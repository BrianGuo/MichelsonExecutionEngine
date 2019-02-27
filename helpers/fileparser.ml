open Misc
open Script_ir_translator
open Script_ir_nodes
open Tezos_error_monad
open Error_monad

type ('param, 'storage) toplevel = {
  param_type : 'param ty ;
  storage_type : 'storage ty ;
  code : ('param * 'storage, packed_internal_operation list * 'storage) lambda
}

let get_toplevel ?environment toplevel_path claimed_storage_type claimed_parameter_type =
  let toplevel_str = Streams.read_file toplevel_path in
  contextualize ?environment ~msg:"toplevel" @@ fun {tezos_context = context; _} ->
  let toplevel_expr = Cast.tl_of_string toplevel_str in
  let (param_ty_node, storage_ty_node, code_field) =
    force_ok_alpha ~msg:"parsing toplevel" @@
    parse_toplevel toplevel_expr in
  let (Ex_ty param_type, _) =
    force_ok_alpha ~msg:"parse arg ty" @@
    Script_ir_translator.parse_ty context ~allow_big_map:false ~allow_operation:false param_ty_node in
  let (Ex_ty storage_type, _) =
    force_ok_alpha ~msg:"parse storage ty" @@
    parse_storage_ty context storage_ty_node in
  let _ = force_ok_alpha ~msg:"storage eq" @@ Script_ir_translator.ty_eq context storage_type claimed_storage_type in
  let _ = force_ok_alpha ~msg:"param eq" @@ Script_ir_translator.ty_eq context param_type claimed_parameter_type in
  let param_type_full = Pair_t ((claimed_parameter_type, None, None),
                                (claimed_storage_type, None, None), None) in
  let ret_type_full =
    Pair_t ((List_t (Operation_t None, None), None, None),
            (claimed_storage_type, None, None), None) in
  parse_returning (Toplevel { storage_type = claimed_storage_type ; param_type = claimed_parameter_type })
    context (param_type_full, None) ret_type_full code_field >>=?? fun (code, _) ->
  return {
    param_type = claimed_parameter_type;
    storage_type = claimed_storage_type;
    code ;
  }