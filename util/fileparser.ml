open Misc
open Script_ir_translator
open Script_ir_nodes
open Tezos_error_monad
open Error_monad
open Execution_context
open Script_interpreter 
open Program
open Data_factory

type ex_ty = Script_ir_nodes.ex_ty

type ('param, 'storage) toplevel = {
  param_type : 'param ty ;
  storage_type : 'storage ty ;
  code : ('param * 'storage, packed_internal_operation list * 'storage) lambda
}

type ex_toplevel = Ex_toplevel : ('a, 'b) toplevel -> ex_toplevel

let get_toplevel_node _ toplevel_path =
  let toplevel_str = Streams.read_file toplevel_path in
  let toplevel_expr = Cast.tl_of_string toplevel_str in
  Lwt.return @@ parse_toplevel toplevel_expr

let get_toplevel_object ?environment toplevel_path claimed_storage_type claimed_parameter_type =
  let toplevel_str = Streams.read_file toplevel_path in
  contextualize ?environment ~msg:"toplevel" @@ fun {tezos_context = context; _} ->
  let toplevel_expr = Cast.tl_of_string toplevel_str in
  let (param_ty_node, storage_ty_node, code_field) =
    force_ok ~msg:"parsing toplevel" @@
    parse_toplevel toplevel_expr in
  let (Ex_ty param_type, _) =
    force_ok ~msg:"parse arg ty" @@
    Script_ir_translator.parse_ty context ~allow_big_map:false ~allow_operation:false param_ty_node in
  let (Ex_ty storage_type, _) =
    force_ok ~msg:"parse storage ty" @@
    parse_storage_ty context storage_ty_node in
  let _ = force_ok ~msg:"storage eq" @@ Script_ir_translator.ty_eq context storage_type claimed_storage_type in
  let _ = force_ok ~msg:"param eq" @@ Script_ir_translator.ty_eq context param_type claimed_parameter_type in
  let param_type_full = Pair_t ((claimed_parameter_type, None, None),
                                (claimed_storage_type, None, None), None) in
  let ret_type_full =
    Pair_t ((List_t (Operation_t None, None), None, None),
            (claimed_storage_type, None, None), None) in
  parse_returning (Toplevel { storage_type = claimed_storage_type ; param_type = claimed_parameter_type })
    context (param_type_full, None) ret_type_full code_field >>=? fun (code, _) ->
  return {
    param_type = claimed_parameter_type;
    storage_type = claimed_storage_type;
    code ;
  }

let get_initial_program context toplevel_path execution_context : ex_program_state =
  Lwt_main.run @@ (
      get_toplevel_node () toplevel_path >>=? fun (param_type, storage_type, code_field) ->
    let (Ex_ty param_type, _) =
      force_ok ~msg:"parse arg ty" @@
      Script_ir_translator.parse_ty context ~allow_big_map:false ~allow_operation:false param_type in
    let (Ex_ty storage_type, _) =
      force_ok ~msg:"parse storage ty" @@
      parse_ty context ~allow_big_map:false ~allow_operation:false storage_type in
    let param_type_full = Pair_t ((param_type, None, None),
                                  (storage_type, None, None), None) in
    let ret_type_full =
      Pair_t ((List_t (Operation_t None, None), None, None),
              (storage_type, None, None), None) in
    parse_returning (Toplevel { storage_type = storage_type ; param_type = param_type })
      context (param_type_full, None) ret_type_full code_field >>=? 
    fun (Lam (desc, _), _) ->
    parse_data_simplified context param_type execution_context.parameter
    >>=? fun (param_val) ->
    parse_data_simplified context storage_type execution_context.storage 
    >>=? fun (storage_val) ->
    let stack = Item ((param_val, storage_val), Empty) in
    let stack_ty = Item_t (param_type_full, Empty_t, None) in
    return @@ Program.Ex_program_state ([Ex_descr desc ], stack, stack_ty)
  ) |> Misc.force_ok ~msg:"Program could not be parsed"

let get_initial_typed_program :
  type param storage.
  Context.t -> string -> (param, storage) Execution_context.typed_t ->
  param ty -> storage ty -> (param, storage) typed_program =
  fun context toplevel_path execution_context claimed_param_type claimed_storage_type ->
  Lwt_main.run @@ (
      get_toplevel_node () toplevel_path >>=? fun (param_type, storage_type, code_field) ->
    let (Ex_ty param_type, _) =
      force_ok ~msg:"parse arg ty" @@
      Script_ir_translator.parse_ty context ~allow_big_map:false ~allow_operation:false param_type in
    let _ = Misc.force_ok ~msg:"actual param type not equal to claimed param type" 
      @@ Script_ir_translator.ty_eq context param_type claimed_param_type in
    let (Ex_ty storage_type, _) =
      force_ok ~msg:"parse storage ty" @@
      parse_ty context ~allow_big_map:false ~allow_operation:false storage_type in
    let _ = Misc.force_ok ~msg:"actual storage type not equal to claimed storage type" 
    @@ Script_ir_translator.ty_eq context storage_type claimed_storage_type in
    let param_type_full = Pair_t ((claimed_param_type, None, None),
                                  (claimed_storage_type, None, None), None) in
    let ret_type_full =
      Pair_t ((List_t (Operation_t None, None), None, None),
              (claimed_storage_type, None, None), None) in
    parse_returning (Toplevel { storage_type = claimed_storage_type ; param_type = claimed_param_type })
      context (param_type_full, None) ret_type_full code_field >>=? 
    fun (code, _) ->
    let stack = Item ((execution_context.parameter, execution_context.storage), Empty) in
    let stack_ty = Item_t (param_type_full, Empty_t, None) in
    return {
      code = code;
      stack = stack;
      stack_ty = stack_ty
    }
  ) |> Misc.force_ok ~msg:"Program could not be parsed"