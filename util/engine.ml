open Execution_context
open Context
open Tezos_error_monad.Error_monad
open Script_interpreter
open Script_ir_translator
open Script_ir_nodes
open Misc

let step_lwt
  ?(execution_context=default_execution_context) 
  ?(context=(Context.init_contracts 10 default_context)) 
  (stack:'a Script_interpreter.stack)
  code
  : (string tzresult Lwt.t) =
    let contracts = get_contracts_and_storage context in
    let source =  fst @@ List.nth contracts execution_context.source in
    let payer = fst @@ List.nth contracts execution_context.payer in
    let self = fst @@ List.nth contracts execution_context.self in
    let amount = execution_context.amount in
    let visitor = fun _ -> () (* TODO: allow for visitors to print *) in
    match code.instr with
    | Seq (h, _) -> 
          Script_interpreter.step context ~source ~payer ~self ~visitor amount h stack >>=? fun (stack, _) ->
              return @@ Cast.stack_to_string h.aft stack
    | _ -> Script_interpreter.step context ~source ~payer ~self ~visitor amount code stack >>=? fun (stack, _) ->
        return @@ Cast.stack_to_string code.aft stack
    



(* let stack_eq st1 st2 = 
  match st1, st2 with
  | Item (t1, rest1), Item (t2, rest2) ->
      (t1 = t2) && stack_eq rest1 rest2
  | Empty, Empty -> true
  | _, _ -> false *)

let execute_with_execution_context ctxt mode code (execution_context : Execution_context.t)  ~arg_type ~storage_type = 
  parse_data ctxt storage_type @@ Cast.node_of_string execution_context.storage >>=?
  fun (storage, context) ->
      let contracts = List.map (fun f -> fst f) (Context_type.Storage_map_mod.bindings ctxt.storage_map) in
      execute 
          context 
          mode 
          ~source:(List.nth contracts execution_context.source)
          ~payer:(List.nth contracts execution_context.payer)
          ~arg_type:arg_type
          ~self:((List.nth contracts execution_context.self), code)
          ~amount:(execution_context.amount)
          ~parameter:(Cast.expr_of_string execution_context.parameter)
          ~storage:storage
          ~storage_ty:storage_type

let get_toplevel_and_execute context toplevel_path execution_context =
  try 
    Lwt_main.run @@ (
      Fileparser.get_toplevel_node () toplevel_path >>=? fun (param_type, storage_type, code_field) ->
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
      context (param_type_full, None) ret_type_full code_field >>=? fun (code, _) -> 
    execute_with_execution_context 
      context 
      Readable 
      code 
      execution_context 
      ~arg_type:param_type 
      ~storage_type:storage_type
  ) |> Misc.force_ok ~msg:"Execution Failed"
  with 
    | Failure f ->
      if f = "nth" then 
        print_endline ("Failed List.nth failure");
        print_endline "Maybe you forgot to initialize the storage?";
        raise (Failure f)

let get_toplevel_and_step context toplevel_path execution_context =
  Lwt_main.run @@ (
      Fileparser.get_toplevel_node () toplevel_path >>=? fun (param_type, storage_type, code_field) ->
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
    parse_data context param_type @@ Cast.node_of_string execution_context.parameter
    >>=? fun (param_val, _) ->
    parse_data context storage_type @@ Cast.node_of_string execution_context.storage 
    >>=? fun (storage_val, _) ->
    let stack = Item ((param_val, storage_val), Empty) in
    step_lwt ~execution_context ~context stack desc >>=? 
    fun (str) ->
      return str
  ) |> Misc.force_ok ~msg:"Execution Failed"


