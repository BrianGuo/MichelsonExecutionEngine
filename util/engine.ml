open Execution_context
open Context
open Tezos_error_monad.Error_monad
open Script_interpreter
open Script_ir_translator
open Script_ir_nodes
open Misc
open Program


let rec step_instr
  ?(execution_context=default_execution_context) 
  ?(context=(Context.init_contracts 10 default_context)) 
  (stack : Cast.ex_typed_stack)
  (code_list : Program.ex_descr list) 
  : (Program.ex_program_state tzresult Lwt.t) =
    let contracts = get_contracts_and_storage context in
    let source =  fst @@ List.nth contracts execution_context.source in
    let payer = fst @@ List.nth contracts execution_context.payer in
    let self = fst @@ List.nth contracts execution_context.self in
    let amount = execution_context.amount in
    let visitor = fun _ -> () (* TODO: allow for visitors to print *) in
    let Cast.Ex_typed_stack (stack_ty, stack) = stack in
    begin
    match code_list with
    (* code list is a list of snippets of code *)
    (* todo: if statements should be included? *)
    | [] -> raise (Failure "no code was provided, the program may have finished executing")
    | (Ex_descr h):: t ->
      Misc.force_ok ~msg:"code does not match this stack type" @@ 
      stack_ty_eq context 0 (h.bef) (stack_ty) |> fun (Eq, _) ->
      begin 
      match h.instr, stack with
      | Seq (h, tl), _ -> 
          step_instr ~execution_context ~context (Ex_typed_stack (stack_ty, stack)) [Ex_descr h]
          >>=? fun (Ex_program_state (code_list, stack, stack_ty)) -> 
            return @@ Ex_program_state (code_list @ [Ex_descr tl] @ t, stack, stack_ty)
      | Loop (body), Item (true, rest) -> 
          (* executing the "truth" part is the step *)
          return @@ Ex_program_state ((Ex_descr body)::t, rest, body.bef)
      | Loop_left (body), Item (L v, rest) -> 
          return @@ Ex_program_state ((Ex_descr body)::t, Item (v, rest), body.bef)
      | _ -> 
        Script_interpreter.step context ~source ~payer ~self ~visitor amount h stack >>=? fun (stack, _) ->
          return @@ Program.Ex_program_state (t, stack, h.aft)
      end
    end
  

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

let step context execution_context ex_program_state : Program.ex_program_state =
  Lwt_main.run @@ (
    let Ex_program_state (desc_list, stack, ty) = ex_program_state in 
    step_instr ~execution_context ~context (Ex_typed_stack (ty, stack)) desc_list >>=? 
    fun (stack) ->
      return stack
  ) |> Misc.force_ok ~msg:"Execution Failed"


