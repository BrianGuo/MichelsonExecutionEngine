open Execution_context
open Context
open Tezos_error_monad.Error_monad
open Script_interpreter
open Script_ir_translator
open Script_ir_nodes
open Misc
open Data_factory
open Tezos_micheline
open Program

let rec typed_step_instr :
  type bef aft.
  ?execution_context :Execution_context.t -> ?context: Context.t -> 
  bef stack -> bef stack_ty -> (bef, aft) descr ->
  ((Program.ex_program_state * Context.t) tzresult Lwt.t) =
  fun ?(execution_context=default_execution_context) ?(context=(Context.init_contracts 10 default_context)) 
  stack stack_ty code ->
    let contracts = get_contracts_and_storage context in
    let source =  fst @@ List.nth contracts execution_context.source in
    let payer = fst @@ List.nth contracts execution_context.payer in
    let self = fst @@ List.nth contracts execution_context.self in
    let amount = execution_context.amount in
    let visitor = fun _ -> () (* TODO: allow for visitors to print *) in
    Misc.force_ok ~msg:"code does not match this stack type" @@ 
    stack_ty_eq context 0 (code.bef) (stack_ty) |> fun (Eq, _) ->
    begin 
    match code.instr, stack with
    | Seq (h, tl), _ -> 
        typed_step_instr ~execution_context ~context stack stack_ty h
        >>=? fun ((Ex_program_state (code_list, stack, stack_ty)), ctxt) -> 
          return @@ (Ex_program_state (code_list @ [Ex_descr tl], stack, stack_ty), ctxt)
    | If (bt, _), Item (true, rest) -> 
        return @@ (Ex_program_state ([(Ex_descr bt)], rest, bt.bef), context)
    | If (_, bf), Item(false, rest) ->
        return @@ (Ex_program_state ([(Ex_descr bf)], rest, bf.bef), context)
    | If_none (bt, _), Item (None, rest) -> 
        return @@ (Ex_program_state ([Ex_descr bt], rest, bt.bef), context)
    | If_none (_, bf), Item (Some v, rest) -> 
        return @@ (Ex_program_state ([(Ex_descr bf)], Item (v, rest), bf.bef), context)
    | If_left (bt, _), Item (L v, rest) ->
        return @@ (Ex_program_state ([(Ex_descr bt)], Item (v, rest), bt.bef), context)
    | If_left (_, bf), Item (R v, rest) ->
        return @@ (Ex_program_state ([(Ex_descr bf)], Item (v, rest), bf.bef), context)
    | If_cons (_, bf), Item ([], rest) ->
        return @@ (Ex_program_state ([(Ex_descr bf)], rest, bf.bef), context)
    | If_cons (bt, _), Item (hd :: tl, rest ) ->
        return @@ (Ex_program_state ([(Ex_descr bt)], Item(hd, Item (tl, rest)), bt.bef), context)
    | Loop (body), Item (true, rest) ->
        return @@ (Ex_program_state ([(Ex_descr body); (Ex_descr code)], rest, body.bef), context)
    | Loop_left (body), Item (L v, rest) -> 
        return @@ (Ex_program_state ([(Ex_descr body); (Ex_descr code)], Item (v, rest), body.bef), context)
    | _ -> 
      Script_interpreter.step context ~source ~payer ~self ~visitor amount code stack >>=? fun (stack, context) ->
        return @@ (Program.Ex_program_state ([], stack, code.aft), context)
    end

  

let execute_lambda_with_execution_context ctxt mode code (execution_context : Execution_context.t)  ~arg_type ~storage_type = 
  parse_data_simplified ctxt storage_type execution_context.storage >>=? fun (storage) ->
  let contracts = List.map (fun f -> fst f) (Context_type.Storage_map_mod.bindings ctxt.storage_map) in
  execute 
      ctxt 
      mode 
      ~source:(List.nth contracts execution_context.source)
      ~payer:(List.nth contracts execution_context.payer)
      ~arg_type:arg_type
      ~self:((List.nth contracts execution_context.self), code)
      ~amount:(execution_context.amount)
      ~parameter:(Micheline.strip_locations execution_context.parameter)
      ~storage:storage
      ~storage_ty:storage_type

let execute_with_execution_context ctxt code (execution_context : Execution_context.t)  ~arg_type ~storage_type = 
  parse_data_simplified ctxt storage_type execution_context.storage >>=? fun (storage) ->
  parse_data_simplified ctxt arg_type execution_context.parameter >>=? fun (parameter) ->
  let contracts = List.map (fun f -> fst f) (Context_type.Storage_map_mod.bindings ctxt.storage_map) in
  let stack = Item ((parameter, storage), Empty) in
  step 
      ctxt 
      ~source:(List.nth contracts execution_context.source)
      ~payer:(List.nth contracts execution_context.payer)
      ~self:((List.nth contracts execution_context.self))
      (execution_context.amount)
      code
      stack

let execute_with_typed_execution_context (type a) (type b) ctxt code (execution_context : (a, b) Execution_context.typed_t) =
  let contracts = List.map (fun f -> fst f) (Context_type.Storage_map_mod.bindings ctxt.storage_map) in
  interp 
      ctxt 
      ~source:(List.nth contracts execution_context.source)
      ~payer:(List.nth contracts execution_context.payer)
      ~self:(List.nth contracts execution_context.self)
      (execution_context.amount)
      code
      (execution_context.parameter, execution_context.storage)

let get_toplevel_and_execute context toplevel_path execution_context = 
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
    execute_lambda_with_execution_context 
      context 
      Readable 
      code 
      execution_context 
      ~arg_type:param_type 
      ~storage_type:storage_type
  ) |> Misc.force_ok ~msg:"Execution Failed"

let get_typed_toplevel_and_execute :
 type param storage.
 Context.t -> string -> (param, storage) Execution_context.typed_t -> 
 param_type:param ty -> storage_type:storage ty -> 
 ((packed_internal_operation list * storage) * Context.t) =
 fun context toplevel_path execution_context ~param_type ~storage_type ->
    Lwt_main.run @@ (
      let program = Fileparser.get_initial_typed_program 
        context toplevel_path execution_context param_type storage_type in
    execute_with_typed_execution_context 
      context 
      program.code 
      execution_context 
  ) |> Misc.force_ok ~msg:"Execution Failed"

let step_typed :
  type bef aft.
  Context.t -> Execution_context.t -> (bef, aft) Program.typed_program -> (Program.ex_program_state * Context.t) = 
  fun context execution_context program -> 
  Lwt_main.run @@ (
    let Lam (descr, _) = program.code in 
    typed_step_instr 
    ~execution_context ~context 
    program.stack
    program.stack_ty
    descr
  ) |> Misc.force_ok ~msg:"Step Execution Failed"

let step context execution_context ex_program_state : (Program.ex_program_state * Context.t) =
  Lwt_main.run @@ (
    let Ex_program_state (desc_list, stack, ty) = ex_program_state in 
    match desc_list with
    | [] -> raise (Failure "No code was provided")
    | h::t ->
      let Ex_descr code = h in
      Misc.force_ok ~msg:"code does not match this stack type" @@ 
      stack_ty_eq context 0 (code.bef) (ty) |> fun (Eq, _) ->
      typed_step_instr ~execution_context ~context stack ty code >>=? 
      fun (program, context) ->
        let Ex_program_state (lst, stack, ty) = program in
        return @@ (Ex_program_state (lst @ t, stack, ty), context)
  ) |> Misc.force_ok ~msg:"Step Execution Failed"

let rec step_n :
  n:int -> Context.t -> Execution_context.t -> Program.ex_program_state ->
  (Program.ex_program_state * Context.t) =
  fun ~n context execution_context program ->
    if n = 0 then
      (program, context)
    else
      let (prog, ctxt) = step context execution_context program in
      step_n ~n:(n-1) ctxt execution_context prog 



