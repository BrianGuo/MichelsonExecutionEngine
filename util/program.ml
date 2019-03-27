open Script_ir_nodes
open Script_interpreter 
open Script_ir_translator

type 'tys stack = 'tys Script_interpreter.stack

type 'ty stack_ty = 'ty Script_ir_nodes.stack_ty

type ('bef, 'aft) descr = ('bef, 'aft) Script_ir_nodes.descr
type ('bef, 'aft) lambda = ('bef, 'aft) Script_ir_nodes.lambda
type ex_descr = Ex_descr : ('b, 'a) descr -> ex_descr
type ex_lambda = Ex_lambda : ('b, 'a) lambda -> ex_lambda
type ex_typed_stack = Cast.ex_typed_stack = Ex_typed_stack : ('a stack_ty * 'a stack) -> ex_typed_stack
type ('param, 'storage) typed_program = {
  code : (('param, 'storage) pair, (packed_internal_operation list, 'storage) pair ) lambda;
  stack : (('param * 'storage) * end_of_stack) stack;
  stack_ty : (('param, 'storage) pair * end_of_stack) stack_ty;
}
type ex_program_state = Ex_program_state : (ex_descr list * 'a stack * 'a stack_ty) -> ex_program_state

module Stack = struct

  let rec stack_eq (st1 : ex_typed_stack) (st2 : ex_typed_stack)  = 
    match st1, st2 with
    | Cast.Ex_typed_stack (Item_t (t1, rest_t1, _), Item (v1, rest1)), Cast.Ex_typed_stack (Item_t (t2, rest_t2, _), Item (v2, rest2)) ->
        (Script_ir_translator.ty_eq Context.default_context t1 t2) |>
        Misc.force_ok ~msg:"stack types equal" |> 
        fun (Eq, _) -> 
          if v1 <> v2 then false else
          stack_eq (Cast.Ex_typed_stack (rest_t1, rest1)) (Cast.Ex_typed_stack (rest_t2, rest2))
    | Cast.Ex_typed_stack (_, Empty), Cast.Ex_typed_stack (_, Empty) -> true
    | _, _ -> false

end