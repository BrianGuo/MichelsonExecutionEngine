open Cast
open Tezos_micheline

let print_stack : 
  type stack_ty.
  stack_ty Script_ir_nodes.stack_ty -> stack_ty Script_interpreter.stack -> unit = 
  (* Todo: formatter version *)
  fun ty stack -> 
    print_endline @@ Cast.stack_to_string ty stack

let print_ex_stack :
  ex_typed_stack -> unit = 
  fun ex_typed_stack ->
    let Ex_typed_stack (ty, stack) = ex_typed_stack in
    print_endline @@ Cast.stack_to_string ty stack

let print_instructions_descr :
  ('bef, 'aft) Script_ir_nodes.descr -> unit = 
  fun descr ->
    print_endline @@ Cast.descr_to_string descr

let print_instructions_lambda : 
  ('bef, 'aft) Script_ir_nodes.lambda -> unit = 
  fun lambda ->
    let open Script_ir_nodes in
    let Lam (descr, _) = lambda in 
      print_instructions_descr descr

let print_node :
  Michelson_v1_primitives.prim Micheline.canonical -> unit = 
  fun expr ->
    let print_node = Micheline_printer.printable Michelson_v1_primitives.string_of_prim expr in
    Micheline_printer.print_expr Format.str_formatter print_node;
    print_endline @@ Format.flush_str_formatter ()