open Cast

let print_stack ty stack : unit = 
  (* Todo: formatter version *)
  print_endline @@ Cast.stack_to_string ty stack

let print_ex_stack ex_typed_stack : unit = 
  let Ex_typed_stack (ty, stack) = ex_typed_stack in
  print_endline @@ Cast.stack_to_string ty stack

let print_instructions descr : unit = 
  print_endline @@ Cast.descr_to_string descr