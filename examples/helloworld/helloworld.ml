open Lib
open Tezos_error_monad.Error_monad
open Fileparser
open Context_type
open Execution_context
open Types
open OUnit

let () = Error_registration.register();

module Environment = struct
  let param_type = unit_t
  let storage_type = string_t
end

let helloWorldFile = "./helloworld.tz"

let execution_context = 
  {source = 0; payer = 0; self = 0; amount = Tez.zero; parameter = (); storage = ""}

let context = Context.default_context |> Context.init_contracts 10 

let execute_directly_with_interpreter =
  let file_object = Fileparser.get_toplevel_object
    helloWorldFile
    Environment.storage_type
    Environment.param_type
  in
  Lwt_main.run (
    Misc.init 10 >>=? fun (_, contracts, _) ->
    match file_object.code with
  | Lam (_, _) ->
      Script_interpreter.execute context Readable
      ~source:(List.nth contracts 0)
      ~payer:(List.nth contracts 1)
      ~arg_type:file_object.param_type
       ~self:(List.nth contracts 2, file_object.code)
       ~amount:Tez.one
       ~parameter:(Cast.expr_of_string "123")
       ~storage:"1231231"
       ~storage_ty:file_object.storage_type
       >>=? fun (result) ->
        List.iter (fun contract -> 
          print_endline @@ Int64.to_string @@ (snd contract).balance;
          match fst contract with
          | Implicit x -> print_endline @@ Signature.Public_key_hash.to_string x;
          | _ -> ()) 
        (Context_type.Storage_map_mod.bindings result.ctxt.storage_map);
        Printer.print_node result.storage;
        Lwt.return @@ ok ("hello"))

let execute_directly_using_engine = 
    Engine.get_typed_toplevel_and_execute 
      context 
      helloWorldFile 
      execution_context 
      ~param_type:Environment.param_type 
      ~storage_type:Environment.storage_type

let print_results_of_engine_execution _ = 
  execute_directly_using_engine |> fun (result) -> 
    (* Storage in helloworld is a string *)
    let ((operations, storage), context) = result in
      print_endline storage;
      print_endline @@ string_of_int @@ List.length operations;
      print_endline @@ Int64.to_string @@ (snd (List.hd @@ Context.get_contracts_and_storage context)).balance

let get_program_state_then_execute = 
  let execution_context = {default_execution_context with 
      parameter = Data_factory.create_data_node_from_ty Environment.param_type ();
      storage = Data_factory.create_data_node_from_ty Environment.storage_type "";
  } in
  let program = Fileparser.get_initial_program context helloWorldFile execution_context in
  let rec step_while program ctxt =
      let Program.Ex_program_state (lst, stack, ty) = program in
      match lst with
      | [] -> Program.Ex_typed_stack (ty, stack)
      | _ -> let (new_program, new_context) = Engine.step ctxt execution_context program in
          step_while new_program new_context
      in
  step_while program context

let print_results_of_full_stepping _ = 
  get_program_state_then_execute |> fun (result) -> 
    (* Storage in helloworld is a string *)
    let ((Program.Ex_typed_stack (ty, stack))) = result in
    print_endline @@ Cast.stack_to_string ty stack;
    print_endline @@ Int64.to_string @@ (snd (List.hd @@ Context.get_contracts_and_storage context)).balance

let get_program_state_and_step_n n = 
  let execution_context = {default_execution_context with 
      parameter = Data_factory.create_data_node_from_ty Environment.param_type ();
      storage = Data_factory.create_data_node_from_ty Environment.storage_type "";
  } in
  let program = Fileparser.get_initial_program context helloWorldFile execution_context in
  Engine.step_n ~n:n context execution_context program

let print_results_of_partial_stepping _ = 
  get_program_state_and_step_n 0 |> fun (result) -> 
    (* Storage in helloworld is a string *)
    let ((Program.Ex_program_state (_, stack, ty), context)) = result in
    print_endline @@ Cast.stack_to_string ty stack;
    print_endline @@ Int64.to_string @@ (snd (List.hd @@ Context.get_contracts_and_storage context)).balance

let test_execution_produces_correct_list_and_storage _ =
  execute_directly_using_engine |> fun ((operations_list, storage), _) ->
    assert_equal (List.length operations_list) 0;
    assert_equal storage "Hello Tezos!";
    ()

let suite = 

let _ = run_test_tt_main @@ (
  "helloworld_test_suite" >:::
  [
    "execution_test" >:: test_execution_produces_correct_list_and_storage
  ]
) in ()

(*
  let () = print_results_of_partial_stepping ()
  let () = print_results_of_engine_execution ()
  let () = print_results_of_full_stepping ()
*)