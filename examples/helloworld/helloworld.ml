open Lib
open Tezos_error_monad.Error_monad
open Fileparser
open Context_type
open Execution_context
open Types

let () = Error_registration.register();

module Environment = struct
  let param_type = unit_t
  let storage_type = string_t
end

let x =
  let t = Fileparser.get_toplevel_object
    "./helloworld.tz"
    Environment.storage_type
    Environment.param_type
  in
  Lwt_main.run (
    Misc.init 10 >>=? fun (_, contracts, _) ->
    let context = 
      {Context.default_context with 
        storage_map = (Storage.init_contracts_storage contracts);
        gas = Limited ({remaining = Z.of_int 100});
        block_gas = Z.of_int 100000 } in
    match t.code with
  | Lam (_, _) ->
      Script_interpreter.execute context Readable
      ~source:(List.nth contracts 0)
      ~payer:(List.nth contracts 1)
      ~arg_type:t.param_type
       ~self:(List.nth contracts 2, t.code)
       ~amount:Tez.one
       ~parameter:(Cast.expr_of_string "123")
       ~storage:"1231231"
       ~storage_ty:t.storage_type
       >>=? fun (result) ->
        List.iter (fun contract -> 
          print_endline @@ Int64.to_string @@ (snd contract).balance;
          match fst contract with
          | Implicit x -> print_endline @@ Signature.Public_key_hash.to_string x;
          | _ -> ()) 
        (Context_type.Storage_map_mod.bindings result.ctxt.storage_map);
        Printer.print_node result.storage;
        Lwt.return @@ ok ("hello"))

let result  = 
  let execution_context = 
    {source = 0; payer = 0; self = 0; amount = Tez.zero; parameter = (); storage = ""} in
  let context = Context.default_context |> Context.init_contracts 10 in 
    Engine.get_typed_toplevel_and_execute context "helloworld.tz" execution_context ~param_type:(Unit_t None) ~storage_type:(String_t None)
    (*|> fun (program) -> 
      Engine.step context execution_context program
    |> fun (program) ->
      Engine.step context execution_context program
    |> fun (program) ->
      Engine.step context execution_context program
    |> fun (program) -> 
      Engine.step context execution_context program
    |> fun (program) -> 
      Engine.step context execution_context program
    |> fun (program) -> 
      Engine.step context execution_context program *)
  
(* let () =
  let open Program in
  let Program.Ex_program_state (code, stack, ty) = result in
  let Ex_descr code = List.hd code in 
    print_endline @@ Cast.stack_to_string ty stack;
    print_endline @@ Cast.descr_to_string code *)

let () =
  print_endline @@ Cast.data_to_string (String_t None) @@ snd @@ fst result
  

