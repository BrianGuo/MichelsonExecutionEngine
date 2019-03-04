open Lib
open Tezos_error_monad
open Error_monad
open Tezos_micheline
open Context_type

let print_test expr =
  let print_node = Micheline_printer.printable Michelson_v1_primitives.string_of_prim expr in
  Micheline_printer.print_expr Format.str_formatter print_node ;
  Format.flush_str_formatter ()

let x =
  let t = Fileparser.get_toplevel
    ~environment:{
      tezos_context = Context_type.default_context;
      identities = [];
    }
    "helloworld.tz"
    (String_t (Some (`Type_annot "")))
    (Unit_t (None))
  in
  Lwt_main.run (
    Context.init 10 >>=? fun (_, contracts, _) ->
    let context = 
      {Context_type.default_context with storage_map = (Storage.init_contracts_storage contracts) } in
    match t.code with
  | Lam (_, expr) ->
      print_endline (print_test expr);
      Script_interpreter.execute context Readable
      ~source:(List.nth contracts 0)
      ~payer:(List.nth contracts 1)
      ~arg_type:t.param_type
       ~self:(List.nth contracts 2, t.code)
       ~amount:Tez.one
       ~parameter:(Cast.expr_of_string "Unit")
       ~storage:""
       ~storage_ty:t.storage_type
       >>=? fun (result) ->
        List.iter (fun contract -> 
          print_endline @@ Int64.to_string @@ (snd contract).balance;
          match fst contract with
          | Implicit x -> print_endline @@ Signature.Public_key_hash.to_string x;
          | _ -> ()) 
        (Context_type.Storage_map_mod.bindings result.ctxt.storage_map);
        print_endline (print_test result.storage);
        Lwt.return @@ ok ("hello"))

let n =
  Misc.force_ok x

