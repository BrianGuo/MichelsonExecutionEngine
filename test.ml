open Lib
open Tezos_error_monad
open Error_monad
open Tezos_micheline

let print_test expr =
  let print_node = Micheline_printer.printable Michelson_v1_primitives.string_of_prim expr in
  Micheline_printer.print_expr Format.str_formatter print_node ;
  Format.flush_str_formatter ()

let x =
  let t = Fileparser.get_toplevel
    ~environment:{tezos_context = B ([]); identities = []}
    "helloworld.tz"
    (String_t (Some (`Type_annot "")))
    (Unit_t (None))
  in
  Lwt_main.run (
    match t.code with
  | Lam (_, expr) ->
      print_endline (print_test expr);
      Script_interpreter.execute (B ([])) Readable
      ~source:0
      ~payer:0
      ~arg_type:t.param_type
       ~self:(0, t.code)
       ~amount:Tez.one
       ~parameter:(Cast.expr_of_string "Unit")
       ~storage:""
       ~storage_ty:t.storage_type
       >>=? fun (result) ->
        print_endline (print_test result.storage);
        Lwt.return @@ ok ("hello"))

let n =
  Misc.force_ok x

