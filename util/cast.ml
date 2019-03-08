open Tezos_error_monad
open Tezos_micheline
exception Expr_from_string

let expr_of_string str =
  let (ast, errs) = Michelson_parser.parse_expression ~check:false str in
  (match errs with
   | [] -> ()
   | lst -> (
       Format.printf "expr_from_string: %a\n" Error_monad.pp_print_error lst;
       raise Expr_from_string
     ));
  ast.expanded

let tl_of_string str =
  let (ast, errs) = Michelson_parser.parse_toplevel ~check:false str in
  (match errs with
   | [] -> ()
   | lst -> (
       Format.printf "expr_from_string: %a\n" Error_monad.pp_print_error lst;
       raise Expr_from_string
     ));
  ast.expanded

  let node_of_string str =
    Micheline.root @@ expr_of_string str

  let node_to_string (node:_ Micheline.node) =
    let stripped = Micheline.strip_locations node in
    let print_node = Micheline_printer.printable Michelson_v1_primitives.string_of_prim stripped in
    Micheline_printer.print_expr Format.str_formatter print_node ;
    Format.flush_str_formatter ()