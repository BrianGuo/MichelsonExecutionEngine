open Tezos_error_monad

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