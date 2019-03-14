open Tezos_error_monad
open Tezos_micheline
open Script_ir_translator
open Tezos_error_monad.Error_monad
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
  
  let convert_string_value_to_type str = 
    let node = Micheline.root @@ expr_of_string str in
      Script_ir_translator.parse_ty Context.default_context ~allow_big_map:false ~allow_operation:false node
  
  let node_to_string (node:_ Micheline.node) =
    let stripped = Micheline.strip_locations node in
    let print_node = Micheline_printer.printable Michelson_v1_primitives.string_of_prim stripped in
    Micheline_printer.print_expr Format.str_formatter print_node ;
    Format.flush_str_formatter ()

let rec mapper (Ex_typed_value (ty, a)) =
  let open Script_ir_nodes in
  let open Micheline in
  match ty, a with
  | Big_map_t (kt, vt, Some (`Type_annot "toto")), map ->
    let kt = ty_of_comparable_ty kt in
    fold_left_s
      (fun l (k, v) ->
          match v with
          | None -> return l
          | Some v -> (
              let key = data_to_node (Ex_typed_value (kt, k)) in
              let value = data_to_node (Ex_typed_value (vt, v)) in
              return (Prim (-1, Michelson_v1_primitives.D_Elt, [ key ; value ], []) :: l))
      )
      []
      (map_fold (fun k v acc -> (k, v) :: acc) map.diff []) >>=? fun items ->
    return (Some (Micheline.Seq (-1, String (-1, "...") :: items)))
  | _ -> return None

and data_to_node (Ex_typed_value (ty, data)) =
  let tc = Context.default_context in
  let node_lwt = Script_ir_translator.unparse_data tc ~mapper Readable ty data in
  Misc.force_ok ~msg:"" @@ Lwt_main.run (node_lwt >>=? fun ((node, _)) ->
  return node) 

let data_to_string ty data =
  let node = data_to_node (Ex_typed_value (ty, data)) in
  node_to_string node

(* let flatten_seq_descr seq =
  let open Script_ir_nodes in
  let rec helper (seq) acc = 
    fun (seq, acc) ->
      match seq with
      | Seq (a, b) -> helper b a::acc
      | x -> List.rev x::acc
  in
  helper seq [] *)

open Script_ir_nodes
open Script_interpreter
type ex_typed_stack =
  Ex_typed_stack : ('a stack_ty * 'a stack) -> ex_typed_stack

let stack_to_string stack_ty stack =
  let rec aux acc fst (Ex_typed_stack(stack_ty,stack)) =
    match (stack_ty, stack) with
    | Item_t (hd_ty, tl_ty, _), Item (hd, tl) -> (
        let separator = if not fst then " ; " else "" in
        let str = data_to_string hd_ty hd in
        let acc = acc ^ separator ^ str in
        let new_value = aux acc false (Ex_typed_stack (tl_ty, tl)) in
        new_value
      )
    | _ -> acc in
  aux "" true @@ Ex_typed_stack(stack_ty, stack)