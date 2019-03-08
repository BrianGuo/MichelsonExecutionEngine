open Tezos_micheline.Micheline_parser
open Tezos_micheline.Micheline

type parsed =
  { source : string ;
    unexpanded : string canonical ;
    expanded : Michelson_v1_primitives.prim canonical ;
    expansion_table : (int * (location * int list)) list ;
    unexpansion_table : (int * int) list }


let expand_all source ast errors =
  let unexpanded, loc_table =
    extract_locations ast in
  let expanded, expansion_errors =
    Michelson_v1_macros.expand_rec (root unexpanded) in
  let expanded, unexpansion_table =
    extract_locations expanded in
  let expansion_table =
    let sorted =
      List.sort (fun (_, a) (_, b) -> compare a b) unexpansion_table in
    let grouped =
      let rec group = function
        | acc, [] -> acc
        | [], (u, e) :: r ->
            group ([ (e, [ u ]) ], r)
        | ((pe, us) :: racc as acc), (u, e) :: r ->
            if e = pe then
              group (((e, u :: us) :: racc), r)
            else
              group (((e, [ u ]) :: acc), r) in
      group ([], sorted) in
    List.map2
      (fun (l, ploc) (l', elocs) ->
         assert (l = l') ;
         (l, (ploc, elocs)))
      (List.sort compare loc_table)
      (List.sort compare grouped) in
  match (Michelson_v1_primitives.prims_of_strings expanded) with
  | Ok expanded ->
      { source ; unexpanded ; expanded ;
        expansion_table ; unexpansion_table },
      errors @ expansion_errors
  | Error errs ->
      { source ; unexpanded ;
        expanded = strip_locations (Seq ((), [])) ;
        expansion_table ; unexpansion_table },
      errors @ expansion_errors @ errs

let parse_expression ?check source =
  let tokens, lexing_errors = tokenize source in
  let ast, parsing_errors = parse_expression ?check tokens in
  expand_all source ast (lexing_errors @ parsing_errors)

let parse_toplevel ?check source =
  let tokens, lexing_errors = tokenize source in
  let asts, parsing_errors = parse_toplevel ?check tokens in
  let ast =
    let start = min_point asts and stop = max_point asts in
    Seq ({ start ; stop }, asts) in
  expand_all source ast (lexing_errors @ parsing_errors)