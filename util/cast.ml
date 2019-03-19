open Tezos_error_monad
open Tezos_micheline
open Script_ir_translator
open Tezos_error_monad.Error_monad
exception Expr_from_string

type ex_descr =
  Ex_descr : (_, _) Script_ir_nodes.descr -> ex_descr

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

  let descr_to_node x =
    let open Micheline in
    let open Script_ir_nodes in
    let rec f : ex_descr -> Script.node = fun descr ->
      let prim ?children ?children_nodes p =
        match (children, children_nodes) with
        | Some children, None ->
          Prim (0, p, List.map f children, [])
        | Some _, Some _ ->
          raise @@ Failure "descr_to_node: too many parameters"
        | None, Some children_nodes ->
          Prim (0, p, children_nodes, [])
        | None, None ->
          Prim (0, p, [], [])
      in
      let (Ex_descr descr) = descr in
      match descr.instr with
      | Dup -> prim I_DUP
      | Drop -> prim I_DROP
      | Swap -> prim I_SWAP
      | Dip c -> prim ~children:[Ex_descr c] I_DIP
      | Car -> prim I_CAR
      | Cdr -> prim I_CDR
      | Cons_pair -> prim I_PAIR
      | Nop -> prim I_NOP
      | Seq (a, b) -> Micheline.Seq (0, List.map f [Ex_descr a ; Ex_descr b])
      | Const v -> (
          let (Item_t (ty, _, _)) = descr.aft in
          prim  ~children_nodes:[data_to_node (Ex_typed_value (ty, v))] I_PUSH
        )
      | Failwith _ -> prim I_FAILWITH
      | If (a, b) -> prim ~children:[Ex_descr a ; Ex_descr b] I_IF
      | Loop c -> prim ~children:[Ex_descr c] I_LOOP
      | If_left (a, b) -> prim ~children:[Ex_descr a ; Ex_descr b] I_IF_LEFT
      | Left -> prim I_LEFT
      | Right -> prim I_RIGHT
      | Loop_left c -> prim ~children:[Ex_descr c] I_LOOP_LEFT
      | If_none (a, b) -> prim ~children:[Ex_descr a ; Ex_descr b] I_IF_NONE
      | Cons_none _ -> prim I_NONE
      | Cons_some -> prim I_SOME
      | Nil -> prim I_NIL
      | Cons_list -> prim I_CONS
      | If_cons (a, b) -> prim ~children:[Ex_descr a ; Ex_descr b] I_IF_CONS
      | List_iter _ -> prim I_ITER
      | List_map _ -> prim I_MAP
      | List_size -> prim I_SIZE 
      | Empty_set _ -> prim I_EMPTY_SET
      | Empty_map _ -> prim I_EMPTY_MAP
      | Set_mem -> prim I_MEM
      | Set_iter _ -> prim I_ITER
      | Set_update -> prim I_UPDATE
      | Set_size -> prim I_SIZE
      | Map_iter _ -> prim I_ITER
      | Map_mem -> prim I_MEM
      | Map_map _ -> prim I_MAP
      | Map_size -> prim I_SIZE
      | Big_map_mem -> prim I_MEM
      | Concat_string -> prim I_CONCAT
      | Concat_string_pair -> prim I_CONCAT
      | Concat_bytes -> prim I_CONCAT
      | Concat_bytes_pair -> prim I_CONCAT
      | Slice_string -> prim I_SLICE
      | Slice_bytes -> prim I_SLICE
      | String_size -> prim I_SIZE
      | Bytes_size -> prim I_SIZE
      | Add_seconds_to_timestamp -> prim I_ADD
      | Add_timestamp_to_seconds -> prim I_ADD
      | Sub_timestamp_seconds -> prim I_SUB
      | Diff_timestamps -> prim I_SUB
      | Compare _ -> prim I_COMPARE
      | Int_nat -> prim I_INT
      | Add_natnat -> prim I_ADD
      | Add_natint -> prim I_ADD
      | Add_intnat -> prim I_ADD
      | Add_intint -> prim I_ADD
      | Add_tez -> prim I_ADD
      | Sub_tez -> prim I_SUB
      | Mul_teznat -> prim I_MUL
      | Mul_nattez -> prim I_MUL
      | Ediv_teznat -> prim I_EDIV
      | Ediv_tez -> prim I_EDIV
      | Sub_int -> prim I_SUB
      | Mul_intint -> prim I_MUL
      | Mul_intnat -> prim I_MUL
      | Mul_natint -> prim I_MUL
      | Mul_natnat -> prim I_MUL
      | Ediv_intint -> prim I_EDIV
      | Ediv_intnat -> prim I_EDIV
      | Ediv_natint -> prim I_EDIV
      | Ediv_natnat -> prim I_MUL
      | Map_get -> prim I_GET
      | Map_update -> prim I_UPDATE
      | Big_map_get -> prim I_GET
      | Big_map_update -> prim I_UPDATE
      | Gt -> prim I_GT
      | Ge -> prim I_GE
      | Lt -> prim I_LT
      | Le -> prim I_LE
      | Eq -> prim I_EQ
      | Neq -> prim I_NEQ
      | Pack _ -> prim I_PACK
      | Unpack _ -> prim I_UNPACK
      | Blake2b -> prim I_BLAKE2B
      | Or -> prim I_OR
      | And -> prim I_AND
      | Xor -> prim I_XOR
      | Not -> prim I_NOT
      | Is_nat -> prim I_EQ
      | Neg_nat -> prim I_NEG
      | Neg_int -> prim I_NEG
      | Abs_int -> prim I_ABS
      | Hash_key -> prim I_HASH_KEY
      | Address -> prim I_ADDRESS
      | Contract _ -> prim I_CONTRACT
      | Create_account -> prim I_CREATE_ACCOUNT
      | Create_contract _ -> prim I_CREATE_CONTRACT
      | Set_delegate -> prim I_SET_DELEGATE
      | Now -> prim I_NOW
      | Balance -> prim I_BALANCE
      | Check_signature -> prim I_CHECK_SIGNATURE
      | Transfer_tokens -> prim I_TRANSFER_TOKENS
      | Implicit_account -> prim I_IMPLICIT_ACCOUNT
      | Sha256 -> prim I_SHA256
      | Sha512 -> prim I_SHA512
      | Steps_to_quota -> prim I_STEPS_TO_QUOTA
      | Source -> prim I_SOURCE
      | Sender -> prim I_SENDER 
      | Self _ -> prim I_SELF
      | Amount -> prim I_AMOUNT 
      | _ -> raise @@ Failure "descr to node" in
    f @@ Ex_descr x
  
  let rec flatten_node =
    let open Micheline in
    function
    | Seq (a, lst) -> (
        let aux = function
          | Prim (loc, p, children, annot) -> [ Prim (loc, p, List.map flatten_node children, annot) ]
          | Seq (_, lst) -> List.map flatten_node lst
          | x -> [ x ] in
        let seqs = List.map aux @@ List.map flatten_node lst in
        Seq (a, List.concat seqs) )
    | x -> x
  
  let descr_to_string descr =
    let node = descr_to_node descr in
    let node = flatten_node node in
    node_to_string node
  
open Script_ir_nodes
open Script_interpreter
type ex_typed_stack = (* todo: move this out of cast; why is this in here? *)
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