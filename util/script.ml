open Tezos_micheline
open Tezos_error_monad
open Error_monad
open Tezos_data_encoding
open Tezos_stdlib

type error += Lazy_script_decode

type location = Micheline.canonical_location
let location_encoding = Micheline.canonical_location_encoding
type expr = Michelson_v1_primitives.prim Micheline.canonical
type node = (location, Michelson_v1_primitives.prim) Micheline.node
type annot = Micheline.annot
type lazy_expr = expr Data_encoding.lazy_t

type t = {
  code : lazy_expr ;
  storage : lazy_expr
}

type expr_hash = string
let force_decode c lexpr =
  let code =
    match Data_encoding.force_decode lexpr with
    | Some v -> ok (v, c)
    | None -> error Lazy_script_decode
  in
  Lwt.return code



let expr_hash_size = 0

let hash_bytes _ = ""

let expr_encoding =
  Micheline.canonical_encoding_v1
  ~variant:"michelson v1"
  Michelson_v1_primitives.prim_encoding

let lazy_expr_encoding =
  Data_encoding.lazy_encoding expr_encoding

let encoding =
  let open Data_encoding in
  def "scripted.contracts" @@
  conv
    (fun { code ; storage } -> (code, storage))
    (fun (code, storage) -> { code ; storage })
    (obj2
       (req "code" lazy_expr_encoding)
       (req "storage" lazy_expr_encoding))

let lazy_expr expr =
  Data_encoding.make_lazy expr_encoding expr

let cost_of_size (blocks, words) =
  let open Gas in
  ((Compare.Int.max 0 (blocks - 1)) *@ alloc_cost 0) +@
  alloc_cost words +@
  step_cost blocks



let int_node_size_of_numbits n =
  (1, 1 + (n + 63) / 64)
let int_node_size n =
  int_node_size_of_numbits (Z.numbits n)

let string_node_size_of_length s =
  (1, 1 + (s + 7) / 8)
let string_node_size s =
  string_node_size_of_length (String.length s)

let bytes_node_size_of_length s =
  (* approx cost of indirection to the C heap *)
  (2, 1 + (s + 7) / 8 + 12)
let bytes_node_size s =
  bytes_node_size_of_length (MBytes.length s)  
let prim_node_size_nonrec_of_lengths n_args annots =
  let annots_length = List.fold_left (fun acc s -> acc + String.length s) 0 annots in
  if Compare.Int.(annots_length = 0) then
    (1 + n_args, 2 + 2 * n_args)
  else
    (2 + n_args, 4 + 2 * n_args + (annots_length + 7) / 8)
let prim_node_size_nonrec args annots =
  let n_args = List.length args in
  prim_node_size_nonrec_of_lengths n_args annots
let seq_node_size_nonrec_of_length n_args =
  (1 + n_args, 2 + 2 * n_args)
let seq_node_size_nonrec args =
  let n_args = List.length args in
  seq_node_size_nonrec_of_length n_args
let int_node_cost n = cost_of_size (int_node_size n)
let int_node_cost_of_numbits n = cost_of_size (int_node_size_of_numbits n)
let string_node_cost s = cost_of_size (string_node_size s)
let string_node_cost_of_length s = cost_of_size (string_node_size_of_length s)
let bytes_node_cost s = cost_of_size (bytes_node_size s)
let bytes_node_cost_of_length s = cost_of_size (bytes_node_size_of_length s)
let prim_node_cost_nonrec args annot = cost_of_size (prim_node_size_nonrec args annot)

let rec node_size node =
  let open Micheline in
  match node with
  | Int (_, n) -> int_node_size n
  | String (_, s) -> string_node_size s
  | Bytes (_, s) -> bytes_node_size s
  | Prim (_, _, args, annot) ->
      List.fold_left
        (fun (blocks, words) node ->
            let (nblocks, nwords) = node_size node in
            (blocks + nblocks, words + nwords))
        (prim_node_size_nonrec args annot)
        args
  | Seq (_, args) ->
      List.fold_left
        (fun (blocks, words) node ->
            let (nblocks, nwords) = node_size node in
            (blocks + nblocks, words + nwords))
        (seq_node_size_nonrec args)
        args
        
  let expr_size expr =
    node_size (Micheline.root expr)
  
  let deserialized_cost expr =
    cost_of_size (expr_size expr)
  
  let serialized_cost bytes =
    let open Gas in
    alloc_mbytes_cost (MBytes.length bytes)

let prim_node_cost_nonrec_of_length n_args annot = cost_of_size (prim_node_size_nonrec_of_lengths n_args annot)
let seq_node_cost_nonrec args = cost_of_size (seq_node_size_nonrec args)
let seq_node_cost_nonrec_of_length n_args = cost_of_size (seq_node_size_nonrec_of_length n_args)
