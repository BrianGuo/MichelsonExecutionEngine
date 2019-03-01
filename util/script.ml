open Tezos_micheline
open Tezos_error_monad
open Error_monad
open Tezos_data_encoding

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

let serialized_cost _ = 0
let deserialized_cost _ = 0
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
