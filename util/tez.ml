open Tezos_data_encoding

include Qty.Make (struct let id = "tez" end)

type t = qty
type tez = qty

let encoding =
  Data_encoding.def "mutez" @@
  encoding
