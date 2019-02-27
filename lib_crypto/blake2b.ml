open Tezos_error_monad.Error_monad
open Tezos_data_encoding

module type Name = sig
  val name : string
  val title : string
  val size : int option
end

module type PrefixedName = sig
  include Name
  val b58check_prefix : string
end

module Make_minimal (K : Name) = struct

  open Blake2
  type t = Blake2b.hash

  include K

  let size =
    match K.size with
    | None -> 32
    | Some x -> x

  let of_string_opt s =
    if String.length s <> size then
      None
    else
      Some (Blake2b.Hash (MBytes.of_string s))
  let of_string s =
    match of_string_opt s with
    | None ->
        generic_error "%s.of_string: wrong string size (%d)"
          K.name (String.length s)
    | Some h -> Ok h
  let of_string_exn s =
    match of_string_opt s with
    | None ->
        Format.kasprintf invalid_arg
          "%s.of_string: wrong string size (%d)"
          K.name (String.length s)
    | Some h -> h
  let to_string (Blake2b.Hash h) = MBytes.to_string h

  let of_hex s = of_string (Hex.to_string s)
  let of_hex_opt s = of_string_opt (Hex.to_string s)
  let of_hex_exn s = of_string_exn (Hex.to_string s)
  let to_hex s = Hex.of_string (to_string s)

  let pp ppf h =
    let `Hex h = to_hex h in
    Format.pp_print_string ppf h
  let pp_short ppf h =
    let `Hex h = to_hex h in
    Format.pp_print_string ppf (String.sub h 0 8)

  let of_bytes_opt b =
    if MBytes.length b <> size then
      None
    else
      Some (Blake2b.Hash b)
  let of_bytes_exn b =
    match of_bytes_opt b with
    | None ->
        let msg =
          Printf.sprintf "%s.of_bytes: wrong string size (%d)"
            K.name (MBytes.length b) in
        raise (Invalid_argument msg)
    | Some h -> h
  let of_bytes s =
    match of_bytes_opt s with
    | Some x -> Ok x
    | None ->
        generic_error "Failed to deserialize a hash (%s)" K.name
  let to_bytes (Blake2b.Hash h) = h

  (* let read src off = of_bytes_exn @@ MBytes.sub src off size *)
  (* let write dst off h = MBytes.blit (to_bytes h) 0 dst off size *)

  let hash_bytes ?key l =
    let state = Blake2b.init ?key size in
    List.iter (fun b -> Blake2b.update state b) l ;
    Blake2b.final state

  let hash_string ?key l =
    let key = Option.map ~f:Bigstring.of_string key in
    let state = Blake2b.init ?key size in
    List.iter (fun s -> Blake2b.update state (MBytes.of_string s)) l ;
    Blake2b.final state

  let path_length = 6
  let to_path key l =
    let `Hex key = to_hex key in
    String.sub key 0 2 :: String.sub key 2 2 ::
    String.sub key 4 2 :: String.sub key 6 2 ::
    String.sub key 8 2 :: String.sub key 10 (size * 2 - 10) :: l
  let of_path path =
    let path = String.concat "" path in
    of_hex_opt (`Hex path)
  let of_path_exn path =
    let path = String.concat "" path in
    of_hex_exn (`Hex path)

  let prefix_path p =
    let `Hex p = Hex.of_string p in
    let len = String.length p in
    let p1 = if len >= 2 then String.sub p 0 2 else ""
    and p2 = if len >= 4 then String.sub p 2 2 else ""
    and p3 = if len >= 6 then String.sub p 4 2 else ""
    and p4 = if len >= 8 then String.sub p 6 2 else ""
    and p5 = if len >= 10 then String.sub p 8 2 else ""
    and p6 = if len > 10 then String.sub p 10 (min (len - 10) (size * 2 - 10)) else "" in
    [ p1 ; p2 ; p3 ; p4 ; p5 ; p6 ]

  let zero = of_hex_exn (`Hex (String.make (size * 2) '0'))

  include Compare.Make(struct
      type nonrec t = t
      let compare (Blake2b.Hash h1) (Blake2b.Hash h2) = MBytes.compare h1 h2
    end)

end

module Make (R : sig
    val register_encoding:
      prefix: string ->
      length:int ->
      to_raw: ('a -> string) ->
      of_raw: (string -> 'a option) ->
      wrap: ('a -> Base58.data) ->
      'a Base58.encoding
  end) (K : PrefixedName) = struct

  include Make_minimal(K)

  (* Serializers *)

  let raw_encoding =
    let open Data_encoding in
    conv to_bytes of_bytes_exn (Fixed.bytes size)

  let hash =
    if Compare.Int.(size >= 8) then
      fun h -> Int64.to_int (MBytes.get_int64 (to_bytes h) 0)
    else if Compare.Int.(size >= 4) then
      fun h -> Int32.to_int (MBytes.get_int32 (to_bytes h) 0)
    else
      fun h ->
        let r = ref 0 in
        let h = to_bytes h in
        for i = 0 to size - 1 do
          r := MBytes.get_uint8 h i + 8 * !r
        done ;
        !r

  type Base58.data += Data of t

  let b58check_encoding =
    R.register_encoding
      ~prefix: K.b58check_prefix
      ~length: size
      ~wrap: (fun s -> Data s)
      ~of_raw: of_string_opt
      ~to_raw: to_string

  include Helpers.Make(struct
      type nonrec t = t
      let title = title
      let name = name
      let b58check_encoding = b58check_encoding
      let raw_encoding = raw_encoding
      let compare = compare
      let equal = equal
      let hash = hash
    end)

end

include
  Make_minimal (struct
    let name = "Generic_hash"
    let title = ""
    let size = None
  end)