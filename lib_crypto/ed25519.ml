(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
open Tezos_stdlib
open Tezos_data_encoding
open Data_encoding
open Hacl

let size = Sign.bytes
let of_bytes_opt s =
  if MBytes.length s = size then Some s else None
let to_bytes x = x

module Public_key_hash = struct

  open Blake2
  type t = Blake2b.hash

  type Base58.data +=
  | Data of t

  let name = "Ed25519.Public_key_hash"
  let title = "An Ed25519 public key hash"

  let size = 20

  let of_string_opt s =
    if String.length s <> size then
      None
    else
      Some (Blake2b.Hash (MBytes.of_string s))

  let to_string (Blake2b.Hash h) = MBytes.to_string h
  let to_bytes (Blake2b.Hash h) = h

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
            name (MBytes.length b) in
        raise (Invalid_argument msg)
    | Some h -> h

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.ed25519_public_key_hash
      ~length: size
      ~wrap: (fun s -> Data s)
      ~of_raw: of_string_opt
      ~to_raw: to_string

  let of_b58check_opt s =
    Base58.simple_decode b58check_encoding s

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Pervasives.failwith "Unexpected data (%s)" name

  let raw_encoding =
    let open Data_encoding in
    conv to_bytes of_bytes_exn (Fixed.bytes size)

  let hash_bytes ?key l =
    let state = Blake2b.init ?key size in
    List.iter (fun b -> Blake2b.update state b) l ;
    Blake2b.final state

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

  let to_b58check s = Base58.simple_encode b58check_encoding s

  let encoding =
    let open Data_encoding in
    splitted
      ~binary:
        (obj1 (req name raw_encoding))
      ~json:
        (def name
           ~title: (title ^ " (Base58Check-encoded)") @@
         conv
           to_b58check
           (Data_encoding.Json.wrap_error of_b58check_exn)
           string)

end

module Public_key = struct

  type t = public Sign.key
  let name = "Ed25519.Public_key"
  let title = "Ed25519 public key"
  let to_string s = MBytes.to_string (Sign.unsafe_to_bytes s)
  let of_string_opt s =
    if String.length s < Sign.pkbytes then None
    else
      let pk = MBytes.create Sign.pkbytes in
      MBytes.blit_of_string s 0 pk 0 Sign.pkbytes ;
      Some (Sign.unsafe_pk_of_bytes pk)

  let to_bytes pk =
    let buf = MBytes.create Sign.pkbytes in
    Sign.blit_to_bytes pk buf ;
    buf

  let of_bytes_opt buf =
    let buflen = MBytes.length buf in
    if buflen < Sign.pkbytes then None
    else
      let pk = MBytes.create Sign.pkbytes in
      MBytes.blit buf 0 pk 0 Sign.pkbytes ;
      Some (Sign.unsafe_pk_of_bytes pk)


  let size = Sign.pkbytes

  type Base58.data +=
    | Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.ed25519_public_key
      ~length: size
      ~to_raw: to_string
      ~of_raw: of_string_opt
      ~wrap: (fun x -> Data x)

  let of_bytes_exn s =
    match of_bytes_opt s with
    | None ->
        Format.kasprintf invalid_arg "of_bytes_exn (%s)" name
    | Some pk -> pk

  let to_b58check s = Base58.simple_encode b58check_encoding s

  let of_b58check_opt s =
    Base58.simple_decode b58check_encoding s

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Pervasives.failwith "Unexpected data (%s)" name

  let raw_encoding = conv to_bytes of_bytes_exn (Fixed.bytes size)

  let hash v =
    Public_key_hash.hash_bytes [ Sign.unsafe_to_bytes v ]

  let encoding =
    splitted
        ~binary:
          (obj1 (req name raw_encoding))
        ~json:
          (def name
             ~title: (title ^ " (Base58Check-encoded)") @@
           conv
             to_b58check
             (Data_encoding.Json.wrap_error of_b58check_exn)
             string)
end


