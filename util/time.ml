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

open Tezos_error_monad
open Error_monad
open Tezos_data_encoding
type error += Timestamp_add (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"timestamp_add"
    ~title:"Timestamp add"
    ~description:"Overflow when adding timestamps."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Overflow when adding timestamps.")
    Data_encoding.empty
    (function Timestamp_add -> Some () | _ -> None)
    (fun () -> Timestamp_add)

let of_seconds x = x

let to_seconds x = x
let to_seconds_string s = Int64.to_string (to_seconds s)
let to_notation _ =
    (* let ft = Int64.to_float t in
    if Int64.of_float ft <> t then
      "out_of_range"
    else
      Printer.Precise_Calendar.sprint
        "%Y-%m-%dT%H:%M:%SZ"
        (Calendar.Precise.from_unixfloat ft) TODO: find the time printer *)
        "time fillin"

let of_notation _ = Some Int64.zero
let pp_hum ppf t = Format.pp_print_string ppf (to_notation t)

let pp = pp_hum
