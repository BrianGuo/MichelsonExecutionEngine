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

open Tezos_error_monad.Error_monad

let error ~loc v f =
  match v with
  | Error err when List.exists f err ->
      return_unit
  | Ok _ ->
      failwith "Unexpected successful result (%s)" loc
  | Error err ->
      failwith "@[Unexpected error (%s): %a@]" loc pp_print_error err

let equal ~loc (cmp : 'a -> 'a -> bool) msg pp a b  =
  if not (cmp a b) then
    failwith "@[@[[%s]@] - @[%s : %a is not equal to %a@]@]" loc msg pp a pp b
  else
    return_unit

let not_equal ~loc (cmp : 'a -> 'a -> bool) msg pp a b  =
  if cmp a b then
    failwith "@[@[[%s]@] - @[%s : %a is equal to %a@]@]" loc msg pp a pp b
  else
    return_unit

(* tez *)
let equal_tez ~loc (a:Tez.t) (b:Tez.t) =
  equal ~loc Tez.(=) "Tez aren't equal" Tez.pp a b

let not_equal_tez ~loc (a:Tez.t) (b:Tez.t) =
  not_equal ~loc Tez.(=) "Tez are equal" Tez.pp a b

(* int *)
let equal_int ~loc (a:int) (b:int) =
  equal ~loc (=) "Integers aren't equal" Format.pp_print_int a b

let not_equal_int ~loc (a:int) (b:int) =
  not_equal ~loc (=) "Integers are equal" Format.pp_print_int a b

(* bool *)
let equal_bool ~loc (a:bool) (b:bool) =
  equal ~loc (=) "Booleans aren't equal" Format.pp_print_bool a b

let not_equal_bool ~loc (a:bool) (b:bool) =
  not_equal ~loc (=) "Booleans are equal" Format.pp_print_bool a b
