open Tezos_error_monad
open Error_monad
open Tezos_micheline
open Tezos_data_encoding

type identity = {
  public_key_hash : Signature.public_key_hash;
  public_key : Signature.public_key;
  secret_key : Signature.secret_key;
  implicit_contract : Contract.t;
}

type environment = {
  tezos_context : Context.t ;
  identities : identity list ;
}

type error += Ecoproto_error of Error_monad.error

let to_string err =
  let json = Error_monad.json_of_error err in
  Data_encoding.Json.to_string json

let print err =
  Format.printf "%s\n" @@ to_string err

let wrap_error = function
      | Ok _ as ok -> ok
      | Error errors -> Error (List.map (fun error -> Ecoproto_error error) errors)

let (>>=??) a f =
  a >>= fun a ->
  match wrap_error a with
  | Ok result -> f result
  | Error errs -> Lwt.return (Error errs)


let force_ok ?(msg = "") = function
  | Ok x -> x
  | Error errs ->
    Format.printf "Errors :\n";
    List.iter print errs ;
    raise @@ Failure ("force_ok : " ^ msg)

let force_ok_alpha ~msg a = force_ok ~msg @@ wrap_error a

let init_environment () =
  Context.init 10 >>=? fun (blk, contracts, accounts) ->
  (* Incremental.begin_construction blk >>=? fun i ->
  let tezos_context = Incremental.context i in
  let tezos_context = Proto_alpha.Alpha_context.Gas.set_limit tezos_context @@ Z.of_int 350000 in *)
  let tezos_context = {Context_type.default_context with block=blk} in
  let identities =
    List.map (fun ((a:Account.t), c) -> {
          public_key = a.pk ;
          public_key_hash = a.pkh ;
          secret_key = a.sk ;
          implicit_contract = c
        }) @@
    List.combine accounts contracts in
  return {tezos_context ; identities}

let contextualize ~msg ?environment f =
  let lwt =
    let environment = match environment with
      | None -> init_environment ()
      | Some x -> return x in
    environment >>=? f
  in
  force_ok ~msg @@ Lwt_main.run lwt

let node_to_string (node:_ Micheline.node) =
  let stripped = Micheline.strip_locations node in
  let print_node = Micheline_printer.printable Michelson_v1_primitives.string_of_prim stripped in
  Micheline_printer.print_expr Format.str_formatter print_node ;
  Format.flush_str_formatter ()