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

module Wrapped_error_monad = struct
  type unwrapped = Error_monad.error = ..
  include (Error_monad : Error_monad_sig.S with type error := unwrapped)
  let unwrap = function
    | Ecoproto_error ecoerror -> Some ecoerror
    | _ -> None
  let wrap ecoerror =
    Ecoproto_error ecoerror
end

let () =
  let id = Format.asprintf "proto.%s.wrapper" "testing" in
  register_wrapped_error_kind
    (module Wrapped_error_monad)
    ~id ~title: ("Error returned by protocol " ^ "testing")
    ~description: ("Wrapped error for economic protocol " ^ "testing" ^ ".")

let to_string err =
  let json = Error_monad.json_of_error err in
  Data_encoding.Json.to_string json

let to_string_list err =
  Format.printf "%a" Error_monad.pp_print_error err

let print err =
  Format.printf "%s\n" @@ to_string err


let force_ok ?(msg = "") = function
  | Ok x -> x
  | Error errs ->
    Format.printf "Errors :\n";
    to_string_list errs;
    raise @@ Failure ("force_ok : " ^ msg)


let init ?(slow=false) (* ?preserved_cycles ?endorsers_per_block ?commitments *) n =
  let accounts = Account.generate_accounts n in
  let contracts = List.map (fun (a, _) ->
    Contract.implicit_contract Account.(a.pkh)) accounts in 
  let blk = 
  begin
  if slow then
    Block.genesis
      (* ?preserved_cycles
      ?endorsers_per_block
      ?commitments *)
      accounts
  else
    Block.genesis
      (* ?preserved_cycles
      ~blocks_per_cycle:32l
      ~blocks_per_commitment:4l
      ~blocks_per_roll_snapshot:8l
      ?endorsers_per_block
      ?commitments *)
      accounts
  end in
  return (blk, contracts, List.map fst accounts)

let init_environment () =
  init 10 >>=? fun (blk, contracts, accounts) ->
  (* Incremental.begin_construction blk >>=? fun i ->
  let tezos_context = Incremental.context i in
  let tezos_context = Proto_alpha.Alpha_context.Gas.set_limit tezos_context @@ Z.of_int 350000 in *)
  let tezos_context = {Context.default_context with block=blk} in
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