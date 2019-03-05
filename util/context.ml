open Tezos_error_monad
open Error_monad

type t = Context_type.t

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

let with_block_gas gas_count ctxt =
  let open Context_type in
  {ctxt with block_gas = Z.of_int gas_count}

let with_maximum_gas gas_limit ctxt =
  let open Context_type in
  {ctxt with gas = Limited {remaining = (Z.of_int gas_limit)} }

let initialize_account_balance ~index ~balance ctxt =
  let open Context_type in
  let bindings = Storage_map_mod.bindings ctxt.storage_map in
  let contract_hash = fst @@ List.nth bindings index in
  let update_balance_fun old_balance = 
    match old_balance with
    | Some _ -> Some balance
    | None -> None 
  in
  {ctxt with storage_map = Storage_map_mod.update contract_hash update_balance_fun ctxt.storage_map }