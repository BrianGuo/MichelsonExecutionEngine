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