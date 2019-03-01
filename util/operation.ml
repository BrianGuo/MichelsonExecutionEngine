open Script_ir_nodes
open Tezos_data_encoding
open Data_encoding


let case tag name args proj inj =
    let open Data_encoding in
    case tag
      ~title:(String.capitalize_ascii name)
      (merge_objs
         (obj1 (req "kind" (constant name)))
         args)
      (fun x -> match proj x with None -> None | Some x -> Some ((), x))
      (fun ((), x) -> inj x)

module Manager_operations = struct

    type 'kind case =
        MCase : { tag: int ;
                  name: string ;
                  encoding: 'a Data_encoding.t ;
                  select: packed_manager_operation -> 'kind manager_operation option ;
                  proj: 'kind manager_operation -> 'a ;
                  inj: 'a -> 'kind manager_operation } -> 'kind case

    let reveal_case =
      MCase {
        tag = 0 ;
        name = "reveal" ;
        encoding =
          (obj1
             (req "public_key" Signature.Public_key.encoding)) ;
        select =
          (function
            | Manager (Reveal _ as op) -> Some op
            | _ -> None) ;
        proj =
          (function Reveal pkh -> pkh) ;
        inj =
          (fun pkh -> Reveal pkh)
      }

    let transaction_case =
      MCase {
        tag = 1 ;
        name = "transaction" ;
        encoding =
          (obj3
             (req "amount" Tez.encoding)
             (req "destination" Contract.encoding)
             (opt "parameters" Script.lazy_expr_encoding)) ;
        select =
          (function
            | Manager (Transaction _ as op) -> Some op
            | _ -> None) ;
        proj =
          (function
            | Transaction { amount ; destination ; parameters } ->
                (amount, destination, parameters)) ;
        inj =
          (fun (amount, destination, parameters) ->
             Transaction { amount ; destination ; parameters })
      }

    let origination_case =
      MCase {
        tag = 2 ;
        name = "origination" ;
        encoding =
          (obj6
             (req "managerPubkey" Signature.Public_key_hash.encoding)
             (req "balance" Tez.encoding)
             (dft "spendable" bool true)
             (dft "delegatable" bool true)
             (opt "delegate" Signature.Public_key_hash.encoding)
             (opt "script" Script.encoding)) ;
        select =
          (function
            | Manager (Origination _ as op) -> Some op
            | _ -> None) ;
        proj =
          (function
            | Origination { manager ; credit ; spendable ;
                            delegatable ; delegate ; script ;
                            preorigination = _
                            (* the hash is only used internally
                               when originating from smart
                               contracts, don't serialize it *) } ->
                (manager, credit, spendable,
                 delegatable, delegate, script)) ;
        inj =
          (fun (manager, credit, spendable, delegatable, delegate, script) ->
             Origination
               {manager ; credit ; spendable ; delegatable ;
                delegate ; script ; preorigination = None })
      }

    let delegation_case =
      MCase {
        tag = 3 ;
        name = "delegation" ;
        encoding =
          (obj1
             (opt "delegate" Signature.Public_key_hash.encoding)) ;
        select =
          (function
            | Manager (Delegation _ as op) -> Some op
            | _ -> None) ;
        proj =
          (function Delegation key -> key) ;
        inj =
          (fun key -> Delegation key)
      }

    let encoding =
      let make (MCase { tag ; name ; encoding ; select ; proj ; inj }) =
        case (Tag tag) name encoding
          (fun o -> match select o with None -> None | Some o -> Some (proj o))
          (fun x -> Manager (inj x)) in
      union ~tag_size:`Uint8 [
        make reveal_case ;
        make transaction_case ;
        make origination_case ;
        make delegation_case ;
      ]

  end

let internal_operation_encoding =
  def "operation.alpha.internal_operation" @@
  conv
    (fun (Internal_operation { source ; operation ; nonce }) ->
       ((source, nonce), Manager operation))
    (fun ((source, nonce), Manager operation) ->
       Internal_operation { source ; operation ; nonce })
    (merge_objs
       (obj2
          (req "source" Contract.encoding)
          (req "nonce" uint16))
       Manager_operations.encoding)