type t = {
  source : int;
  payer : int;
  self : int;
  amount : Tez.t;
  parameter : Script.node ;
  storage : Script.node;
}

let default_execution_context = {
  source = 0;
  payer = 0;
  self = 0;
  amount = Tez.zero;
  parameter = String (0, "");
  storage = String (0, "");
}

type ('param, 'storage) typed_t = {
  source : int;
  payer : int;
  self : int;
  amount : Tez.t;
  parameter : 'param;
  storage : 'storage;
}

let default_typed_execution_context param storage =
  {
    source = 0;
    payer = 0;
    self = 0;
    amount = Tez.zero;
    parameter = param;
    storage = storage;
  }

type execution_trace =
  (Script.location * Gas.t * (Script.expr * string option) list) list

let with_source_index ind ctxt =
  {ctxt with source = ind }

let with_payer_index ind ctxt =
  {ctxt with payer = ind}

let with_self_index ind ctxt =
  { ctxt with self = ind }

let with_amount_index ind ctxt = 
  {ctxt with amount = ind}

let with_parameter param ctxt = 
  {ctxt with parameter = param}

let with_storage storage ctxt = 
  { ctxt with storage = storage }

