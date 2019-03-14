type t = {
  source : int;
  payer : int;
  self : int;
  amount : Tez.t;
  parameter : string ;
  storage : string;
}

let default_execution_context = {
  source = 0;
  payer = 0;
  self = 0;
  amount = Tez.zero;
  parameter = "";
  storage = "";
}

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

