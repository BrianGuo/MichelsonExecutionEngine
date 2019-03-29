open Script_ir_nodes

let unit_t = Unit_t None
let int_t = Int_t None
let nat_t = Nat_t None
let signature_t = Signature_t None
let string_t = String_t None
let bytes_t = Bytes_t None
let mutez_t = Mutez_t None
let key_hash_t = Key_hash_t None
let key_t = Key_t None
let timestamp_t = Timestamp_t None
let address_t = Address_t None
let bool_t = Bool_t None
let pair_t t1 t2 = Pair_t ((t1, None, None), (t2, None, None), None)
let union_t t1 t2 = Union_t ((t1, None), (t2, None), None)
let lambda_t arg ret = Lambda_t (arg, ret, None)
let option_t arg = Option_t ((arg, None), None, None)
let list_t t = List_t (t, None)
let set_t t = Set_t (t, None)
let map_t key value = Map_t (key, value, None)
let big_map_t key value = Big_map_t (key, value, None)
let contract_t arg = Contract_t (arg, None)
let operation_t = Operation_t None

let int_key = Int_key None
let nat_key = Nat_key None
let string_key = String_key None
let bytes_key = Bytes_key None
let mutez_key = Mutez_key None
let bool_key = Bool_key None
let key_hash_key = Key_hash_key None
let timestamp_key = Timestamp_key None
let address_key = Address_key None

module type Env = sig
  val param_type : 'a ty 
  val storage_type : 'b ty
end