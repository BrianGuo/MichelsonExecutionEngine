open Script_ir_nodes

(* todo: maybe there is a visitor pattern way of doing this? *)
type simple_comparable_ty =
  | Int_key
  | Nat_key 
  | String_key 
  | Bytes_key 
  | Mutez_key 
  | Bool_key 
  | Key_hash_key 
  | Timestamp_key 
  | Address_key

type simple_ty =
  | Unit_t 
  | Int_t 
  | Nat_t
  | Signature_t
  | String_t
  | Bytes_t
  | Mutez_t
  | Key_hash_t
  | Key_t
  | Timestamp_t
  | Address_t
  | Bool_t
  | Pair_t of (simple_ty * simple_ty)
  | Union_t of (simple_ty * simple_ty)
  | Lambda_t of (simple_ty * simple_ty)
  | Option_t of simple_ty
  | List_t of simple_ty
  | Set_t of simple_comparable_ty
  | Map_t of (simple_comparable_ty * simple_ty)
  | Big_map_t of (simple_comparable_ty * simple_ty)
  | Contract_t of simple_ty
  | Operation_t

  (* and ('bef, 'aft) instr =
  (* stack ops *)
  | Drop :
      (_ * 'rest, 'rest) instr
  | Dup :
      ('top * 'rest, 'top * ('top * 'rest)) instr
  | Swap :
      ('tip * ('top * 'rest), 'top * ('tip * 'rest)) instr
  | Const : 'ty ->
    ('rest, ('ty * 'rest)) instr
  (* pairs *)
  | Cons_pair :
      (('car * ('cdr * 'rest)), (('car, 'cdr) pair * 'rest)) instr
  | Car :
      (('car, _) pair * 'rest, 'car * 'rest) instr
  | Cdr :
      ((_, 'cdr) pair * 'rest, 'cdr * 'rest) instr
  (* options *)
  | Cons_some :
      ('v * 'rest, 'v option * 'rest) instr
  | Cons_none : 'a ty ->
    ('rest, 'a option * 'rest) instr
  | If_none : ('bef, 'aft) descr * ('a * 'bef, 'aft) descr ->
    ('a option * 'bef, 'aft) instr
  (* unions *)
  | Left :
      ('l * 'rest, (('l, 'r) union * 'rest)) instr
  | Right :
      ('r * 'rest, (('l, 'r) union * 'rest)) instr
  | If_left : ('l * 'bef, 'aft) descr * ('r * 'bef, 'aft) descr ->
    (('l, 'r) union * 'bef, 'aft) instr
  (* lists *)
  | Cons_list :
      ('a * ('a list * 'rest), ('a list * 'rest)) instr
  | Nil :
      ('rest, ('a list * 'rest)) instr
  | If_cons : ('a * ('a list * 'bef), 'aft) descr * ('bef, 'aft) descr ->
    ('a list * 'bef, 'aft) instr
  | List_map : ('a * 'rest, 'b * 'rest) descr ->
    ('a list * 'rest, 'b list * 'rest) instr
  | List_iter : ('a * 'rest, 'rest) descr ->
    ('a list * 'rest, 'rest) instr
  | List_size : ('a list * 'rest, n num * 'rest) instr
  (* sets *)
  | Empty_set : 'a comparable_ty ->
    ('rest, 'a set * 'rest) instr
  | Set_iter : ('a * 'rest, 'rest) descr ->
    ('a set * 'rest, 'rest) instr
  | Set_mem :
      ('elt * ('elt set * 'rest), bool * 'rest) instr
  | Set_update :
      ('elt * (bool * ('elt set * 'rest)), 'elt set * 'rest) instr
  | Set_size : ('a set * 'rest, n num * 'rest) instr
  (* maps *)
  | Empty_map : 'a comparable_ty * 'v ty ->
    ('rest, ('a, 'v) map * 'rest) instr
  | Map_map : (('a * 'v) * 'rest, 'r * 'rest) descr ->
    (('a, 'v) map * 'rest, ('a, 'r) map * 'rest) instr
  | Map_iter : (('a * 'v) * 'rest, 'rest) descr ->
    (('a, 'v) map * 'rest, 'rest) instr
  | Map_mem :
      ('a * (('a, 'v) map * 'rest), bool * 'rest) instr
  | Map_get :
      ('a * (('a, 'v) map * 'rest), 'v option * 'rest) instr
  | Map_update :
      ('a * ('v option * (('a, 'v) map * 'rest)), ('a, 'v) map * 'rest) instr
  | Map_size : (('a, 'b) map * 'rest, n num * 'rest) instr
  (* big maps *)
  | Big_map_mem :
      ('a * (('a, 'v) big_map * 'rest), bool * 'rest) instr
  | Big_map_get :
      ('a * (('a, 'v) big_map * 'rest), 'v option * 'rest) instr
  | Big_map_update :
      ('key * ('value option * (('key, 'value) big_map * 'rest)), ('key, 'value) big_map * 'rest) instr
  (* string operations *)
  | Concat_string :
      (string list * 'rest, string * 'rest) instr
  | Concat_string_pair :
      (string * (string * 'rest), string * 'rest) instr
  | Slice_string :
      (n num * (n num * (string * 'rest)), string option * 'rest) instr
  | String_size :
      (string * 'rest, n num * 'rest) instr
  (* bytes operations *)
  | Concat_bytes :
      (MBytes.t list * 'rest, MBytes.t * 'rest) instr
  | Concat_bytes_pair :
      (MBytes.t * (MBytes.t * 'rest), MBytes.t * 'rest) instr
  | Slice_bytes :
      (n num * (n num * (MBytes.t * 'rest)), MBytes.t option * 'rest) instr
  | Bytes_size :
      (MBytes.t * 'rest, n num * 'rest) instr
  (* timestamp operations *)
  | Add_seconds_to_timestamp :
      (z num * (Script_timestamp.t * 'rest),
       Script_timestamp.t * 'rest) instr
  | Add_timestamp_to_seconds :
      (Script_timestamp.t * (z num * 'rest),
       Script_timestamp.t * 'rest) instr
  | Sub_timestamp_seconds :
      (Script_timestamp.t * (z num * 'rest),
       Script_timestamp.t * 'rest) instr
  | Diff_timestamps :
      (Script_timestamp.t * (Script_timestamp.t * 'rest),
       z num * 'rest) instr
  (* currency operations *)
  (* TODO: we can either just have conversions to/from integers and
     do all operations on integers, or we need more operations on
     Tez. Also Sub_tez should return Tez.t option (if negative) and *)
  | Add_tez :
      (Tez.t * (Tez.t * 'rest), Tez.t * 'rest) instr
  | Sub_tez :
      (Tez.t * (Tez.t * 'rest), Tez.t * 'rest) instr
  | Mul_teznat :
      (Tez.t * (n num * 'rest), Tez.t * 'rest) instr
  | Mul_nattez :
      (n num * (Tez.t * 'rest), Tez.t * 'rest) instr
  | Ediv_teznat :
      (Tez.t * (n num * 'rest), ((Tez.t, Tez.t) pair) option * 'rest) instr
  | Ediv_tez :
      (Tez.t * (Tez.t * 'rest), ((n num, Tez.t) pair) option * 'rest) instr
  (* boolean operations *)
  | Or :
      (bool * (bool * 'rest), bool * 'rest) instr
  | And :
      (bool * (bool * 'rest), bool * 'rest) instr
  | Xor :
      (bool * (bool * 'rest), bool * 'rest) instr
  | Not :
      (bool * 'rest, bool * 'rest) instr
  (* integer operations *)
  | Is_nat :
      (z num * 'rest, n num option * 'rest) instr
  | Neg_nat :
      (n num * 'rest, z num * 'rest) instr
  | Neg_int :
      (z num * 'rest, z num * 'rest) instr
  | Abs_int :
      (z num * 'rest, n num * 'rest) instr
  | Int_nat :
      (n num * 'rest, z num * 'rest) instr
  | Add_intint :
      (z num * (z num * 'rest), z num * 'rest) instr
  | Add_intnat :
      (z num * (n num * 'rest), z num * 'rest) instr
  | Add_natint :
      (n num * (z num * 'rest), z num * 'rest) instr
  | Add_natnat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | Sub_int :
      ('s num * ('t num * 'rest), z num * 'rest) instr
  | Mul_intint :
      (z num * (z num * 'rest), z num * 'rest) instr
  | Mul_intnat :
      (z num * (n num * 'rest), z num * 'rest) instr
  | Mul_natint :
      (n num * (z num * 'rest), z num * 'rest) instr
  | Mul_natnat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | Ediv_intint :
      (z num * (z num * 'rest), ((z num, n num) pair) option * 'rest) instr
  | Ediv_intnat :
      (z num * (n num * 'rest), ((z num, n num) pair) option * 'rest) instr
  | Ediv_natint :
      (n num * (z num * 'rest), ((z num, n num) pair) option * 'rest) instr
  | Ediv_natnat :
      (n num * (n num * 'rest), ((n num, n num) pair) option * 'rest) instr
  | Lsl_nat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | Lsr_nat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | Or_nat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | And_nat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | And_int_nat :
      (z num * (n num * 'rest), n num * 'rest) instr
  | Xor_nat :
      (n num * (n num * 'rest), n num * 'rest) instr
  | Not_nat :
      (n num * 'rest, z num * 'rest) instr
  | Not_int :
      (z num * 'rest, z num * 'rest) instr
  (* control *)
  | Seq : ('bef, 'trans) descr * ('trans, 'aft) descr ->
    ('bef, 'aft) instr
  | If : ('bef, 'aft) descr * ('bef, 'aft) descr ->
    (bool * 'bef, 'aft) instr
  | Loop : ('rest, bool * 'rest) descr ->
    (bool * 'rest, 'rest) instr
  | Loop_left : ('a * 'rest, ('a, 'b) union * 'rest) descr ->
    (('a, 'b) union * 'rest, 'b * 'rest) instr
  | Dip : ('bef, 'aft) descr ->
    ('top * 'bef, 'top * 'aft) instr
  | Exec :
      ('arg * (('arg, 'ret) lambda * 'rest), 'ret * 'rest) instr
  | Lambda : ('arg, 'ret) lambda ->
    ('rest, ('arg, 'ret) lambda * 'rest) instr
  | Failwith :
      'a ty -> ('a * 'rest, 'aft) instr
  | Nop :
      ('rest, 'rest) instr
  (* comparison *)
  | Compare : 'a comparable_ty ->
    ('a * ('a * 'rest), z num * 'rest) instr
  (* comparators *)
  | Eq :
      (z num * 'rest, bool * 'rest) instr
  | Neq :
      (z num * 'rest, bool * 'rest) instr
  | Lt :
      (z num * 'rest, bool * 'rest) instr
  | Gt :
      (z num * 'rest, bool * 'rest) instr
  | Le :
      (z num * 'rest, bool * 'rest) instr
  | Ge :
      (z num * 'rest, bool * 'rest) instr

  (* protocol *)
  | Address :
      (_ typed_contract * 'rest, Context_type.contract_type * 'rest) instr
  | Contract : 'p ty ->
    (Context_type.contract_type * 'rest, 'p typed_contract option * 'rest) instr
  | Transfer_tokens :
      ('arg * (Tez.t * ('arg typed_contract * 'rest)), packed_internal_operation * 'rest) instr
  | Create_account :
      (public_key_hash * (public_key_hash option * (bool * (Tez.t * 'rest))),
       packed_internal_operation * (Context_type.contract_type * 'rest)) instr
  | Implicit_account :
      (public_key_hash * 'rest, unit typed_contract * 'rest) instr
  | Create_contract : 'g ty * 'p ty * ('p * 'g, packed_internal_operation list * 'g) lambda  ->
    (public_key_hash * (public_key_hash option * (bool * (bool * (Tez.t * ('g * 'rest))))),
     packed_internal_operation * (Context_type.contract_type * 'rest)) instr
  | Set_delegate :
      (public_key_hash option * 'rest, packed_internal_operation * 'rest) instr
  | Now :
      ('rest, Script_timestamp.t * 'rest) instr
  | Balance :
      ('rest, Tez.t * 'rest) instr
  | Check_signature :
      (public_key * (signature * (MBytes.t * 'rest)), bool * 'rest) instr
  | Hash_key :
      (public_key * 'rest, public_key_hash * 'rest) instr
  | Pack : 'a ty ->
    ('a * 'rest, MBytes.t * 'rest) instr
  | Unpack : 'a ty ->
    (MBytes.t * 'rest, 'a option * 'rest) instr
  | Blake2b :
      (MBytes.t * 'rest, MBytes.t * 'rest) instr
  | Sha256 :
      (MBytes.t * 'rest, MBytes.t * 'rest) instr
  | Sha512 :
      (MBytes.t * 'rest, MBytes.t * 'rest) instr
  | Steps_to_quota : (* TODO: check that it always returns a nat *)
      ('rest, n num * 'rest) instr
  | Source :
      ('rest, Context_type.contract_type * 'rest) instr
  | Sender :
      ('rest, Context_type.contract_type * 'rest) instr
  | Self : 'p ty ->
    ('rest, 'p typed_contract * 'rest) instr
  | Amount :
      ('rest, Tez.t * 'rest) instr
      
and unparameterized_descr =
{ loc : Script.location ;
  instr : (simple_ty * simple_ty) instr;
  bef : simple_ty stack_ty ;
  aft : simple_ty stack_ty ; }

and unparameterized_lambda =
| Lam of (simple_ty * unit, simple_ty * unit) unparameterized_descr * Script.expr

and unparameterized_toplevel = {
  param_type : simple_ty ;
  storage_type : simple_ty ;
  code : (simple_ty * simple_ty, packed_internal_operation list * simple_ty) unparameterized_lambda
} *)

let simple_comp_ty_of_comp_ty 
: type ta. ta comparable_ty -> simple_comparable_ty 
  = fun comp_ty ->
  match comp_ty with
  | Int_key _ -> Int_key
  | Nat_key _ -> Nat_key 
  | String_key _ -> String_key 
  | Bytes_key _ -> Bytes_key 
  | Mutez_key _ -> Mutez_key 
  | Bool_key _ -> Bool_key 
  | Key_hash_key _ -> Key_hash_key 
  | Timestamp_key _ -> Timestamp_key 
  | Address_key _ -> Address_key

let rec simple_ty_of_ty 
  : type ta. ta ty -> simple_ty 
  = fun ty -> 
  match ty with
  | Unit_t _ -> Unit_t
  | Int_t _ -> Int_t
  | Nat_t _ -> Nat_t
  | Signature_t _ -> Signature_t
  | String_t _ -> String_t
  | Bytes_t _ -> Bytes_t
  | Mutez_t _ -> Mutez_t 
  | Key_hash_t _ -> Key_hash_t
  | Key_t _ -> Key_t
  | Timestamp_t _ -> Timestamp_t
  | Address_t _ -> Address_t
  | Bool_t _ -> Bool_t
  | Pair_t ((t1, _, _), (t2, _, _), _) -> Pair_t (simple_ty_of_ty t1, simple_ty_of_ty t2)
  | Union_t ((t1, _), (t2, _), _) -> Union_t (simple_ty_of_ty t1, simple_ty_of_ty t2)
  | Lambda_t (t1, t2, _) -> Lambda_t (simple_ty_of_ty t1, simple_ty_of_ty t2)
  | Option_t ((t1, _), _, _ ) -> Option_t (simple_ty_of_ty t1)
  | List_t (t1, _) -> List_t (simple_ty_of_ty t1)
  | Set_t (t1, _) -> Set_t (simple_comp_ty_of_comp_ty t1)
  | Map_t (t1, t2, _) -> Map_t (simple_comp_ty_of_comp_ty t1, simple_ty_of_ty t2)
  | Big_map_t (t1, t2, _) -> Big_map_t (simple_comp_ty_of_comp_ty t1, simple_ty_of_ty t2)
  | Contract_t (t1, _) -> Contract_t (simple_ty_of_ty t1)
  | Operation_t _ -> Operation_t

(* let comparable_ty_of_simple_ty
  : simple_comparable_ty -> ta comparable_ty
  = fun comp_ty ->
  match comp_ty with
  | Int_key -> Int_key None
  | Nat_key -> Nat_key  None
  | String_key -> String_key  None
  | Bytes_key -> Bytes_key  None
  | Mutez_key -> Mutez_key  None
  | Bool_key -> Bool_key  None
  | Key_hash_key -> Key_hash_key  None
  | Timestamp_key -> Timestamp_key  None
  | Address_key -> Address_key None

let rec ty_of_simple_ty
  : type ta. simple_ty -> ta ty
  = fun ty ->
  match ty with
  | Unit_t  _ -> Unit_t None
  | Int_t  _ -> Int_t None
  | Nat_t _ -> Nat_t None
  | Signature_t _ -> Signature_t None
  | String_t _ -> String_t None
  | Bytes_t _ -> Bytes_t None
  | Mutez_t _ -> Mutez_t None
  | Key_hash_t _ -> Key_hash_t None
  | Key_t _ -> Key_t None
  | Timestamp_t _ -> Timestamp_t None
  | Address_t _ -> Address_t None
  | Bool_t _ -> Bool_t None
  | Pair_t (a, b) -> 
      Pair_t ((ty_of_simple_ty a, None, None), (ty_of_simple_ty b, None, None), None)
  | Union_t (a, b) ->
      Union_t ((ty_of_simple_ty a, None), (ty_of_simple_ty b, None), None)
  | Lambda_t (a, b) ->
      Lambda_t (ty_of_simple_ty a, ty_of_simple_ty b, None)
  | Option_t a -> Option_t ((ty_of_simple_ty a, None), None, None)
  | List_t a -> List_t (ty_of_simple_ty a, None)
  | Set_t a -> Set_t (comparable_ty_of_simple_ty a, None) 
  | Map_t (a, b) -> Map_t (comparable_ty_of_simple_ty a, ty_of_simple_ty b, None)
  | Big_map_t (a, b) -> Big_map_t (comparable_ty_of_simple_ty a, ty_of_simple_ty b, None)
  | Contract_t a -> Contract_t (ty_of_simple_ty a, None)
  | Operation_t _ -> Operation_t None *)