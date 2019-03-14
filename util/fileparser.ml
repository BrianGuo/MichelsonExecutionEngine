open Misc
open Script_ir_translator
open Script_ir_nodes
open Tezos_error_monad
open Error_monad

type ex_ty = Script_ir_nodes.ex_ty

type ('param, 'storage) toplevel = {
  param_type : 'param ty ;
  storage_type : 'storage ty ;
  code : ('param * 'storage, packed_internal_operation list * 'storage) lambda
}

type ex_toplevel = Ex_toplevel : ('a, 'b) toplevel -> ex_toplevel

let reconstruct_comparable_type
  : type a. a comparable_ty -> a comparable_ty
  = fun comp_ty ->
  match comp_ty with
  | Int_key _ -> Int_key None
  | Nat_key _ -> Nat_key  None
  | String_key _ -> String_key  None
  | Bytes_key _ -> Bytes_key  None
  | Mutez_key _ -> Mutez_key  None
  | Bool_key _ -> Bool_key  None
  | Key_hash_key _ -> Key_hash_key  None
  | Timestamp_key _ -> Timestamp_key  None
  | Address_key _ -> Address_key None

let rec reconstruct_type
  : type a. a ty -> a ty
  = fun ty -> 
  match ty with
  | Unit_t _ -> Unit_t None
  | Int_t _ -> Int_t None 
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
  | Pair_t ((t1, _, _), (t2, _, _), _) -> Pair_t ((reconstruct_type t1, None, None), (reconstruct_type t2, None, None), None)
  | Union_t ((t1, _), (t2, _), _) -> Union_t ((reconstruct_type t1, None), (reconstruct_type t2, None), None)
  | Lambda_t (t1, t2, _) -> Lambda_t (reconstruct_type t1, reconstruct_type t2, None)
  | Option_t ((t1, _), _, _ ) -> Option_t ((reconstruct_type t1, None), None, None)
  | List_t (t1, _) -> List_t (reconstruct_type t1, None)
  | Set_t (t1, _) -> Set_t (reconstruct_comparable_type t1, None)
  | Map_t (t1, t2, _) -> Map_t (reconstruct_comparable_type t1, reconstruct_type t2, None)
  | Big_map_t (t1, t2, _) -> Big_map_t (reconstruct_comparable_type t1, reconstruct_type t2, None)
  | Contract_t (t1, _) -> Contract_t (reconstruct_type t1, None)
  | Operation_t _ -> Operation_t None
    

let get_toplevel_node _ toplevel_path =
  let toplevel_str = Streams.read_file toplevel_path in
  let toplevel_expr = Cast.tl_of_string toplevel_str in
  Lwt.return @@ parse_toplevel toplevel_expr

let get_toplevel_object ?environment toplevel_path claimed_storage_type claimed_parameter_type =
  let toplevel_str = Streams.read_file toplevel_path in
  contextualize ?environment ~msg:"toplevel" @@ fun {tezos_context = context; _} ->
  let toplevel_expr = Cast.tl_of_string toplevel_str in
  let (param_ty_node, storage_ty_node, code_field) =
    force_ok ~msg:"parsing toplevel" @@
    parse_toplevel toplevel_expr in
  let (Ex_ty param_type, _) =
    force_ok ~msg:"parse arg ty" @@
    Script_ir_translator.parse_ty context ~allow_big_map:false ~allow_operation:false param_ty_node in
  let (Ex_ty storage_type, _) =
    force_ok ~msg:"parse storage ty" @@
    parse_storage_ty context storage_ty_node in
  let _ = force_ok ~msg:"storage eq" @@ Script_ir_translator.ty_eq context storage_type claimed_storage_type in
  let _ = force_ok ~msg:"param eq" @@ Script_ir_translator.ty_eq context param_type claimed_parameter_type in
  let param_type_full = Pair_t ((claimed_parameter_type, None, None),
                                (claimed_storage_type, None, None), None) in
  let ret_type_full =
    Pair_t ((List_t (Operation_t None, None), None, None),
            (claimed_storage_type, None, None), None) in
  parse_returning (Toplevel { storage_type = claimed_storage_type ; param_type = claimed_parameter_type })
    context (param_type_full, None) ret_type_full code_field >>=? fun (code, _) ->
  return {
    param_type = claimed_parameter_type;
    storage_type = claimed_storage_type;
    code ;
  }
