open Script_ir_nodes
open Script_ir_translator
open Tezos_error_monad.Error_monad
open Error_registration
open Michelson_v1_primitives
open Tezos_micheline.Micheline
open Tezos_stdlib
open Tezos_data_encoding

let generate_data_from_string (ty : 'a ty) (str : string) = 
  parse_data Context.default_context ty @@ Cast.node_of_string str


let rec parse_data_simplified 
  : type a. ?type_logger:type_logger -> Context.t -> a ty -> Script.node -> a tzresult Lwt.t =
  fun ?type_logger ctxt ty script_data ->
    let error () =
      Lwt.return (serialize_ty_for_error ctxt ty) >>|? fun (ty, _ctxt) ->
      Invalid_constant (location script_data, strip_locations script_data, ty) in
    let traced body =
      trace_eval error body in
    let parse_items ?type_logger loc ctxt expr key_type value_type items item_wrapper =
      fold_left_s
        (fun (last_value, map) item ->
            match item with
            | Prim (_, D_Elt, [ k; v ], _) ->
                parse_comparable_data_simplified ?type_logger ctxt key_type k >>=? fun (k) ->
                parse_data_simplified ?type_logger ctxt value_type v >>=? fun (v) ->
                begin match last_value with
                  | Some value ->
                      if Compare.Int.(0 <= (compare_comparable key_type value k))
                      then
                        if Compare.Int.(0 = (compare_comparable key_type value k))
                        then fail (Duplicate_map_keys (loc, strip_locations expr))
                        else fail (Unordered_map_keys (loc, strip_locations expr))
                      else return_unit
                  | None -> return_unit
                end >>=? fun () ->
                return (Some k, map_update k (Some (item_wrapper v)) map)
            | Prim (loc, D_Elt, l, _) ->
                fail @@ Invalid_arity (loc, D_Elt, 2, List.length l)
            | Prim (loc, name, _, _) ->
                fail @@ Invalid_primitive (loc, [ D_Elt ], name)
            | Int _ | String _ | Bytes _ | Seq _ ->
                error () >>=? fail)
        (None, empty_map key_type) items |> traced >>|? fun (_, items) ->
      (items) in
    match ty, script_data with
    (* Unit *)
    | Unit_t _, _ ->
      return ()
    | Bool_t _, Prim(_, bool, _, _) ->
      begin
      match bool with
      | D_True -> return true
      | _ -> return false
      end
    | Bool_t _, _ ->
      traced (fail (Generic_error))
    (* Strings *)
    | String_t _, String (_, s) ->
        let rec check_printable_ascii i =
          if Compare.Int.(i < 0) then true
          else match String.get s i with
            | '\n' | '\x20'..'\x7E' -> check_printable_ascii (i - 1)
            | _ -> false in
        if check_printable_ascii (String.length s - 1) then
          return s
    else
      error () >>=? fail
    | String_t _, _ ->
      traced (fail (Generic_error))
    (* Byte sequences *)
    | Bytes_t _, Bytes (_, v) ->
      return v
    | Bytes_t _, _ ->
      traced (fail (Generic_error))
    | Int_t _, Int (_, v) ->
      return @@ Script_int.of_zint v
    | Nat_t _, Int (_, v) ->
        let v = Script_int.of_zint v in
        if Compare.Int.(Script_int.compare v Script_int.zero >= 0) then
          return @@ Script_int.abs v
        else
          error () >>=? fail
    | Int_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Int_kind ], kind expr)))
    | Nat_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Int_kind ], kind expr)))
    (* Tez amounts *)
    | Mutez_t _, Int (_, v) ->
        begin try
            match Tez.of_mutez (Z.to_int64 v) with
            | None -> raise Exit
            | Some tez -> return tez
          with _ ->
            error () >>=? fail
        end
    | Mutez_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Int_kind ], kind expr)))
    (* Timestamps *)
    | Timestamp_t _, (Int (_, v)) (* As unparsed with [Optimized] or out of bounds [Readable]. *) ->
        return @@ Script_timestamp.of_zint v
    | Timestamp_t _, String (_, s) (* As unparsed with [Redable]. *) ->
        begin match Script_timestamp.of_string s with
          | Some v -> return v
          | None -> error () >>=? fail
        end
    | Timestamp_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Int_kind ], kind expr)))
    (* IDs *)
    | Key_t _, Bytes (_, bytes) -> (* As unparsed with [Optimized]. *)
        begin match Data_encoding.Binary.of_bytes Signature.Public_key.encoding bytes with
          | Some k -> return k
          | None -> error () >>=? fail
        end
    | Key_t _, String (_, s) -> (* As unparsed with [Readable]. *)
        begin match Signature.Public_key.of_b58check_opt s with
          | Some k -> return k
          | None -> error () >>=? fail
        end
    | Key_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Bytes_kind ], kind expr)))
    | Key_hash_t _, Bytes (_, bytes) -> (* As unparsed with [Optimized]. *)
        begin
          match Data_encoding.Binary.of_bytes Signature.Public_key_hash.encoding bytes with
          | Some k -> return k
          | None -> error () >>=? fail
        end
    | Key_hash_t _, String (_, s) (* As unparsed with [Readable]. *) ->
        begin match Signature.Public_key_hash.of_b58check_opt s with
          | Some k -> return k
          | None -> error () >>=? fail
        end
    | Key_hash_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Bytes_kind ], kind expr)))
    (* Signatures *)
    | Signature_t _, Bytes (_, bytes) (* As unparsed with [Optimized]. *) ->
        begin match Data_encoding.Binary.of_bytes Signature.encoding bytes with
          | Some k -> return k
          | None -> error () >>=? fail
        end
    | Signature_t _, String (_, s) (* As unparsed with [Readable]. *) ->
        begin match Signature.of_b58check_opt s with
          | Some s -> return s
          | None -> error () >>=? fail
        end
    | Signature_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Bytes_kind ], kind expr)))
    (* Operations *)
    | Operation_t _, _ ->
        (* operations cannot appear in parameters or storage,
            the protocol should never parse the bytes of an operation *)
        assert false
    (* Addresses *)
    | Address_t _, Bytes (_, bytes) (* As unparsed with [O[ptimized]. *) ->
        begin
          match Data_encoding.Binary.of_bytes Contract.encoding bytes with
          | Some c -> return c
          | None -> error () >>=? fail
        end
    | Address_t _, String (_, s) (* As unparsed with [Readable]. *) ->
        traced (Lwt.return (Contract.of_b58check s)) >>=? fun c ->
        return c
    | Address_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Bytes_kind ], kind expr)))
    (* Contracts *)
    | Contract_t (ty, _), Bytes (_, bytes) (* As unparsed with [Optimized]. *) ->
        begin
          match Data_encoding.Binary.of_bytes Contract.encoding bytes with
          | Some c ->
              return (ty, c)
          | None -> error () >>=? fail
        end
    | Contract_t (ty, _), String (loc, s) (* As unparsed with [Readable]. *) ->
        traced @@
        Lwt.return (Contract.of_b58check s) >>=? fun c ->
        parse_contract ctxt loc ty c >>=? fun (_, _) ->
        return (ty, c)
    | Contract_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Bytes_kind ], kind expr)))
    (* Pairs *)
    | Pair_t ((ta, _, _), (tb, _, _), _), Prim (_, D_Pair, [ va; vb ], _) ->
        traced @@
        parse_data_simplified ctxt ta va >>=? fun (va) ->
        parse_data_simplified ctxt tb vb >>=? fun (vb) ->
        return (va, vb)
    | Pair_t _, Prim (loc, D_Pair, l, _) ->
        fail @@ Invalid_arity (loc, D_Pair, 2, List.length l)
    | Pair_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Pair ]))
    (* Unions *)
    | Union_t ((tl, _), _, _), Prim (_, D_Left, [ v ], _) ->
        traced @@
        parse_data_simplified ctxt tl v >>=? fun (v) ->
        return (L v)
    | Union_t _, Prim (loc, D_Left, l, _) ->
        fail @@ Invalid_arity (loc, D_Left, 1, List.length l)
    | Union_t (_, (tr, _), _), Prim (_, D_Right, [ v ], _) ->
        traced @@
        parse_data_simplified ctxt tr v >>=? fun (v) ->
        return (R v)
    | Union_t _, Prim (loc, D_Right, l, _) ->
        fail @@ Invalid_arity (loc, D_Right, 1, List.length l)
    | Union_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Left ; D_Right ]))
    (* Lambdas *)
    | Lambda_t (ta, tr, _ty_name), (Seq (_loc, _) as script_instr) ->
        traced @@ parse_returning Lambda ?type_logger ctxt (ta, Some (`Var_annot "@arg")) tr script_instr
        >>=? fun (lambda, _) -> return lambda
    | Lambda_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    (* Options *)
    | Option_t ((t, _), _, _), Prim (_, D_Some, [ v ], _) ->
        traced @@
        parse_data_simplified ?type_logger ctxt t v >>=? fun (v) ->
        return @@ Some v
    | Option_t _, Prim (loc, D_Some, l, _) ->
        fail @@ Invalid_arity (loc, D_Some, 1, List.length l)
    | Option_t (_, _, _), Prim (_, D_None, [], _) ->
        return None
    | Option_t _, Prim (loc, D_None, l, _) ->
        fail @@ Invalid_arity (loc, D_None, 0, List.length l)
    | Option_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Some ; D_None ]))
    (* Lists *)
    | List_t (t, _ty_name), Seq (_loc, items) ->
        traced @@
        fold_right_s
          (fun v (rest) ->
            parse_data_simplified ?type_logger ctxt t v >>=? fun (v) ->
            return ((v :: rest)))
          items ([])
    | List_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    (* Sets *)
    | Set_t (t, _ty_name), (Seq (loc, vs) as expr) ->
        traced @@
        fold_left_s
          (fun (last_value, set) v ->
              parse_comparable_data ?type_logger ctxt t v >>=? fun (v, _) ->
              begin match last_value with
                | Some value ->
                    if Compare.Int.(0 <= (compare_comparable t value v))
                    then
                      if Compare.Int.(0 = (compare_comparable t value v))
                      then fail (Duplicate_set_values (loc, strip_locations expr))
                      else fail (Unordered_set_values (loc, strip_locations expr))
                    else return_unit
                | None -> return_unit
              end >>=? fun () ->
              return (Some v, set_update v true set))
          (None, empty_set t) vs >>|? fun (_, set) ->
        (set)
    | Set_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    (* Maps *)
    | Map_t (tk, tv, _ty_name), (Seq (loc, vs) as expr) ->
        parse_items ?type_logger loc ctxt expr tk tv vs (fun x -> x) >>=? 
        fun (x) -> return x
    | Map_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    | Big_map_t (tk, tv, _ty_name), (Seq (loc, vs) as expr) ->
        parse_items ?type_logger loc ctxt expr tk tv vs (fun x -> Some x) >>|? fun (diff) ->
        ({ diff ; key_type = ty_of_comparable_ty tk ; value_type = tv })
    | Big_map_t (_tk, _tv, _), expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))

and parse_comparable_data_simplified
  : type a.
    ?type_logger:type_logger ->
    Context.t -> a comparable_ty -> Script.node -> a tzresult Lwt.t
  = fun ?type_logger ctxt ty script_data ->
    parse_data_simplified ?type_logger ctxt (ty_of_comparable_ty ty) script_data

type 'a data = ('a ty * 'a)
type ex_data = Ex_data : 'a data -> ex_data

(* TODO: throw error messages instead of returning default null values *)
let rec create_data_node_from_ty
  : type a.
  a ty ->  a -> Script.node  =
  fun ty data -> 
  let create_binding_node = fun tk tv (k,v)  -> 
    let key = create_data_node_from_comparable_ty tk k in
    let value = create_data_node_from_ty tv v in
    Prim (0, D_Elt, [key; value], [])
  in
  match ty with
  | Unit_t _ -> Prim (0, D_Unit, [], [])
  | Bool_t _ -> if data then Prim (0, D_True, [], []) else Prim (0, D_False, [], [])
  | String_t _ -> String (0, data)
  | Bytes_t _ -> Bytes (0, data)
  | Int_t _ -> Int (0, data)
  | Nat_t _ -> Int (0, data)
  | Mutez_t _ -> Int (0, Z.of_int64 @@ Tez.to_mutez data)
  | Timestamp_t _ -> Int (0, data)
  | Key_t _ -> 
    begin 
    match Data_encoding.Binary.to_bytes Signature.Public_key.encoding data with
    | Some k -> Bytes (0, k)
    | None -> raise (Failure "Can't convert key to bytes")
    end
  | Key_hash_t _ ->
    begin
      match Data_encoding.Binary.to_bytes Signature.Public_key_hash.encoding data with
      | Some k -> Bytes (0, k)
      | None -> raise (Failure ("Can't convert key_hash to bytes"))
    end
  | Signature_t _ -> String (0, Signature.to_b58check data)
  | Address_t _ ->
      begin
        match Data_encoding.Binary.to_bytes Contract.encoding data with
        | Some c -> Bytes (0, c)
        | None -> raise (Failure "Can't convert address to bytes")
      end
  | Contract_t _ ->
      begin
        match Data_encoding.Binary.to_bytes Contract.encoding (snd data) with
        | Some c -> Bytes (0, c)
        | None -> raise (Failure "Can't convert contract to bytes")
      end
  | Pair_t ((t1, _, _), (t2, _, _), _) -> 
      let first = create_data_node_from_ty t1 (fst data) in
      let second = create_data_node_from_ty t2 (snd data) in
      Prim (0, D_Pair, [first ; second], [])
  | Union_t ((tl, _), (tr, _), _) ->
      begin
      match data with
      | L v -> 
        let v_node = create_data_node_from_ty tl v in
         Prim (0, D_Left, [v_node], [])
      | R v -> 
        let v_node = create_data_node_from_ty tr v in
          Prim (0, D_Right, [v_node], [])
      end
  | Lambda_t _ ->
      (* TODO: unit test this *)
      let Lam (descr, _) = data in
      Cast.descr_to_node descr
  | Option_t ((t, _), _, _) ->
      begin
      match data with
      | Some v -> 
        let v_node = create_data_node_from_ty t v in 
          Prim (0, D_Some, [v_node], [])
      | None -> Prim (0, D_None, [], [])
      end
  | List_t (t, _) ->
      let lst = List.map (fun v -> create_data_node_from_ty t v) data in
      Seq (0, lst)
  | Set_t (t, _) ->
      let module V = (val data) in
      let lst = List.map (fun x -> create_data_node_from_comparable_ty t x) (V.OPS.elements V.boxed) in
      Seq (0, lst)
  | Map_t (tk, tv, _) -> 
      let module V = (val data) in
      let lst = List.map (create_binding_node tk tv) (V.OPS.bindings @@ fst V.boxed) in
      Seq (0, lst)
  | Big_map_t (tk, tv, _) -> 
      let module V = (val data.diff) in 
      let lst = List.fold_left (fun acc (k, v) -> 
          match v with
          | None -> acc
          | Some v' ->
              let key = create_data_node_from_comparable_ty tk k in
              let value = create_data_node_from_ty tv v' in
              Prim (0, D_Elt, [key; value], []) :: acc
      ) [] (V.OPS.bindings @@ fst V.boxed) in
      Seq (0, lst)
  | _ -> Prim (0, D_Unit, [], [])

and create_data_node_from_comparable_ty
      : type a.
      a comparable_ty -> a -> Script.node
    = fun ty data ->
      create_data_node_from_ty (ty_of_comparable_ty ty) data


let lambda_of_descr :
  type bef aft.
  (bef * end_of_stack, aft * end_of_stack) descr -> (bef, aft) lambda =
  fun code -> 
      Lam (code, strip_locations @@ Cast.descr_to_node code)

let descr_of_lambda :
  type bef aft.
  (bef, aft) lambda -> (bef * end_of_stack, aft * end_of_stack) descr =
  fun lam ->
    let Lam (descr, _) = lam in
    descr