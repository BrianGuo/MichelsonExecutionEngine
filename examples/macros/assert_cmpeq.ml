open Tezos_execution_engine
open Types
open Execution_context
open Data_factory
open OUnit

let () = Error_registration.register ()

let execute_and_test 
    ?cmp:(def=(fun x y -> x = y)) file param_type storage_type execution_context 
    context storage_result = 
  Engine.get_typed_toplevel_and_execute context file execution_context ~param_type ~storage_type
  |> fun ((_, storage), _) ->
    assert_equal ~cmp:def storage storage_result; ()

let execute_failure
    file param_type storage_type execution_context 
    context = 
  assert_raises (Failure  "force_ok : Execution Failed") (fun _ -> Engine.get_typed_toplevel_and_execute context file execution_context ~param_type ~storage_type)
  

let context = Context.default_context |> Context.init_contracts 10

let _ = run_test_tt_main (
  "assert_eq" >:::
  [
    "assert_cmpeq_success" >:: (fun _ -> execute_and_test "assert_cmpeq.tz" (pair_t int_t int_t) (unit_t) (default_typed_execution_context (Z.of_int 1, Z.of_int 1) ()) context () );
    (* TODO: better failure testing - should expect a specific type of failure *)
    "assert_cmpeq_fail" >:: (fun _ -> execute_failure "assert_cmpeq.tz" (pair_t int_t int_t) (unit_t) (default_typed_execution_context (Z.of_int 5, Z.of_int 1) ()) context );
    "assert_cmpge_test" >:: (fun _ -> execute_and_test "assert_cmpge.tz" (pair_t int_t int_t) (unit_t)  
    (default_typed_execution_context (Z.of_int 2, Z.of_int 1) ()) context ()) ;
    "assert_cmpge_fail" >:: (fun _ -> execute_failure "assert_cmpge.tz" (pair_t int_t int_t) (unit_t) (default_typed_execution_context (Z.of_int 5, Z.of_int 7) ()) context );
    "assert_cmple_test" >:: (fun _ -> execute_and_test "assert_cmple.tz" (pair_t int_t int_t) (unit_t) (default_typed_execution_context (Z.of_int 5, Z.of_int 7) ()) context ());
    "big_map_get_add_test" >:: (fun _ -> execute_and_test "big_map_get_add.tz" ~cmp:(fun _ _ -> true) (pair_t (pair_t int_t (option_t int_t)) (pair_t int_t (option_t int_t))) (pair_t (big_map_t int_key int_t) unit_t) (default_typed_execution_context ((Z.of_int 5, Some (Z.of_int 2)), (Z.of_int 1, Some (Z.of_int 1))) (empty_big_map int_key int_t, ())) context (empty_big_map int_key int_t, ())) ;
    "big_map_mem_test" >:: (fun _ -> execute_and_test
    "big_map_mem.tz" ~cmp:(fun _ _ -> true) (pair_t int_t bool_t) (pair_t (big_map_t int_key unit_t) unit_t) (default_typed_execution_context (Z.of_int 5, false)(empty_big_map int_key unit_t, ())) context (empty_big_map int_key unit_t, ())) ;
    "take_my_money_test" >:: (fun _ -> execute_and_test "take_my_money.tz" (key_hash_t) (unit_t) (default_typed_execution_context (key_hash_of_string "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU") ()) context ());
  ]
)