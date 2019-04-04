open Lib
open Types
open Execution_context
open OUnit

let () = Error_registration.register ()

let execute_and_test 
    file param_type storage_type execution_context 
    context storage_result = 
  Engine.get_typed_toplevel_and_execute context file execution_context ~param_type ~storage_type
  |> fun ((_, storage), _) ->
    assert_equal storage storage_result; ()

let execute_failure
    file param_type storage_type execution_context 
    context = 
  assert_raises (Failure  "force_ok : Execution Failed") (fun _ -> Engine.get_typed_toplevel_and_execute context file execution_context ~param_type ~storage_type)
  

let context = Context.default_context |> Context.init_contracts 10

let _ = run_test_tt_main (
  "assert_eq" >:::
  [
    "assert_cmpeq_success" >:: (fun _ -> execute_and_test "assert_cmpeq.tz" (pair_t int_t int_t) (unit_t) (default_typed_execution_context (Z.of_int 1, Z.of_int 1) ()) context () );
    "assert_cmpeq_fail" >:: (fun _ -> execute_failure "assert_cmpeq.tz" (pair_t int_t int_t) (unit_t) (default_typed_execution_context (Z.of_int 5, Z.of_int 1) ()) context );
    "assert_cmpge_test" >:: (fun _ -> execute_and_test "assert_cmpge.tz" (pair_t int_t int_t) (unit_t)  
    (default_typed_execution_context (Z.of_int 2, Z.of_int 1) ()) context ()) ;
    "assert_cmpge_fail" >:: (fun _ -> execute_failure "assert_cmpge.tz" (pair_t int_t int_t) (unit_t) (default_typed_execution_context (Z.of_int 5, Z.of_int 7) ()) context );

  ]
)