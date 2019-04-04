open Lib
open Types
open Execution_context
open OUnit

let () = Error_registration.register ()

module Environment = struct
  let param_type = pair_t int_t int_t
  let storage_type = int_t
end

let file = "./sumtwo.tz"

let execution_context = 
  {source = 0; payer = 0; self = 0; amount = Tez.zero; parameter = (Z.of_int 1, Z.of_int 1); storage = Z.of_int 0}

let context = Context.default_context |> Context.init_contracts 10 

let execute_directly_using_engine = 
  Engine.get_typed_toplevel_and_execute 
    context 
    file 
    execution_context 
    ~param_type:Environment.param_type 
    ~storage_type:Environment.storage_type

let test_execution_produces_correct_list_and_storage _ =
  execute_directly_using_engine |> fun ((operations_list, storage), _) ->
    assert_equal (List.length operations_list) 0;
    assert_equal storage (Z.of_int 2);
    ()

let _ = run_test_tt_main (
  "sumtwo_test_suite" >:::
  [
    "execution_test" >:: test_execution_produces_correct_list_and_storage
  ]
)