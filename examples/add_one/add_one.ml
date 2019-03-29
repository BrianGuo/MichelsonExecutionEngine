open Lib
open Execution_context
open Types
open OUnit

let () = Error_registration.register();

module Environment = struct
  let param_type = list_t int_t
  let storage_type = list_t int_t
end

let file = "./add_one.tz"

let context = Context.default_context |> Context.init_contracts 10 

let execution_context = 
  {
    source = 0; payer = 0; self = 0; amount = Tez.zero; 
    parameter = List.map Z.of_int [1;2;3]; 
    storage = []
  }

let execute_directly_using_engine = 
  Engine.get_typed_toplevel_and_execute
    context
    file
    execution_context
    ~param_type:Environment.param_type
    ~storage_type:Environment.storage_type

let () = execute_directly_using_engine |>
  fun ((operations_list, storage), _) ->
    assert_equal (List.length operations_list) 0;
    assert_equal storage @@ List.map Z.of_int [2;3;4];
    ()