open Lib
open Execution_context
open Types
open Printer
open OUnit

let () = Error_registration.register();

module Environment = struct
  let param_type = union_t (key_hash_t) (pair_t (key_t) (pair_t (mutez_t) (signature_t)))
  let storage_type = map_t (key_hash_key) (mutez_t)
end

let file = "./accounts.tz"

let context = Context.default_context |> Context.init_contracts 10 

let execution_context = 
  {source = 0; payer = 0; self = 0; amount = Tez.zero; 
  parameter = Data_factory.union_left @@ Data_factory.key_hash_of_string "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"; 
  storage = Data_factory.empty_map ~key:key_hash_key
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
    Printer.print_data Environment.storage_type storage 


