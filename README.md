# Michelson Execution Engine

Execution Engine for michelson smart-contracts using OCaml.

Todos:

- External contracts currently not callable
    - make/include a mocking framework for these objects
- Assert classes not yet ported over
    - assert class added, not yet tested
- Have not yet verified that signatures/hashing works as intended.
    - Signatures now follow the native client's signature scheme (supposedly)
- Gas counts not properly accounted for
    - Gas counts now properly counted
- Stack trace not pretty printed in a desirable way.
    - Currently in the process of making stack traces print meaningful messages
- Build out API Docs
- File structure reorganization
- build guide (add to this readme?)
- include license in comments on all files
- include mli files for most interfaces
- Bigmap diff taken out of execution (I think this implies that storage to maps doesn't work)
    - big maps included in execution
- remove printing of errors when they are exepcted for tests. 
## Optional Desirables

- Converting ocaml arguments to parameters
- Easier way to pass in arguments instead of current verbose way
    - desirable way is "with \_" function (i.e. with_self self, with_currency currency)
    - Context easily modified in this manner

- Design choices:
    - Dip executed as one instruction and not stepped into. 