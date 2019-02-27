# micheltest

Testing framework for michelson smart-contracts using OCaml.

Todos:

- External contracts currently not callable
    - make/include a mocking framework for these objects
- Assert classes not yet ported over
- Have not yet verified that signatures/hashing works as intended.
    - Signatures now follow the native client's signature scheme (supposedly)
- Stack trace not pretty printed in a desirable way.
- Build out API Docs
- File structure reorganization
- build guide (add to this readme?)
- include license in comments on all files
- include mli files for most interfaces