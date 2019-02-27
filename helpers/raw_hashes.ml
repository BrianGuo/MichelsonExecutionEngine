open Tezos_stdlib

let blake2b _ = MBytes.make 5 'c'

let sha256 = blake2b
let sha512 = sha256