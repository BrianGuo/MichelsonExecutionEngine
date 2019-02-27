open Big_int

type t = (Account.t * big_int) list

let genesis accounts =
  accounts