open Big_int

type t = {
  pkh : Signature.public_key_hash;
  pk :  Signature.public_key;
  sk :  Signature.secret_key ;
}

let range i j =
  let rec loop acc j =
    if j < i then acc else loop (j :: acc) (pred j) in
  loop [] j

let generate_accounts n : (t * big_int) list =
  let amount = big_int_of_string "+4000000000000" in
  List.map (fun _ ->
      let (pkh, pk, sk) = Signature.generate_key () in
      let account = { pkh ; pk ; sk } in
      account, amount)
    (range 0 (n-1))

