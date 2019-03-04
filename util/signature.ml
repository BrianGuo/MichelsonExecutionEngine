open Tezos_data_encoding
open Data_encoding
open Tezos_error_monad.Error_monad
open Tezos_stdlib
open Tezos_crypto

type secret_key =
  | Ed25519 of Ed25519.Secret_key.t
  | Secp256k1 of Secp256k1.Secret_key.t
  | P256 of P256.Secret_key.t

type public_key_hash =
  | Ed25519 of Ed25519.Public_key_hash.t
  | Secp256k1 of Secp256k1.Public_key_hash.t
  | P256 of P256.Public_key_hash.t

type public_key =
  | Ed25519 of Ed25519.Public_key.t
  | Secp256k1 of Secp256k1.Public_key.t
  | P256 of P256.Public_key.t

type watermark =
  | Block_header of string
  | Endorsement of string
  | Generic_operation
  | Custom of MBytes.t

module Public_key = struct
  type t = public_key =
  | Ed25519 of Ed25519.Public_key.t
  | Secp256k1 of Secp256k1.Public_key.t
  | P256 of P256.Public_key.t

  let name = "Signature.Public_key"
  let title = "A Ed25519, Secp256k1, or P256 public key"

  let of_b58check_opt s : (public_key option) =
    match Base58.decode s with
    | Some (Ed25519.Public_key.Data public_key) -> Some (Ed25519 public_key)
    | Some (P256.Public_key.Data public_key) -> Some (P256 public_key)
    | _ -> None

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Pervasives.failwith "Unexpected data (%s)" name

  let to_b58check (h : public_key) =
    match h with
    | Ed25519 pk -> Ed25519.Public_key.to_b58check pk
    | P256 pk -> P256.Public_key.to_b58check pk
    | _ -> ""

  let raw_encoding =
      def "public_key" ~description:"A Ed25519, Secp256k1, or P256 public key" @@
        union [
          case (Tag 0) Ed25519.Public_key.encoding
            ~title:"Ed25519"
            (fun (x : public_key) ->
              match x with
              | Ed25519 x -> Some x
              | _ -> None)
            (function x -> Ed25519 x);
          case (Tag 1) Secp256k1.Public_key.encoding
            ~title:"Secp256k1"
            (fun (x : public_key) ->
              match x with
              | Secp256k1 x -> Some x
              | _ -> None)
            (function x -> Secp256k1 x) ;
          case
            ~title:"P256" (Tag 2) P256.Public_key.encoding
            (fun (x : public_key) ->
              match x with
              | P256 x -> Some x
              | _ -> None)
            (function x -> P256 x)
        ]

  let encoding : (public_key Data_encoding.t) =
    splitted
      ~binary:
        (obj1 (req name raw_encoding))
      ~json:
        (def name
           ~title: (title ^ " (Base58Check-encoded)") @@
         conv
           to_b58check
           (Data_encoding.Json.wrap_error of_b58check_exn)
           string)


  let hash (pk : public_key) : public_key_hash =
    match pk with
    | Ed25519 pk -> Ed25519 (Ed25519.Public_key.hash pk)
    | P256 pk -> P256 (P256.Public_key.hash pk)
    | Secp256k1 pk -> Secp256k1 (Secp256k1.Public_key.hash pk)
end

module Public_key_hash = struct

  type t = public_key_hash =
  | Ed25519 of Ed25519.Public_key_hash.t
  | Secp256k1 of Secp256k1.Public_key_hash.t
  | P256 of P256.Public_key_hash.t

  let title = "A Ed25519, Secp256k1, or P256 public key hash"

  let encoding : (public_key_hash Data_encoding.t) =
    def "public_key_hash" ~description:"A Ed25519, Secp256k1, or P256 public key hash" @@
    union [
      case (Tag 0) Ed25519.Public_key_hash.encoding
        ~title:"Ed25519"
        (fun (x : public_key_hash) ->
              match x with
              | Ed25519 x -> Some x
              | _ -> None)
        (function x -> Ed25519 x);
      case (Tag 1) Secp256k1.Public_key_hash.encoding
        ~title:"Secp256k1"
        (fun (x : public_key_hash) ->
              match x with
              | Secp256k1 x -> Some x
              | _ -> None)
        (function x -> Secp256k1 x) ;
      case (Tag 2)
        ~title:"P256" P256.Public_key_hash.encoding
        (fun (x : public_key_hash) ->
              match x with
              | P256 x -> Some x
              | _ -> None)
        (function x -> P256 x)
    ]

  let of_b58check_opt s : public_key_hash option =
    match Base58.decode s with
    | Some Ed25519.Public_key_hash.Data pkh -> Some (Ed25519 pkh)
    | Some P256.Public_key_hash.Data pkh -> Some (P256 pkh)
    | Some Secp256k1.Public_key_hash.Data pkh -> Some (Secp256k1 pkh)
    | _ -> None

  let to_b58check (h: public_key_hash) =
    match h with
    | Ed25519 pkh -> Ed25519.Public_key_hash.to_b58check pkh
    | P256 pkh -> P256.Public_key_hash.to_b58check pkh
    | Secp256k1 pkh -> Secp256k1.Public_key_hash.to_b58check pkh

  include Compare.Make(struct
      type nonrec t = t
      let compare a b =
        match ((a : public_key_hash), (b : public_key_hash)) with
        | Ed25519 x, Ed25519 y ->
            Ed25519.Public_key_hash.compare x y
        | Secp256k1 x, Secp256k1 y ->
            Secp256k1.Public_key_hash.compare x y
        | P256 x, P256 y ->
            P256.Public_key_hash.compare x y
        | _ -> Pervasives.compare a b
    end)
  
  let raw_encoding =
    let open Data_encoding in
    def "public_key_hash" ~description:title @@
    union [
      case (Tag 0) Ed25519.Public_key_hash.encoding
        ~title:"Ed25519"
        (function Ed25519 x -> Some x | _ -> None)
        (function x -> Ed25519 x);
      case (Tag 1) Secp256k1.Public_key_hash.encoding
        ~title:"Secp256k1"
        (function Secp256k1 x -> Some x | _ -> None)
        (function x -> Secp256k1 x) ;
      case (Tag 2)
        ~title:"P256" P256.Public_key_hash.encoding
        (function P256 x -> Some x | _ -> None)
        (function x -> P256 x)
    ]
  let to_bytes s =
    Data_encoding.Binary.to_bytes_exn raw_encoding s
  let to_string s = MBytes.to_string (to_bytes s)

end

module Secret_key = struct

  type t = secret_key =
    | Ed25519 of Ed25519.Secret_key.t
    | Secp256k1 of Secp256k1.Secret_key.t
    | P256 of P256.Secret_key.t

  let name = "Signature.Secret_key"
  let title = "A Ed25519, Secp256k1 or P256 secret key"

  let to_public_key = function
    | Ed25519 sk -> Public_key.Ed25519 (Ed25519.Secret_key.to_public_key sk)
    | Secp256k1 sk -> Public_key.Secp256k1 (Secp256k1.Secret_key.to_public_key sk)
    | P256 sk -> Public_key.P256 (P256.Secret_key.to_public_key sk)

  include Compare.Make(struct
      type nonrec t = t
      let compare a b = match (a, b) with
        | Ed25519 x, Ed25519 y -> Ed25519.Secret_key.compare x y
        | Secp256k1 x, Secp256k1 y -> Secp256k1.Secret_key.compare x y
        | P256 x, P256 y -> P256.Secret_key.compare x y
        | _ -> Pervasives.compare a b
    end)

  type Base58.data += Data of t (* unused *)
  let b58check_encoding = (* unused *)
    Base58.register_encoding
      ~prefix: "\255\255"
      ~length: 2
      ~to_raw: (fun _ -> assert false)
      ~of_raw: (fun _ -> assert false)
      ~wrap: (fun x -> Data x)

  let of_b58check_opt b =
    match Base58.decode b with
    | Some (Ed25519.Secret_key.Data sk) -> Some (Ed25519 sk)
    | Some (Secp256k1.Secret_key.Data sk) -> Some (Secp256k1 sk)
    | Some (P256.Secret_key.Data sk) -> Some (P256 sk)
    | _ -> None

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Pervasives.failwith "Unexpected data (%s)" name
  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        generic_error
          "Failed to read a b58check_encoding data (%s): %S"
          name s

  let to_b58check = function
    | Ed25519 sk -> Ed25519.Secret_key.to_b58check sk
    | Secp256k1 sk -> Secp256k1.Secret_key.to_b58check sk
    | P256 sk -> P256.Secret_key.to_b58check sk

  let to_short_b58check = function
    | Ed25519 sk -> Ed25519.Secret_key.to_short_b58check sk
    | Secp256k1 sk -> Secp256k1.Secret_key.to_short_b58check sk
    | P256 sk -> P256.Secret_key.to_short_b58check sk

  include Helpers.MakeEncoder(struct
      type nonrec t = t
      let name = name
      let title = title
      let raw_encoding =
        let open Data_encoding in
        def "secret_key" ~description:title @@
        union [
          case (Tag 0) Ed25519.Secret_key.encoding
            ~title:"Ed25519"
            (function Ed25519 x -> Some x | _ -> None)
            (function x -> Ed25519 x);
          case (Tag 1) Secp256k1.Secret_key.encoding
            ~title:"Secp256k1"
            (function Secp256k1 x -> Some x | _ -> None)
            (function x -> Secp256k1 x) ;
          case (Tag 2)
            ~title:"P256" P256.Secret_key.encoding
            (function P256 x -> Some x | _ -> None)
            (function x -> P256 x)
        ]
      let of_b58check = of_b58check
      let of_b58check_opt = of_b58check_opt
      let of_b58check_exn = of_b58check_exn
      let to_b58check = to_b58check
      let to_short_b58check = to_short_b58check
    end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

end

type t =
  | Ed25519 of Ed25519.t
  | Secp256k1 of Secp256k1.t
  | P256 of P256.t
  | Unknown of MBytes.t

let to_bytes = function
| Ed25519 b -> Ed25519.to_bytes b
| Secp256k1 b -> Secp256k1.to_bytes b
| P256 b -> P256.to_bytes b
| Unknown b -> b

let name = "Signature"
let title = "A Ed25519, Secp256k1 or P256 signature"

type Base58.data += Data of t

let size =
  assert (Ed25519.size = Secp256k1.size && Secp256k1.size = P256.size) ;
  Ed25519.size

let of_bytes_opt s =
  if MBytes.length s = size then Some (Unknown s) else None

let to_string s = MBytes.to_string (to_bytes s)
let of_string_opt s = of_bytes_opt (MBytes.of_string s)

let b58check_encoding =
  Base58.register_encoding
    ~prefix: Base58.Prefix.generic_signature
    ~length: Ed25519.size
    ~to_raw: to_string
    ~of_raw: of_string_opt
    ~wrap: (fun x -> Data x)

let of_bytes_opt s =
  if MBytes.length s = size then Some (Unknown s) else None

let to_string s = MBytes.to_string (to_bytes s)
let of_string_opt s = of_bytes_opt (MBytes.of_string s)

let of_b58check_opt s =
  if TzString.has_prefix ~prefix:Ed25519.b58check_encoding.encoded_prefix s then
    Option.map
      (Ed25519.of_b58check_opt s)
      ~f: (fun x -> Ed25519 x)
  else if TzString.has_prefix ~prefix:Secp256k1.b58check_encoding.encoded_prefix s then
    Option.map
      (Secp256k1.of_b58check_opt s)
      ~f: (fun x -> Secp256k1 x)
  else if TzString.has_prefix ~prefix:P256.b58check_encoding.encoded_prefix s then
    Option.map
      (P256.of_b58check_opt s)
      ~f: (fun x -> P256 x)
  else
    Base58.simple_decode b58check_encoding s

include Helpers.MakeRaw(struct
    type nonrec t = t
    let name = name
    let of_bytes_opt = of_bytes_opt
    let of_string_opt = of_string_opt
    let to_string = to_string
  end)

include Compare.Make(struct
    type nonrec t = t
    let compare a b =
      let a = to_bytes a
      and b = to_bytes b in
      MBytes.compare a b
  end)

let of_b58check_exn s =
  match of_b58check_opt s with
  | Some x -> x
  | None -> Format.kasprintf Pervasives.failwith "Unexpected data (%s)" name

let of_b58check s =
  match of_b58check_opt s with
  | Some x -> Ok x
  | None ->
      generic_error
        "Failed to read a b58check_encoding data (%s): %S"
        name s

let to_short_b58check = function
  | Ed25519 b -> Ed25519.to_short_b58check b
  | Secp256k1 b -> Secp256k1.to_short_b58check b
  | P256 b -> P256.to_short_b58check b
  | Unknown b -> Base58.simple_encode b58check_encoding (Unknown b)

let to_b58check = function
  | Ed25519 b -> Ed25519.to_b58check b
  | Secp256k1 b -> Secp256k1.to_b58check b
  | P256 b -> P256.to_b58check b
  | Unknown b -> Base58.simple_encode b58check_encoding (Unknown b)

let bytes_of_watermark = function
  | Block_header chain_id -> MBytes.concat "" [ MBytes.of_string "\x01" ; MBytes.of_string chain_id ]
  | Endorsement chain_id -> MBytes.concat "" [ MBytes.of_string "\x02" ; MBytes.of_string chain_id ]
  | Generic_operation -> MBytes.of_string "\x03"
  | Custom bytes      -> bytes

let check ?watermark public_key signature message =
  let watermark = Option.map ~f:bytes_of_watermark watermark in
  match public_key, signature with
  | Public_key.Ed25519 pk, Unknown signature -> begin
      match Ed25519.of_bytes_opt signature with
      | Some s -> Ed25519.check ?watermark pk s message
      | None -> false
    end
  | Public_key.Secp256k1 pk, Unknown signature -> begin
      match Secp256k1.of_bytes_opt signature with
      | Some s -> Secp256k1.check ?watermark pk s message
      | None -> false
    end
  | Public_key.P256 pk, Unknown signature -> begin
      match P256.of_bytes_opt signature with
      | Some s -> P256.check ?watermark pk s message
      | None -> false
    end
  | Public_key.Ed25519 pk, Ed25519 signature ->
      Ed25519.check ?watermark pk signature message
  | Public_key.Secp256k1 pk, Secp256k1 signature ->
      Secp256k1.check ?watermark pk signature message
  | Public_key.P256 pk, P256 signature ->
      P256.check ?watermark pk signature message
  | _ -> false

include Helpers.MakeEncoder(struct
    type nonrec t = t
    let name = name
    let title = title
    let raw_encoding =
      Data_encoding.conv
        to_bytes
        of_bytes_exn
        (Data_encoding.Fixed.bytes size)
    let of_b58check = of_b58check
    let of_b58check_opt = of_b58check_opt
    let of_b58check_exn = of_b58check_exn
    let to_b58check = to_b58check
    let to_short_b58check = to_short_b58check
  end)

type algo =
  | Ed25519
  | Secp256k1
  | P256

let generate_key ?(algo = Ed25519) ?seed () =
  match algo with
  | Ed25519 ->
      let pkh, pk, sk = Ed25519.generate_key ?seed () in
      (Public_key_hash.Ed25519 pkh,
       Public_key.Ed25519 pk, Secret_key.Ed25519 sk)
  | Secp256k1 ->
      let pkh, pk, sk = Secp256k1.generate_key ?seed () in
      (Public_key_hash.Secp256k1 pkh,
       Public_key.Secp256k1 pk, Secret_key.Secp256k1 sk)
  | P256 ->
      let pkh, pk, sk = P256.generate_key ?seed () in
      (Public_key_hash.P256 pkh,
       Public_key.P256 pk, Secret_key.P256 sk)