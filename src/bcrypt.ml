external hashpass : string -> string -> string = "campfire_bcrypt_hashpass"

let bcrypt_base64 bytes =
  let alphabet = "./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" in
  let output = Buffer.create 24 in
  let length = String.length bytes in
  let rec encode offset =
    if offset < length then (
      let c1 = Char.code bytes.[offset] in
      Buffer.add_char output alphabet.[c1 lsr 2];
      let c1 = (c1 land 0x03) lsl 4 in
      if offset + 1 >= length then Buffer.add_char output alphabet.[c1]
      else (
        let c2 = Char.code bytes.[offset + 1] in
        let c1 = c1 lor (c2 lsr 4) in
        Buffer.add_char output alphabet.[c1];
        let c2 = (c2 land 0x0f) lsl 2 in
        if offset + 2 >= length then Buffer.add_char output alphabet.[c2]
        else (
          let c3 = Char.code bytes.[offset + 2] in
          Buffer.add_char output alphabet.[c2 lor (c3 lsr 6)];
          Buffer.add_char output alphabet.[c3 land 0x3f]);
        encode (offset + 3)))
  in
  encode 0;
  Buffer.contents output

let hash password =
  if String.length password > 72 then invalid_arg "bcrypt password exceeds 72 bytes";
  let salt = "$2b$12$" ^ bcrypt_base64 (Rails_crypto.random_bytes 16) in
  hashpass password salt

let valid_hash hash =
  String.length hash = 60
  && (String.starts_with ~prefix:"$2a$" hash
     || String.starts_with ~prefix:"$2b$" hash
     || String.starts_with ~prefix:"$2y$" hash)
  && hash.[6] = '$'
  && (let cost = int_of_string_opt (String.sub hash 4 2) in
      match cost with Some cost -> cost >= 4 && cost <= 31 | None -> false)

let constant_time_equal left right =
  let difference = ref 0 in
  for index = 0 to String.length left - 1 do
    difference :=
      !difference lor (Char.code left.[index] lxor Char.code right.[index])
  done;
  !difference = 0

let verify ~hash password =
  if not (valid_hash hash) || String.length password > 72 then false
  else
    try
      let normalized =
        if String.starts_with ~prefix:"$2y$" hash then "$2b$" ^ String.sub hash 4 56
        else hash
      in
      let salt = String.sub normalized 0 29 in
      constant_time_equal normalized (hashpass password salt)
    with Invalid_argument _ | Failure _ -> false
