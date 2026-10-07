external hashpass : string -> string -> string = "campfire_bcrypt_hashpass"

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
