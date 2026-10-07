external pbkdf2_sha256 : string -> string -> int -> int -> string
  = "campfire_pbkdf2_sha256"

external hmac : int -> string -> string -> string = "campfire_hmac"
external random_bytes : int -> string = "campfire_random_bytes"
external aes_256_gcm_encrypt : string -> string -> string -> string
  = "campfire_aes_256_gcm_encrypt"

external aes_256_gcm_decrypt : string -> string -> string -> string option
  = "campfire_aes_256_gcm_decrypt"

let base64_alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64_encode input =
  let length = String.length input in
  let output = Bytes.create (((length + 2) / 3) * 4) in
  let get index = Char.code input.[index] in
  let rec loop index out =
    if index < length then (
      let remaining = length - index in
      let a = get index in
      let b = if remaining > 1 then get (index + 1) else 0 in
      let c = if remaining > 2 then get (index + 2) else 0 in
      Bytes.set output out base64_alphabet.[a lsr 2];
      Bytes.set output (out + 1)
        base64_alphabet.[((a land 3) lsl 4) lor (b lsr 4)];
      Bytes.set output (out + 2)
        (if remaining > 1 then
           base64_alphabet.[((b land 15) lsl 2) lor (c lsr 6)]
         else '=');
      Bytes.set output (out + 3)
        (if remaining > 2 then base64_alphabet.[c land 63] else '=');
      loop (index + 3) (out + 4))
  in
  loop 0 0;
  Bytes.unsafe_to_string output

let base64_decode input =
  let input =
    String.map (function '-' -> '+' | '_' -> '/' | character -> character) input
  in
  let remainder = String.length input mod 4 in
  let input =
    if remainder = 0 then input
    else if remainder = 2 then input ^ "=="
    else if remainder = 3 then input ^ "="
    else input
  in
  let length = String.length input in
  let digit character =
    try Some (String.index base64_alphabet character)
    with Not_found -> None
  in
  if length mod 4 <> 0 then None
  else
    let output = Buffer.create ((length / 4) * 3) in
    let rec loop index =
      if index = length then Some (Buffer.contents output)
      else
        match (digit input.[index], digit input.[index + 1]) with
        | Some a, Some b ->
            let last = index + 4 = length in
            let c, d = input.[index + 2], input.[index + 3] in
            let emit_first () = Buffer.add_char output (Char.chr ((a lsl 2) lor (b lsr 4))) in
            (match (digit c, digit d) with
            | Some c, Some d ->
                emit_first ();
                Buffer.add_char output
                  (Char.chr (((b land 15) lsl 4) lor (c lsr 2)));
                Buffer.add_char output
                  (Char.chr (((c land 3) lsl 6) lor d));
                loop (index + 4)
            | Some c, None when d = '=' && last ->
                emit_first ();
                Buffer.add_char output
                  (Char.chr (((b land 15) lsl 4) lor (c lsr 2)));
                loop (index + 4)
            | None, None when c = '=' && d = '=' && last ->
                emit_first ();
                loop (index + 4)
            | _ -> None)
        | _ -> None
    in
    loop 0

let base64url_encode input =
  base64_encode input
  |> String.map (function '+' -> '-' | '/' -> '_' | character -> character)
  |> String.split_on_char '=' |> List.hd

let percent_decode input =
  let length = String.length input in
  let hex character =
    match character with
    | '0' .. '9' -> Char.code character - Char.code '0'
    | 'a' .. 'f' -> Char.code character - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code character - Char.code 'A' + 10
    | _ -> -1
  in
  let output = Buffer.create length in
  let rec loop index =
    if index = length then Some (Buffer.contents output)
    else if input.[index] <> '%' then (
      Buffer.add_char output input.[index];
      loop (index + 1))
    else if index + 2 >= length then None
    else
      let high, low = hex input.[index + 1], hex input.[index + 2] in
      if high < 0 || low < 0 then None
      else (
        Buffer.add_char output (Char.chr ((high lsl 4) lor low));
        loop (index + 3))
  in
  loop 0

let json_encode value =
  Yojson.Basic.to_string value
  |> fun json ->
  let output = Buffer.create (String.length json) in
  String.iter
    (fun character ->
      Buffer.add_string output
        (match character with
        | '<' -> "\\u003c"
        | '>' -> "\\u003e"
        | '&' -> "\\u0026"
        | _ -> String.make 1 character))
    json;
  Buffer.contents output

let derive_key secret salt length = pbkdf2_sha256 secret salt 1000 length

let hex input =
  let output = Bytes.create (String.length input * 2) in
  let digits = "0123456789abcdef" in
  String.iteri
    (fun index character ->
      let byte = Char.code character in
      Bytes.set output (index * 2) digits.[byte lsr 4];
      Bytes.set output ((index * 2) + 1) digits.[byte land 15])
    input;
  Bytes.unsafe_to_string output

let constant_time_equal left right =
  let left_length, right_length = String.length left, String.length right in
  let difference = ref (left_length lxor right_length) in
  let maximum = max left_length right_length in
  for index = 0 to maximum - 1 do
    let left_byte = if index < left_length then Char.code left.[index] else 0 in
    let right_byte = if index < right_length then Char.code right.[index] else 0 in
    difference := !difference lor (left_byte lxor right_byte)
  done;
  !difference = 0

let hmac_hex secret salt data =
  secret |> fun key -> derive_key key salt 64 |> fun key -> hmac 1 key data |> hex

let iso_time_at timestamp =
  let tm = Unix.gmtime timestamp in
  let millis = int_of_float ((timestamp -. floor timestamp) *. 1000.) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour
    tm.Unix.tm_min tm.Unix.tm_sec millis

let utc_now () = iso_time_at (Unix.gettimeofday ())

let cookie_expiration () =
  let timestamp = Unix.gettimeofday () +. (20. *. 365. *. 24. *. 60. *. 60.) in
  let tm = Unix.gmtime timestamp in
  let weekdays = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  let months =
    [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
  in
  ( iso_time_at timestamp,
    Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
      weekdays.(tm.Unix.tm_wday) tm.Unix.tm_mday months.(tm.Unix.tm_mon)
      (tm.Unix.tm_year + 1900) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec )

let cookie_envelope name value expires_at =
  `Assoc
    [ ( "_rails",
        `Assoc
          [ ("message", `String (json_encode value |> base64_encode));
            ("exp", Option.fold ~none:`Null ~some:(fun text -> `String text) expires_at);
            ("pur", `String ("cookie." ^ name)) ] ) ]
  |> json_encode

let signing_mac secret payload = hmac_hex secret "signed cookie" payload

let sign_cookie ~secret ~name ?expires_at value =
  let payload = cookie_envelope name value expires_at |> base64_encode in
  payload ^ "--" ^ signing_mac secret payload

let split_signature raw =
  match String.rindex_opt raw '-' with
  | None -> None
  | Some last when last > 0 && raw.[last - 1] = '-' ->
      Some (String.sub raw 0 (last - 1), String.sub raw (last + 1) (String.length raw - last - 1))
  | _ -> None

let decode_envelope ~name raw =
  match Yojson.Basic.from_string raw with
  | `Assoc [ ("_rails", `Assoc fields) ] ->
      let field key = List.assoc_opt key fields in
      (match (field "message", field "pur", field "exp") with
      | Some (`String message), Some (`String purpose), expiry
        when purpose = "cookie." ^ name ->
          let expiry_text =
            match expiry with Some (`String text) -> Some text | _ -> None
          in
          if
            (match expiry_text with
            | Some expiry -> expiry <= utc_now ()
            | None -> false)
          then None
          else
            Option.bind (base64_decode message) (fun json ->
                try Some (Yojson.Basic.from_string json) with _ -> None)
      | _ -> None)
  | _ -> None

let verify_cookie ~secret ~name raw =
  Option.bind (percent_decode raw) (fun raw ->
      Option.bind (split_signature raw) (fun (payload, signature) ->
          let expected = signing_mac secret payload in
          if not (constant_time_equal signature expected) then None
          else
            Option.bind (base64_decode payload) (fun envelope ->
                decode_envelope ~name envelope)))

let encrypt_cookie ~secret ~name ?expires_at ?nonce value =
  let nonce = Option.value nonce ~default:(random_bytes 12) in
  if String.length nonce <> 12 then invalid_arg "Rails cookie nonce must be 12 bytes";
  let key = derive_key secret "authenticated encrypted cookie" 32 in
  let cipher_and_tag =
    aes_256_gcm_encrypt key nonce (cookie_envelope name value expires_at)
  in
  let cipher_length = String.length cipher_and_tag - 16 in
  let cipher = String.sub cipher_and_tag 0 cipher_length in
  let tag = String.sub cipher_and_tag cipher_length 16 in
  String.concat "--" (List.map base64_encode [ cipher; nonce; tag ])

let decrypt_cookie ~secret ~name raw =
  Option.bind (percent_decode raw) (fun raw ->
      match String.split_on_char '-' raw with
      | [ cipher; ""; nonce; ""; tag ] ->
          Option.bind (base64_decode cipher) (fun cipher ->
              Option.bind (base64_decode nonce) (fun nonce ->
                  Option.bind (base64_decode tag) (fun tag ->
                      if String.length nonce <> 12 || String.length tag <> 16 then None
                      else
                        let key = derive_key secret "authenticated encrypted cookie" 32 in
                        Option.bind
                          (aes_256_gcm_decrypt key nonce (cipher ^ tag))
                          (decode_envelope ~name))))
      | _ -> None)
