external generate_keypair : unit -> string * string = "campfire_web_push_generate_keypair"
external ecdh : string -> string -> string = "campfire_web_push_ecdh"
external aes128gcm : string -> string -> string -> string = "campfire_web_push_aes128gcm"
external sign_vapid : string -> string -> string = "campfire_web_push_vapid_sign"
external verify_vapid : string -> string -> string -> bool = "campfire_web_push_vapid_verify"
external public_from_private : string -> string = "campfire_web_push_public_from_private"

let hmac key data = Rails_crypto.hmac 2 key data

let hkdf_expand prk info length =
  if length < 0 || length > 32 then invalid_arg "Web Push HKDF length";
  String.sub (hmac prk (info ^ "\001")) 0 length

let derive_keys ~shared_secret ~auth_secret ~user_public ~server_public ~salt =
  let prk_key = hmac auth_secret shared_secret in
  let info = "WebPush: info\000" ^ user_public ^ server_public in
  let input_key_material = hkdf_expand prk_key info 32 in
  let prk = hmac salt input_key_material in
  ( hkdf_expand prk "Content-Encoding: aes128gcm\000" 16,
    hkdf_expand prk "Content-Encoding: nonce\000" 12 )

let encode_record ~user_public ~auth_secret payload =
  if String.length user_public <> 65 || String.length auth_secret <> 16 then
    invalid_arg "invalid Web Push subscription keys";
  if String.length payload > 3993 then invalid_arg "Web Push payload exceeds 3993 bytes";
  let server_private, server_public = generate_keypair () in
  let salt = Rails_crypto.random_bytes 16 in
  let shared_secret = ecdh server_private user_public in
  let key, nonce =
    derive_keys ~shared_secret ~auth_secret ~user_public ~server_public ~salt
  in
  let plaintext = payload ^ "\002" in
  let encrypted = aes128gcm key nonce plaintext in
  let record_size = "\000\000\016\000" in
  salt ^ record_size ^ "\065" ^ server_public ^ encrypted

let decode_key encoded =
  match Rails_crypto.base64_decode encoded with
  | Some decoded -> decoded
  | None -> invalid_arg "invalid VAPID base64url key"

let vapid_jwt ~private_key ~public_key ~subject ~audience ~expiration =
  if expiration <= 0L then invalid_arg "invalid VAPID expiration";
  if String.length private_key <> 32 || String.length public_key <> 65 then
    invalid_arg "invalid VAPID key length";
  if public_from_private private_key <> public_key then
    invalid_arg "VAPID public and private keys do not match";
  let header = `Assoc [ ("typ", `String "JWT"); ("alg", `String "ES256") ] in
  let claims =
    `Assoc
      [ ("aud", `String audience);
        ("exp", `Int (Int64.to_int expiration));
        ("sub", `String subject) ]
  in
  let encoded_header =
    Rails_crypto.base64url_encode (Yojson.Basic.to_string header)
  in
  let encoded_claims =
    Rails_crypto.base64url_encode (Yojson.Basic.to_string claims)
  in
  let signing_input = encoded_header ^ "." ^ encoded_claims in
  signing_input ^ "."
  ^ Rails_crypto.base64url_encode (sign_vapid private_key signing_input)

let authorization_header ~private_key ~public_key ~subject ~audience ~now =
  let private_key = decode_key private_key in
  let public_key = decode_key public_key in
  let jwt =
    vapid_jwt ~private_key ~public_key ~subject ~audience
      ~expiration:(Int64.add now 43_200L)
  in
  "vapid t=" ^ jwt ^ ", k=" ^ Rails_crypto.base64url_encode public_key
