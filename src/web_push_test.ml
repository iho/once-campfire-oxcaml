let base64url = function
  | value ->
      match Rails_crypto.base64_decode value with
      | Some decoded -> decoded
      | None -> failwith "invalid RFC 8291 test vector"

let () =
  let server_private =
    base64url "yfWPiYE-n46HLnH0KqZOF1fJJU3MYrct3AELtAQ-oRw"
  in
  let server_public =
    base64url
      "BP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A8"
  in
  let user_public =
    base64url
      "BCVxsr7N_eNgVRqvHtD0zTZsEc6-VV-JvLexhqUzORcxaOzi6-AYWXvTBHm4bjyPjs7Vd8pZGH6SRpkNtoIAiw4"
  in
  let auth_secret = base64url "BTBZMqHH6r4Tts7J_aSIgg" in
  let salt = base64url "DGv6ra1nlYgDCS1FRnbzlw" in
  let shared_secret = Web_push.ecdh server_private user_public in
  assert (
    Rails_crypto.base64url_encode shared_secret
    = "kyrL1jIIOHEzg3sM2ZWRHDRB62YACZhhSlknJ672kSs");
  assert (Web_push.public_from_private server_private = server_public);
  let signing_input = "independent VAPID ES256 test" in
  let signature = Web_push.sign_vapid server_private signing_input in
  assert (String.length signature = 64);
  assert (Web_push.verify_vapid server_public signing_input signature);
  assert (not (Web_push.verify_vapid server_public (signing_input ^ "!") signature));
  let key, nonce =
    Web_push.derive_keys ~shared_secret ~auth_secret ~user_public
      ~server_public ~salt
  in
  assert (Rails_crypto.base64url_encode key = "oIhVW04MRdy2XN9CiKLxTg");
  assert (Rails_crypto.base64url_encode nonce = "4h_95klXJ5E_qnoN");
  let payload =
    base64url "V2hlbiBJIGdyb3cgdXAsIEkgd2FudCB0byBiZSBhIHdhdGVybWVsb24"
  in
  let encrypted = Web_push.aes128gcm key nonce (payload ^ "\002") in
  assert (
    Rails_crypto.base64url_encode encrypted
    = "8pfeW0KbunFT06SuDKoJH9Ql87S1QUrdirN6GcG7sFz1y1sqLgVi1VhjVkHsUoEsbI_0LpXMuGvnzQ");
  let record = Web_push.encode_record ~user_public ~auth_secret payload in
  assert (String.length record = 86 + String.length payload + 1 + 16);
  assert (String.sub record 16 5 = "\000\000\016\000\065");
  assert (String.get record 21 = '\004');
  let private_key, public_key = Web_push.generate_keypair () in
  assert (String.length private_key = 32 && String.length public_key = 65);
  assert (String.get public_key 0 = '\004');
  assert (String.length (Web_push.ecdh private_key user_public) = 32);
  let token =
    Web_push.vapid_jwt ~private_key:server_private ~public_key:server_public
      ~subject:"mailto:campfire@example.test" ~audience:"https://fcm.googleapis.com"
      ~expiration:1_800_000_000L
  in
  (match String.split_on_char '.' token with
  | [ header; claims; encoded_signature ] ->
      let signing_input = header ^ "." ^ claims in
      let signature = base64url encoded_signature in
      assert (Web_push.verify_vapid server_public signing_input signature);
      let decoded_header = Yojson.Basic.from_string (base64url header) in
      let decoded_claims = Yojson.Basic.from_string (base64url claims) in
      assert (Yojson.Basic.Util.member "alg" decoded_header = `String "ES256");
      assert (Yojson.Basic.Util.member "aud" decoded_claims = `String "https://fcm.googleapis.com");
      assert (Yojson.Basic.Util.member "exp" decoded_claims = `Int 1_800_000_000);
      assert (Yojson.Basic.Util.member "sub" decoded_claims = `String "mailto:campfire@example.test")
  | _ -> failwith "VAPID JWT did not have three compact parts");
  let authorization =
    Web_push.authorization_header
      ~private_key:(Rails_crypto.base64url_encode server_private)
      ~public_key:(Rails_crypto.base64url_encode server_public)
      ~subject:"mailto:campfire@example.test" ~audience:"https://fcm.googleapis.com"
      ~now:1_700_000_000L
  in
  match String.split_on_char ',' authorization with
  | [ token_field; key_field ] ->
      assert (String.starts_with ~prefix:"vapid t=" token_field);
      assert
        (String.trim key_field
        = "k=" ^ Rails_crypto.base64url_encode server_public);
      let token = String.sub token_field 8 (String.length token_field - 8) in
      (match String.split_on_char '.' token with
      | [ header; claims; signature ] ->
          let signing_input = header ^ "." ^ claims in
          assert
            (Web_push.verify_vapid server_public signing_input
               (base64url signature))
      | _ -> failwith "authorization header contains an invalid VAPID token")
  | _ -> failwith "invalid VAPID authorization header"
