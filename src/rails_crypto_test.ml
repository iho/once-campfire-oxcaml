let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

let secret =
  "5335c3b1ad35b4ad170c3413bd651ef3b6ed64e257261871a6de3f978cf3868ee417a927040935fb30b0f7debdedb34a2a403e9f34b16cf594c917c2ecd4a995"

let () =
  check "Rails PBKDF2 key"
    "4777669222a1dfbfcb0ae1a07fee1450b0c2aeccc8821f27a4514212906dd3d142608325b7b3126978936f407bb147605a074d6d4a63bb4edd9518d72f4f9399"
    (Rails_crypto.derive_key secret "signed cookie" 64 |> Rails_crypto.hex);
  let signed_cookie =
    "eyJfcmFpbHMiOnsibWVzc2FnZSI6IkltVTFXRmRDUjBkdVJVcHhhVWh3VmtOYWVtSnFObUp5WVNJPSIsImV4cCI6IjIwNDYtMDEtMDFUMTI6MDA6MDAuMDAwWiIsInB1ciI6ImNvb2tpZS5zZXNzaW9uX3Rva2VuIn19--97d26d3a75e1d4d5a12fa3a2448d1feac36314b9"
  in
  let signed_value = `String "e5XWBGGnEJqiHpVCZzbj6bra" in
  check "Rails signed cookie generation" signed_cookie
    (Rails_crypto.sign_cookie ~secret ~name:"session_token"
       ~expires_at:"2046-01-01T12:00:00.000Z" signed_value);
  check "Rails signed cookie verification" (Some signed_value)
    (Rails_crypto.verify_cookie ~secret ~name:"session_token" signed_cookie);
  check "signed cookie purpose is bound" None
    (Rails_crypto.verify_cookie ~secret ~name:"another_cookie" signed_cookie);
  let session =
    `Assoc
      [ ("session_id", `String "6d3a2b1c0f9e8d7c6b5a493827161504");
        ("_csrf_token", `String "qK3zv7cQ2oYlP4-sX6JtW0nBf9eR1uHaMdLgE5iVy8k") ]
  in
  let encrypted_cookie =
    "NOlqOHTLnofV9OfOPTKwbz37W5h0q5v2MN7NAxU32S858Btw/VEXc0cNPhs9WTp42dlZrAyx8asFgNETEY83jKHTybeGKogaRsHJXG/M9O92ZHdCZg4zFslMfzI2DgBZIyKL3sIesagGeqVNYqRgi/4vc+QA3lX2xTL01kNKc/sKGtt2WXI45za/LQyIy1xHHmfNXpGS7nEswMMw5AXqXk1lC0PKGKd+s8YLD8f+0Tc3dJ9M6hygi2I16zTTRhRCFl5WdKjc0WsmRblgDw+xrYJ5s9NwwuqN/tA8Uom39ma0w+QUh+GBrvR5dueBJK0=--aLsLJQUZe1sHp4Y/--gXqGsSLjvclcSmibYj1XYQ=="
  in
  let nonce = Option.get (Rails_crypto.base64_decode "aLsLJQUZe1sHp4Y/") in
  check "Rails AES-256-GCM cookie generation" encrypted_cookie
    (Rails_crypto.encrypt_cookie ~secret ~name:"_campfire_session"
       ~expires_at:"2046-01-01T12:00:00.000Z" ~nonce session);
  check "Rails encrypted cookie verification" (Some session)
    (Rails_crypto.decrypt_cookie ~secret ~name:"_campfire_session" encrypted_cookie);
  let tampered = String.sub encrypted_cookie 0 10 ^ "X" ^ String.sub encrypted_cookie 11
      (String.length encrypted_cookie - 11)
  in
  check "tampered encrypted cookie is rejected" None
    (Rails_crypto.decrypt_cookie ~secret ~name:"_campfire_session" tampered);
  check "random bytes have requested length" 32
    (Rails_crypto.random_bytes 32 |> String.length);
  let rails_session_cookie =
    "CJuFb+fXRvpUoNtY4h/3YITukfWGtWCQLO4OPZYcHMn6HLweBkvzsOApyzRn+9La/Ed/mjpqK2iY9Gqp2aFVGhWTgX6kPrgE7JvDxUYU7ShT9CzVgpwCZygPwbx3E1xwnGCzoBseopfOOv2eQ+nrNsCEz6yds00OiyK+BYa7mpbUbAYs7DZMULAQ5YpmCpL0D/EJG82/ITt30zyGqYFVz6QC+NGOzijaAGa/a8cMZ/s7fQoXuJRp3U7YyIE0BAmqPDw+XZvK06E7NYeGxtjFBnlAVeaRv7a/gDu7oHcNuMibCfNav12bI244Za6WYWk=--yM/f2qfj5rpZNtpG--GHo5InJ0wALtcC8X9NuN3A=="
  in
  let session = Session.load ~secret (Some rails_session_cookie) in
  check "Rails session raw CSRF token" true
    (Session.valid_csrf session
       "k4vklJ5KxBdxnfVe662NP9X81VADA1_Hwj_ZP4LzBIo");
  let meta_token =
    "BXXSjYYRIEvqXvscN-ABJ8tOpJLgBsyNqmP25rPBAO4zlFDLhzOfRQnSIGr6PzeEfn5gv1VMtOOYsYw2jtwzVw"
  in
  check "Rails-issued masked CSRF token" true
    (Session.valid_csrf session meta_token);
  check "Rails per-form CSRF token" true
    (Session.valid_csrf ~path:"/session" ~method_:"POST" session
       "WqkUeNYeKQbeGaA7uvoDQED86MN9qcz-3tuiwhqJwhGAb8mhiEC6i1IRgp3keZdsWJ8FltYfGPZpTvXwcPJWrg");
  check "CSRF rejects wrong token" false
    (Session.valid_csrf session "invalid-token");
  check "logout clears serialized session" (`Assoc [])
    (Session.values (Session.clear session));
  let changed =
    Session.set session "return_to_after_authenticating" (`String "/rooms/1")
  in
  let changed_cookie =
    Session.set_cookie ~secret changed |> Option.get
    |> String.split_on_char ';' |> List.hd
    |> String.split_on_char '=' |> List.tl |> String.concat "="
  in
  check "changed Rails session cookie round trip"
    (Some (`Assoc
      [ ("session_id", `String "36166e44ee4e3a234bcf50a9f864d2a7");
        ("_csrf_token", `String "k4vklJ5KxBdxnfVe662NP9X81VADA1_Hwj_ZP4LzBIo");
        ("return_to_after_authenticating", `String "/rooms/1") ]))
    (Rails_crypto.decrypt_cookie ~secret ~name:"_campfire_session" changed_cookie)
