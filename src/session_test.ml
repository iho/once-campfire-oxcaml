let () =
  let session = Session.load ~secret:"test-secret" None in
  let insecure = Session.set_cookie ~secret:"test-secret" session |> Option.get in
  let secure = Session.set_cookie ~secure:true ~secret:"test-secret" session |> Option.get in
  if String.ends_with ~suffix:"; Secure" insecure then
    failwith "plain HTTP session cookie unexpectedly has Secure";
  if not (String.ends_with ~suffix:"; Secure" secure) then
    failwith "forwarded HTTPS session cookie lacks Secure"
