let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

let () =
  let boundary = "---------------------------test-boundary" in
  let body =
    "--" ^ boundary
    ^ "\r\nContent-Disposition: form-data; name=\"authenticity_token\"\r\n\r\ncsrf"
    ^ "\r\n--" ^ boundary
    ^ "\r\nContent-Disposition: form-data; name=\"user[name]\"\r\n\r\nAda Lovelace"
    ^ "\r\n--" ^ boundary
    ^ "\r\nContent-Disposition: form-data; name=\"message[attachment]\"; filename=\"hello.txt\"\r\nContent-Type: text/plain\r\n\r\nhello\000world"
    ^ "\r\n--" ^ boundary ^ "--\r\n"
  in
  let fields, files = Multipart.parse ~boundary body in
  check "multipart form fields" [ ("authenticity_token", "csrf"); ("user[name]", "Ada Lovelace") ] fields;
  check "multipart binary upload field"
    [ { Multipart.name = "message[attachment]"; filename = "hello.txt";
        content_type = "text/plain"; data = "hello\000world" } ] files;
  check "multipart boundary parsing" (Some boundary)
    (Multipart.boundary_of_content_type
       ("multipart/form-data; boundary=\"" ^ boundary ^ "\""));
  (try
     ignore (Multipart.parse ~boundary "not a multipart body");
     failwith "malformed multipart body was accepted"
   with Multipart.Malformed -> ())
