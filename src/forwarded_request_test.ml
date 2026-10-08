let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

let () =
  check "plain HTTP default" "http" (Forwarded_request.scheme "");
  check "first forwarded protocol is secure" "https"
    (Forwarded_request.scheme " HTTPS , http");
  check "later forwarded protocol cannot override first" false
    (Forwarded_request.secure "http, https");
  check "HTTPS origin accepted behind TLS proxy" true
    (Forwarded_request.valid_origin ~origin:"https://campfire.example"
       ~host:"campfire.example" ~forwarded_proto:"https");
  check "HTTP origin rejected behind TLS proxy" false
    (Forwarded_request.valid_origin ~origin:"http://campfire.example"
       ~host:"campfire.example" ~forwarded_proto:"https")
