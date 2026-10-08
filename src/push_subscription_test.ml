let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

let public_resolver host =
  if host = "fcm.googleapis.com" || host = "edge.fcm.googleapis.com"
  then [ "8.8.8.8" ]
  else []

let () =
  check "Google FCM endpoint is accepted with public DNS" true
    (Push_subscription.valid_endpoint ~resolve:public_resolver
       "https://fcm.googleapis.com/fcm/send/token");
  check "permitted push-service subdomains are accepted" true
    (Push_subscription.valid_endpoint ~resolve:public_resolver
       "https://edge.fcm.googleapis.com:443/send/token");
  check "non-default HTTPS port is rejected" false
    (Push_subscription.valid_endpoint ~resolve:public_resolver
       "https://fcm.googleapis.com:8443/send/token");
  check "HTTP endpoint is rejected" false
    (Push_subscription.valid_endpoint ~resolve:public_resolver
       "http://fcm.googleapis.com/send/token");
  check "user information is rejected" false
    (Push_subscription.valid_endpoint ~resolve:public_resolver
       "https://user@fcm.googleapis.com/send/token");
  check "suffix spoofing is rejected" false
    (Push_subscription.valid_endpoint ~resolve:public_resolver
       "https://fcm.googleapis.com.attacker.example/send/token");
  check "unapproved host is rejected before lookup" false
    (Push_subscription.valid_endpoint ~resolve:public_resolver
       "https://attacker.example/send/token");
  check "private DNS answers are rejected" false
    (Push_subscription.valid_endpoint
       ~resolve:(fun _ -> [ "8.8.8.8"; "10.0.0.2" ])
       "https://fcm.googleapis.com/send/token");
  check "documentation-only DNS addresses are rejected" false
    (Push_subscription.valid_endpoint ~resolve:(fun _ -> [ "203.0.113.20" ])
       "https://fcm.googleapis.com/send/token");
  check "IPv4-mapped private DNS answers are rejected" false
    (Push_subscription.valid_endpoint ~resolve:(fun _ -> [ "::ffff:10.0.0.2" ])
       "https://fcm.googleapis.com/send/token");
  check "IPv6 unique-local DNS answers are rejected" false
    (Push_subscription.valid_endpoint ~resolve:(fun _ -> [ "fd12::1" ])
       "https://fcm.googleapis.com/send/token");
  check "IPv6 global DNS answers are accepted" true
    (Push_subscription.valid_endpoint ~resolve:(fun _ -> [ "2606:4700:4700::1111" ])
       "https://fcm.googleapis.com/send/token");
  check "empty DNS results are rejected" false
    (Push_subscription.valid_endpoint ~resolve:(fun _ -> [])
       "https://fcm.googleapis.com/send/token")
