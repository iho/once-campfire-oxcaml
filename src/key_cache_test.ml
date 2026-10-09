let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

(* Independent vectors: Node crypto.pbkdf2Sync(secret, salt, 1000, size,
   "sha256").toString("hex"). Existing rails_crypto_test covers Rails tokens. *)
let vectors =
  [ ("cache-test-secret-a", "signed cookie", 64,
     "c5cd98dcd91774abe4e8c4fd6939985506544dd1195e5be40b01876134ea74f2a9db7efdca2be982c095ecf06b741df85d4b0d6edcf93c4a5fd982f475d23993");
    ("cache-test-secret-a", "signed cookie", 32,
     "c5cd98dcd91774abe4e8c4fd6939985506544dd1195e5be40b01876134ea74f2");
    ("cache-test-secret-a", "authenticated encrypted cookie", 32,
     "ccbcd6e895208271f774edba2fe15f7d03a9f25fd04a88bb239ab91a8b073960");
    ("cache-test-secret-b", "signed cookie", 64,
     "c3596b766ddc6038a56e7889d89e74347bc2f212b365fc130f9b0a9411ccb9b0a08f7120dab808b0f50c018339abfb53c176c50cdf5d395e40bca2dddc5e1a65");
    ("cache-test-secret-b", "authenticated encrypted cookie", 32,
     "4a298b4558e88434b318bcb8a1c02d98131902897a144311b4cc619a25f2a61d") ]

let verify (secret, salt, size, expected) =
  check "derived key matches independent vector" expected
    (Rails_crypto.derive_key secret salt size |> Rails_crypto.hex)

let () =
  List.iter (fun vector -> verify vector; verify vector) vectors;
  List.iter verify (List.rev vectors);
  (* Exceed capacity, then verify keys again after eviction. *)
  for i = 1 to 40 do
    let salt = "salt-" ^ string_of_int i in
    check "key after salt churn"
      (Rails_crypto.pbkdf2_sha256 "cache-test-secret-a" salt 1000 32)
      (Rails_crypto.derive_key "cache-test-secret-a" salt 32)
  done;
  List.iter verify vectors;
  let check_bypass secret salt size =
    check "oversized request derives the same bytes"
      (Rails_crypto.pbkdf2_sha256 secret salt 1000 size)
      (Rails_crypto.derive_key secret salt size);
    check "oversized request clears retained keys" None
      (Domain.Safe.DLS.get Rails_crypto.derived_keys);
    List.iter verify vectors
  in
  check_bypass (String.make 1025 's') "signed cookie" 64;
  check_bypass "cache-test-secret-a" (String.make 129 's') 64;
  check_bypass "cache-test-secret-a" "signed cookie" 65;
  let cookie = Rails_crypto.sign_cookie ~secret:"cache-test-secret-a"
      ~name:"session_token" (`String "session") in
  check "rotation rejects previously signed cookies" None
    (Rails_crypto.verify_cookie ~secret:"cache-test-secret-b" ~name:"session_token" cookie);
  check "restoring secret restores correct key" (Some (`String "session"))
    (Rails_crypto.verify_cookie ~secret:"cache-test-secret-a" ~name:"session_token" cookie);
  (* Exercise the legacy C crypto bindings from independent domains. The FFI
     declarations predate portability annotations; the cache itself uses Safe.DLS. *)
  let domains = List.init 4 (fun offset ->
      (Domain.spawn [@alert "-do_not_spawn_domains-unsafe_multidomain"]) (fun () ->
      for i = 0 to 99 do
        verify (List.nth vectors ((i + offset) mod List.length vectors))
      done)) in
  List.iter Domain.join domains;
  List.iter verify vectors;
  List.iter (fun length ->
      match Rails_crypto.derive_key "cache-test-secret-a" "signed cookie" length with
      | _ -> failwith "invalid key length accepted"
      | exception Invalid_argument _ -> ()) [0; -1; 1025]
