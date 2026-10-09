(* Measures request-path cryptography, not HTTP throughput. Use the same command
   on both revisions; tokens are checked on every iteration. *)
let () =
  let iterations = ref 1000 and rounds = ref 5 in
  Arg.parse
    [ ("--iterations", Arg.Set_int iterations, "Operations per sample");
      ("--rounds", Arg.Set_int rounds, "Number of samples") ]
    (fun _ -> raise (Arg.Bad "unexpected argument")) "crypto benchmark";
  if !iterations < 1 || !rounds < 1 then invalid_arg "positive counts required";
  let secret = String.make 128 'b' in
  let identity = `String "benchmark-session-token" in
  let session = `Assoc [ ("session_id", `String "benchmark-session") ] in
  let signed = Rails_crypto.sign_cookie ~secret ~name:"session_token" identity in
  let encrypted = Rails_crypto.encrypt_cookie ~secret ~name:"_campfire_session" session in
  let expected_stream = Rails_crypto.sign_turbo_stream_name ~secret "room_123" in
  let cycle () =
    if Rails_crypto.verify_cookie ~secret ~name:"session_token" signed <> Some identity
    then failwith "signed cookie mismatch";
    if Rails_crypto.decrypt_cookie ~secret ~name:"_campfire_session" encrypted <> Some session
    then failwith "encrypted cookie mismatch";
    if Rails_crypto.sign_turbo_stream_name ~secret "room_123" <> expected_stream
    then failwith "stream signature mismatch"
  in
  for _ = 1 to 20 do cycle () done;
  for round = 1 to !rounds do
    Gc.full_major ();
    let started = Unix.gettimeofday () in
    for _ = 1 to !iterations do cycle () done;
    let elapsed = Unix.gettimeofday () -. started in
    let result =
      `Assoc
        [ ("workload", `String "verify_signed_cookie_decrypt_session_sign_stream");
          ("round", `Int round); ("iterations", `Int !iterations);
          ("elapsed_seconds", `Float elapsed);
          ("operations_per_second", `Float (float_of_int !iterations /. elapsed)) ]
    in
    print_endline (Yojson.Basic.to_string result)
  done
