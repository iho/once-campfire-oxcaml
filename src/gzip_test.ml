let read_all channel =
  let buffer = Buffer.create 128 in
  (try
     while true do
       Buffer.add_channel buffer channel 4096
     done
   with End_of_file -> ());
  Buffer.contents buffer

let () =
  List.iter
    (fun (encoding, expected) ->
      if Gzip.accepts_encoding encoding <> expected then
        failwith ("unexpected gzip negotiation for " ^ encoding))
    [ ("gzip", true); ("br, gzip;q=0.8", true); ("GZIP", true);
      ("gzip;q=0", false); ("br", false); ("*;q=0.5", true);
      ("gzip;q=0, *;q=1", false); ("gzip;q=1.2", false) ];
  let input = String.concat "\n" (List.init 100 (fun index -> "message-" ^ string_of_int index)) in
  let compressed, encoded =
    Gzip.encode ~accepted:true ~content_type:"text/html; charset=utf-8" input
  in
  if not encoded then failwith "large HTML response was not marked for gzip";
  if String.length compressed < 18 || String.sub compressed 0 2 <> "\x1f\x8b" then
    failwith "gzip output has no gzip header";
  let short_body, short_encoded =
    Gzip.encode ~accepted:true ~content_type:"text/css" "body{}"
  in
  if short_encoded || short_body <> "body{}" then
    failwith "small text response should remain uncompressed";
  let autocomplete_body = String.make 805 'a' in
  let autocomplete_body, autocomplete_encoded =
    Gzip.encode ~accepted:true ~content_type:"text/html" autocomplete_body
  in
  if autocomplete_encoded || String.length autocomplete_body <> 805 then
    failwith "sub-kilobyte autocomplete response should remain uncompressed";
  let _, threshold_encoded =
    Gzip.encode ~accepted:true ~content_type:"text/css" (String.make 1024 'a')
  in
  if not threshold_encoded then failwith "1 KiB text response was not compressed";
  let binary_body, binary_encoded =
    Gzip.encode ~accepted:true ~content_type:"image/png" input
  in
  if binary_encoded || binary_body <> input then
    failwith "binary image response should remain uncompressed";
  let unaccepted_body, unaccepted =
    Gzip.encode ~accepted:false ~content_type:"text/html" input
  in
  if unaccepted || unaccepted_body <> input then
    failwith "response was compressed without gzip negotiation";
  let path = Filename.temp_file "oxcaml-gzip-" ".gz" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      let output = open_out_bin path in
      Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () -> output_string output compressed);
      let command = "gzip -dc " ^ Filename.quote path in
      let channel = Unix.open_process_in command in
      let decoded = read_all channel in
      match Unix.close_process_in channel with
      | Unix.WEXITED 0 when decoded = input -> ()
      | _ -> failwith "gzip output did not round-trip through the system gzip decoder")
