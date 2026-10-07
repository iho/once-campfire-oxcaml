let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

let byte value = Char.chr (value land 0xff)

let client_frame opcode payload =
  let length = String.length payload in
  let prefix =
    if length < 126 then String.init 2 (function 0 -> byte (0x80 lor opcode) | _ -> byte (0x80 lor length))
    else if length <= 0xffff then
      String.init 4 (function
        | 0 -> byte (0x80 lor opcode)
        | 1 -> byte (0x80 lor 126)
        | 2 -> byte (length lsr 8)
        | _ -> byte length)
    else failwith "test frame too large"
  in
  let mask = "\x12\x34\x56\x78" in
  let payload =
    String.mapi (fun index character -> byte (Char.code character lxor Char.code mask.[index mod 4])) payload
  in
  String.sub prefix 0 1 ^ String.make 1 (byte ((Char.code prefix.[1] land 0x7f) lor 0x80))
  ^ (if length < 126 then "" else String.sub prefix 2 (String.length prefix - 2))
  ^ mask ^ payload

let () =
  check "RFC 6455 accept key"
    "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
    (Websocket.websocket_accept "dGhlIHNhbXBsZSBub25jZQ==");
  check "masked WebSocket text frame"
    (Ok { Websocket.opcode = 1; payload = "hello" })
    (Websocket.decode_client_frame (client_frame 1 "hello"));
  let wide = String.make 130 'x' in
  check "masked extended WebSocket text frame"
    (Ok { Websocket.opcode = 1; payload = wide })
    (Websocket.decode_client_frame (client_frame 1 wide));
  check "unmasked client frame rejected" true
    (Result.is_error
       (Websocket.decode_client_frame (Websocket.encode_server_frame ~opcode:1 "hello")));
  check "reserved frame bits rejected" true
    (Result.is_error (Websocket.decode_client_frame "\xC1\x81\x12\x34\x56\x78\x7A"));
  check "server text frame encoding" "\x81\x05hello"
    (Websocket.encode_server_frame ~opcode:1 "hello")
