type frame = { opcode : int; payload : string }

exception Protocol_error of string
exception Closed

let max_payload = 1_048_576

let websocket_accept key =
  Rails_crypto.sha1 (key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
  |> Rails_crypto.base64_encode

let byte value = Char.chr (value land 0xff)

let encode_server_frame ~opcode payload =
  let length = String.length payload in
  if length > max_payload then invalid_arg "WebSocket payload too large";
  let header = Buffer.create 10 in
  Buffer.add_char header (byte (0x80 lor opcode));
  if length < 126 then Buffer.add_char header (byte length)
  else if length <= 0xffff then (
    Buffer.add_char header (byte 126);
    Buffer.add_char header (byte (length lsr 8));
    Buffer.add_char header (byte length))
  else (
    Buffer.add_char header (byte 127);
    for shift = 56 downto 0 do
      Buffer.add_char header (byte (Int64.to_int (Int64.shift_right_logical (Int64.of_int length) shift)))
    done);
  Buffer.contents header ^ payload

let decode_client_frame raw =
  let length = String.length raw in
  if length < 2 then Error "short WebSocket frame"
  else
    let first = Char.code raw.[0] and second = Char.code raw.[1] in
    let fin = first land 0x80 <> 0 in
    let opcode = first land 0x0f in
    let masked = second land 0x80 <> 0 in
    let marker = second land 0x7f in
    if first land 0x70 <> 0 then Error "reserved WebSocket bits are set"
    else if not fin then Error (Printf.sprintf "fragmented WebSocket frame 0x%02x" first)
    else if not masked then Error "client WebSocket frame is not masked"
    else if not (List.mem opcode [ 1; 2; 8; 9; 10 ]) then Error "unknown WebSocket opcode"
    else
      let length_info =
        if marker < 126 then Ok (2, marker)
        else if marker = 126 then
          if length < 4 then Error "short WebSocket length"
          else Ok (4, (Char.code raw.[2] lsl 8) lor Char.code raw.[3])
        else if length < 10 then Error "short WebSocket length"
        else
          let value = ref 0L in
          for index = 2 to 9 do
            value := Int64.logor (Int64.shift_left !value 8)
                (Int64.of_int (Char.code raw.[index]))
          done;
          if !value < 0L || !value > Int64.of_int max_payload then Error "WebSocket payload too large"
          else Ok (10, Int64.to_int !value)
      in
      match length_info with
      | Error _ as error -> error
      | Ok (offset, payload_length) ->
          let control = opcode land 0x08 <> 0 in
          if payload_length > max_payload then Error "WebSocket payload too large"
          else if control && (not fin || payload_length > 125) then Error "invalid WebSocket control frame"
          else if length <> offset + 4 + payload_length then Error "invalid WebSocket frame length"
          else
            let mask = String.sub raw offset 4 in
            let payload_offset = offset + 4 in
            let payload = Bytes.create payload_length in
            for index = 0 to payload_length - 1 do
              Bytes.set payload index
                (byte (Char.code raw.[payload_offset + index] lxor Char.code mask.[index mod 4]))
            done;
            Ok { opcode; payload = Bytes.unsafe_to_string payload }

let read_frame_from read_exact =
  let first_two = read_exact 2 in
  let marker = Char.code first_two.[1] land 0x7f in
  let extra_length = if marker = 126 then 2 else if marker = 127 then 8 else 0 in
  let extended = read_exact extra_length in
  let mask_and_payload_length =
    if marker < 126 then first_two ^ extended
    else first_two ^ extended
  in
  let total_length = String.length mask_and_payload_length in
  let payload_length =
    if marker < 126 then marker
    else if marker = 126 then
      (Char.code extended.[0] lsl 8) lor Char.code extended.[1]
    else (
      let value = ref 0L in
      String.iter (fun character -> value := Int64.logor (Int64.shift_left !value 8) (Int64.of_int (Char.code character))) extended;
      if !value < 0L || !value > Int64.of_int max_payload then raise (Protocol_error "WebSocket payload too large");
      Int64.to_int !value)
  in
  if payload_length > max_payload then raise (Protocol_error "WebSocket payload too large");
  let mask_and_payload = read_exact (4 + payload_length) in
  let raw = String.sub mask_and_payload_length 0 total_length ^ mask_and_payload in
  match decode_client_frame raw with
  | Ok frame -> frame
  | Error reason -> raise (Protocol_error reason)

let read_frame flow =
  let read length =
    let output = Bytes.create length in
    if length > 0 then Eio.Flow.read_exact flow (Cstruct.of_bytes output);
    Bytes.unsafe_to_string output
  in
  read_frame_from read

let read_frame_buffered input =
  read_frame_from (fun length -> Eio.Buf_read.take length input)

let write_frame flow ~opcode payload =
  Eio.Flow.write flow [ Cstruct.of_string (encode_server_frame ~opcode payload) ]
