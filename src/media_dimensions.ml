let byte value index =
  if index < 0 || index >= String.length value then None
  else Some (Char.code value.[index])

let u16_le value index =
  match (byte value index, byte value (index + 1)) with
  | Some low, Some high -> Some (low lor (high lsl 8))
  | _ -> None

let u16_be value index =
  match (byte value index, byte value (index + 1)) with
  | Some high, Some low -> Some ((high lsl 8) lor low)
  | _ -> None

let u24_le value index =
  match (byte value index, byte value (index + 1), byte value (index + 2)) with
  | Some low, Some middle, Some high -> Some (low lor (middle lsl 8) lor (high lsl 16))
  | _ -> None

let u32_le value index =
  match (u16_le value index, u16_le value (index + 2)) with
  | Some low, Some high -> Some (low lor (high lsl 16))
  | _ -> None

let u32_be value index =
  match (u16_be value index, u16_be value (index + 2)) with
  | Some high, Some low -> Some ((high lsl 16) lor low)
  | _ -> None

let pair width height =
  match (width, height) with
  | Some width, Some height when width > 0 && height > 0 -> Some (width, height)
  | _ -> None

let image_content_type_of_bytes value =
  let length = String.length value in
  let starts prefix = String.starts_with ~prefix value in
  let ftyp_has_brand brands =
    let length = String.length value in
    if length < 16 || String.sub value 4 4 <> "ftyp" then false
    else
      let box_size = Option.value (u32_be value 0) ~default:0 in
      let limit = min (min box_size length) 128 in
      let rec scan index =
        index + 4 <= limit
        && (List.mem (String.sub value index 4) brands || scan (index + 4))
      in
      box_size >= 16 && scan 8
  in
  if starts "\137PNG\r\n\026\n" then Some "image/png"
  else if starts "\255\216\255" then Some "image/jpeg"
  else if starts "GIF87a" || starts "GIF89a" then Some "image/gif"
  else if starts "BM" then Some "image/bmp"
  else if length >= 12 && String.sub value 0 4 = "RIFF"
          && String.sub value 8 4 = "WEBP" then Some "image/webp"
  else if starts "II*\000" || starts "MM\000*" || starts "II+\000"
          || starts "MM\000+" then Some "image/tiff"
  else if ftyp_has_brand [ "avif"; "avis" ] then Some "image/avif"
  else if ftyp_has_brand [ "heic"; "heix"; "hevc"; "hevx" ] then Some "image/heic"
  else if
    ftyp_has_brand [ "heif"; "heis"; "heim"; "hevm"; "hevs"; "mif1"; "msf1" ]
  then Some "image/heif"
  else if starts "\000\000\000\012jP  \r\n\135\n" || starts "\255\079" then
    Some "image/jp2"
  else if starts "\000\000\001\000" then Some "image/vnd.microsoft.icon"
  else None

let jpeg value =
  let length = String.length value in
  let is_sof = function
    | 0xc0 | 0xc1 | 0xc2 | 0xc3 | 0xc5 | 0xc6 | 0xc7 | 0xc9 | 0xca | 0xcb
    | 0xcd | 0xce | 0xcf -> true
    | _ -> false
  in
  let rec scan index =
    if index + 3 >= length || value.[index] <> '\255' then None
    else
      let rec skip_fill marker_index =
        if marker_index < length && value.[marker_index] = '\255' then
          skip_fill (marker_index + 1)
        else marker_index
      in
      let marker_index = skip_fill (index + 1) in
      if marker_index >= length then None
      else
        let marker = Char.code value.[marker_index] in
        if marker = 0xd9 || marker = 0xda then None
        else if marker = 0x01 || (marker >= 0xd0 && marker <= 0xd7) then
          scan (marker_index + 1)
        else
          match u16_be value (marker_index + 1) with
          | None -> None
          | Some segment_length when segment_length < 2 -> None
          | Some segment_length when is_sof marker ->
              pair (u16_be value (marker_index + 6))
                (u16_be value (marker_index + 4))
          | Some segment_length -> scan (marker_index + 1 + segment_length)
  in
  if length >= 2 && value.[0] = '\255' && value.[1] = '\216' then scan 2
  else None

let png value =
  let signature = "\137PNG\r\n\026\n" in
  if String.length value >= 24 && String.sub value 0 8 = signature
     && String.sub value 12 4 = "IHDR"
  then pair (u32_be value 16) (u32_be value 20)
  else None

let gif value =
  if String.length value >= 10
     && (String.sub value 0 6 = "GIF87a" || String.sub value 0 6 = "GIF89a")
  then pair (u16_le value 6) (u16_le value 8)
  else None

let bmp value =
  if String.length value >= 26 && String.sub value 0 2 = "BM" then
    let height = Option.map abs (u32_le value 22) in
    pair (u32_le value 18) height
  else None

let webp value =
  if String.length value < 30 || String.sub value 0 4 <> "RIFF"
     || String.sub value 8 4 <> "WEBP"
  then None
  else
    match String.sub value 12 4 with
    | "VP8X" ->
        Option.bind (u24_le value 24) (fun width_minus_one ->
            Option.map
              (fun height_minus_one -> width_minus_one + 1, height_minus_one + 1)
              (u24_le value 27))
    | "VP8 " ->
        (match (byte value 23, u16_le value 26, u16_le value 28) with
        | Some 0x9d, Some width, Some height ->
            pair (Some (width land 0x3fff)) (Some (height land 0x3fff))
        | _ -> None)
    | "VP8L" ->
        (match (byte value 20, byte value 21, byte value 22, byte value 23, byte value 24) with
        | Some 0x2f, Some b0, Some b1, Some b2, Some b3 ->
            let width = 1 + b0 + ((b1 land 0x3f) lsl 8) in
            let height = 1 + (b1 lsr 6) + (b2 lsl 2) + ((b3 land 0x0f) lsl 10) in
            pair (Some width) (Some height)
        | _ -> None)
    | _ -> None

let of_bytes value =
  match png value with
  | Some dimensions -> Some dimensions
  | None ->
      (match jpeg value with
          | Some dimensions -> Some dimensions
          | None ->
              (match gif value with
              | Some dimensions -> Some dimensions
              | None ->
              (match webp value with Some dimensions -> Some dimensions | None -> bmp value)))

let pdfinfo_output output =
  let parse_size line =
    let words = String.split_on_char ' ' line |> List.filter (fun word -> word <> "") in
    let rec find = function
      | left :: "x" :: right :: _ ->
          (try
             let width = Float.ceil (float_of_string left) |> int_of_float in
             let height = Float.ceil (float_of_string right) |> int_of_float in
             pair (Some width) (Some height)
           with _ -> None)
      | _ :: rest -> find rest
      | [] -> None
    in
    find words
  in
  String.split_on_char '\n' output
  |> List.find_map (fun line ->
         if String.starts_with ~prefix:"Page size:" line
            || String.starts_with ~prefix:"Page 1 size:" line
         then parse_size line
         else None)

let vipsheader_output output =
  let dimension field =
    let prefix = field ^ ":" in
    String.split_on_char '\n' output
    |> List.find_map (fun line ->
           if String.starts_with ~prefix line then
             let value = String.sub line (String.length prefix)
                 (String.length line - String.length prefix)
               |> String.trim
             in
             Option.bind (int_of_string_opt value) (fun value -> Some value)
           else None)
  in
  pair (dimension "width") (dimension "height")

let of_file path =
  try
    let channel = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
        let length = min (in_channel_length channel) 8_388_608 in
        let bytes = Bytes.create length in
        let read = input channel bytes 0 length in
        of_bytes (Bytes.sub_string bytes 0 read))
  with _ -> None
