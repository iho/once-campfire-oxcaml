type file_part = {
  name : string;
  filename : string;
  content_type : string;
  data : string;
}

exception Malformed

let find_substring haystack needle start =
  let haystack_length = String.length haystack
  and needle_length = String.length needle in
  let rec search index =
    if index + needle_length > haystack_length then None
    else if String.sub haystack index needle_length = needle then Some index
    else search (index + 1)
  in
  search start

let parameters value =
  let length = String.length value in
  let skip_space index =
    let rec skip index =
      if index < length && (value.[index] = ' ' || value.[index] = '\t') then
        skip (index + 1)
      else index
    in
    skip index
  in
  let read_token index =
    let rec finish index =
      if index < length && value.[index] <> ';' && value.[index] <> '=' then
        finish (index + 1)
      else index
    in
    let finish = finish index in
    (String.sub value index (finish - index) |> String.trim, finish)
  in
  let read_value index =
    if index < length && value.[index] = '"' then
      let buffer = Buffer.create 32 in
      let rec quoted index =
        if index >= length then raise Malformed
        else
          match value.[index] with
          | '"' -> (Buffer.contents buffer, index + 1)
          | '\\' when index + 1 < length ->
              Buffer.add_char buffer value.[index + 1];
              quoted (index + 2)
          | '\r' | '\n' -> raise Malformed
          | character -> Buffer.add_char buffer character; quoted (index + 1)
      in
      quoted (index + 1)
    else
      let rec finish index =
        if index < length && value.[index] <> ';' then finish (index + 1)
        else index
      in
      let finish = finish index in
      (String.sub value index (finish - index) |> String.trim, finish)
  in
  let rec collect index result =
    let index = skip_space index in
    if index >= length then List.rev result
    else if value.[index] <> ';' then raise Malformed
    else
      let key, after_key = read_token (skip_space (index + 1)) in
      let after_key = skip_space after_key in
      if key = "" || after_key >= length || value.[after_key] <> '=' then
        collect after_key result
      else
        let value, after_value = read_value (skip_space (after_key + 1)) in
        collect after_value ((String.lowercase_ascii key, value) :: result)
  in
  match read_token 0 with
  | (kind, index) -> (String.lowercase_ascii (String.trim kind), collect index [])

let parse ~boundary body =
  if boundary = "" || String.length boundary > 70
     || String.exists (function '\r' | '\n' -> true | _ -> false) boundary
  then raise Malformed;
  let delimiter = "--" ^ boundary in
  let delimiter_length = String.length delimiter in
  let length = String.length body in
  if length < delimiter_length + 2
     || String.sub body 0 delimiter_length <> delimiter
  then raise Malformed;
  let fields = ref [] and files = ref [] in
  let rec parts cursor count =
    if count > 100 then raise Malformed;
    if cursor + 2 <= length && String.sub body cursor 2 = "--" then
      (List.rev !fields, List.rev !files)
    else if cursor + 2 > length || String.sub body cursor 2 <> "\r\n" then
      raise Malformed
    else
      let headers_start = cursor + 2 in
      match find_substring body "\r\n\r\n" headers_start with
      | None -> raise Malformed
      | Some headers_end when headers_end - headers_start <= 16384 ->
          let raw_headers = String.sub body headers_start (headers_end - headers_start) in
          let headers =
            String.split_on_char '\n' raw_headers
            |> List.map (fun line -> String.trim (String.trim line |> String.trim))
            |> List.filter_map (fun line ->
                   match String.index_opt line ':' with
                   | None -> None
                   | Some index ->
                       Some
                         (String.sub line 0 index |> String.lowercase_ascii |> String.trim,
                          String.sub line (index + 1) (String.length line - index - 1)
                          |> String.trim))
          in
          let disposition = List.assoc_opt "content-disposition" headers in
          let kind, params =
            match disposition with Some value -> parameters value | None -> raise Malformed
          in
          if kind <> "form-data" then raise Malformed;
          let name = List.assoc_opt "name" params |> Option.value ~default:"" in
          if name = "" || String.length name > 256 then raise Malformed;
          let data_start = headers_end + 4 in
          (match find_substring body ("\r\n" ^ delimiter) data_start with
          | None -> raise Malformed
          | Some data_end ->
              let data_length = data_end - data_start in
              if data_length > 52_428_800 then raise Malformed;
              let data = String.sub body data_start data_length in
              let filename = List.assoc_opt "filename" params in
              (match filename with
              | Some filename when filename <> "" ->
                  if String.length filename > 1024 then raise Malformed;
                  let content_type =
                    List.assoc_opt "content-type" headers
                    |> Option.value ~default:"application/octet-stream"
                  in
                  files := { name; filename; content_type; data } :: !files
              | _ ->
                  if data_length > 1_048_576 then raise Malformed;
                  fields := (name, data) :: !fields);
              parts (data_end + 2 + delimiter_length) (count + 1))
      | Some _ -> raise Malformed
  in
  parts delimiter_length 0

let boundary_of_content_type content_type =
  let kind, params = parameters content_type in
  if kind <> "multipart/form-data" then None
  else List.assoc_opt "boundary" params
