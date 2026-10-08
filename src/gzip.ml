external compress : string -> string = "caml_gzip_compress"

let textual_content_type content_type =
  let content_type = String.lowercase_ascii content_type in
  List.exists (fun prefix -> String.starts_with ~prefix content_type)
    [ "text/"; "application/json"; "application/javascript"; "image/svg+xml" ]

let encode ~accepted ~content_type body =
  if accepted && textual_content_type content_type && String.length body >= 1024 then
    (compress body, true)
  else (body, false)

let accepts_encoding value =
  let encodings =
    value
    |> String.split_on_char ','
    |> List.filter_map (fun item ->
           match String.split_on_char ';' item with
           | [] -> None
           | coding :: parameters ->
               let coding = String.trim (String.lowercase_ascii coding) in
               let quality =
                 List.find_map
                   (fun parameter ->
                     match String.split_on_char '=' parameter with
                     | [ name; value ]
                       when String.trim (String.lowercase_ascii name) = "q" ->
                         (try Some (float_of_string (String.trim value)) with _ -> Some 0.)
                     | _ -> None)
                   parameters
                 |> Option.value ~default:1.
               in
               Some (coding, if quality >= 0. && quality <= 1. then quality else 0.))
  in
  let quality coding = List.assoc_opt coding encodings in
  let quality =
    match quality "gzip" with
    | Some value -> value
    | None -> quality "*" |> Option.value ~default:0.
  in
  quality > 0.
