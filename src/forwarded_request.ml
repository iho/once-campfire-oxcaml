let scheme forwarded_proto =
  match
    forwarded_proto |> String.split_on_char ',' |> List.hd |> String.trim
    |> String.lowercase_ascii
  with
  | "https" -> "https"
  | _ -> "http"

let secure forwarded_proto = scheme forwarded_proto = "https"

let valid_origin ~origin ~host ~forwarded_proto =
  origin = scheme forwarded_proto ^ "://" ^ host
