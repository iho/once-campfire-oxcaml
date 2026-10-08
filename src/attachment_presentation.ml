let preview_dimensions ~width ~height =
  match (width, height) with
  | Some width, Some height when width > 0 && height > 0 ->
      let scale =
        min 1.0
          (min (1200.0 /. float_of_int width) (800.0 /. float_of_int height))
      in
      Some (float_of_int width *. scale, float_of_int height *. scale)
  | _ -> None

let number value =
  let formatted = Printf.sprintf "%.6f" value in
  let rec trim index =
    if index > 0 && formatted.[index] = '0' then trim (index - 1)
    else if formatted.[index] = '.' then String.sub formatted 0 index
    else String.sub formatted 0 (index + 1)
  in
  trim (String.length formatted - 1)

let wrap_preview ~width ~height html =
  let body, class_name =
    match preview_dimensions ~width ~height with
    | None -> html, "max-inline-size center overflow-clip"
    | Some (width, height) ->
        let style =
          " style=\"width: " ^ number (width /. 2.0) ^ "px; aspect-ratio: "
          ^ number (width /. height) ^ ";\""
        in
        "<div class=\"max-inline-size center flex overflow-clip\"" ^ style ^ ">"
        ^ html ^ "</div>", ""
  in
  if class_name = "" then body
  else "<div class=\"" ^ class_name ^ "\">" ^ body ^ "</div>"
