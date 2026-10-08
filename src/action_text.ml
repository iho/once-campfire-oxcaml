let sanitize ?secret ?resolve_user body =
  let escape_text buffer value =
    let length = String.length value in
    let safe_entity entity =
      List.mem entity [ "amp"; "lt"; "gt"; "quot"; "apos"; "nbsp" ]
      || (String.length entity > 1 && entity.[0] = '#'
         && (let start = if entity.[1] = 'x' || entity.[1] = 'X' then 2 else 1 in
             start < String.length entity
             && let valid = ref true in
                for i = start to String.length entity - 1 do
                  let c = entity.[i] in
                  if not
                       (match c with
                       | '0' .. '9' -> true
                       | 'a' .. 'f' | 'A' .. 'F' when start = 2 -> true
                       | _ -> false)
                  then valid := false
                done;
                !valid))
    in
    let rec copy index =
      if index < length then
        match value.[index] with
        | '&' ->
            (match String.index_from_opt value (index + 1) ';' with
            | Some finish when finish - index <= 12 ->
                let entity = String.sub value (index + 1) (finish - index - 1) in
                if safe_entity entity then (
                  Buffer.add_substring buffer value index (finish - index + 1);
                  copy (finish + 1))
                else (
                  Buffer.add_string buffer "&amp;";
                  copy (index + 1))
            | _ ->
                Buffer.add_string buffer "&amp;";
                copy (index + 1))
        | '<' -> Buffer.add_string buffer "&lt;"; copy (index + 1)
        | '>' -> Buffer.add_string buffer "&gt;"; copy (index + 1)
        | '"' -> Buffer.add_string buffer "&quot;"; copy (index + 1)
        | '\'' -> Buffer.add_string buffer "&#39;"; copy (index + 1)
        | character -> Buffer.add_char buffer character; copy (index + 1)
    in
    copy 0
  in
  let attribute attributes name =
    let length = String.length attributes in
    let is_word = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
      | _ -> false
    in
    let rec scan index =
      if index + String.length name > length then None
      else if String.lowercase_ascii (String.sub attributes index (String.length name))
              = String.lowercase_ascii name
              && (index = 0 || not (is_word attributes.[index - 1]))
              && (index + String.length name = length
                  || not (is_word attributes.[index + String.length name]))
      then
        let cursor = ref (index + String.length name) in
        while !cursor < length && (attributes.[!cursor] = ' ' || attributes.[!cursor] = '\t') do incr cursor done;
        if !cursor >= length || attributes.[!cursor] <> '=' then scan (index + 1)
        else (
          incr cursor;
          while !cursor < length && (attributes.[!cursor] = ' ' || attributes.[!cursor] = '\t') do incr cursor done;
          if !cursor >= length || (attributes.[!cursor] <> '"' && attributes.[!cursor] <> '\'') then None
          else
            let quote = attributes.[!cursor] in
            incr cursor;
            match String.index_from_opt attributes !cursor quote with
            | None -> None
            | Some finish -> Some (String.sub attributes !cursor (finish - !cursor)))
      else scan (index + 1)
    in
    scan 0
  in
  let allowed =
    [ "a"; "b"; "blockquote"; "br"; "code"; "del"; "div"; "em"; "i";
      "li"; "ol"; "p"; "pre"; "s"; "strong"; "u"; "ul" ]
  in
  let safe_href value =
    let value = String.trim value in
    let lower = String.lowercase_ascii value in
    value <> ""
    && List.exists (fun prefix -> String.starts_with ~prefix lower)
         [ "http://"; "https://"; "mailto:"; "/"; "#" ]
    && not (String.starts_with ~prefix:"//" value)
  in
  let href attributes =
    let length = String.length attributes in
    let rec scan index =
      if index >= length then None
      else
        let is_word = function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
          | _ -> false
        in
        if attributes.[index] = 'h' && index + 4 <= length
           && String.lowercase_ascii (String.sub attributes index 4) = "href"
           && (index = 0 || not (is_word attributes.[index - 1]))
           && (index + 4 = length || not (is_word attributes.[index + 4]))
        then
          let cursor = ref (index + 4) in
          while !cursor < length && (attributes.[!cursor] = ' ' || attributes.[!cursor] = '\t') do incr cursor done;
          if !cursor >= length || attributes.[!cursor] <> '=' then scan (index + 4)
          else (
            incr cursor;
            while !cursor < length && (attributes.[!cursor] = ' ' || attributes.[!cursor] = '\t') do incr cursor done;
            if !cursor >= length then None
            else
              let quote =
                if attributes.[!cursor] = '\'' || attributes.[!cursor] = '"' then
                  Some attributes.[!cursor]
                else None
              in
              Option.iter (fun _ -> incr cursor) quote;
              let start = !cursor in
              while !cursor < length
                    && (match quote with
                       | Some q -> attributes.[!cursor] <> q
                       | None -> attributes.[!cursor] <> ' ' && attributes.[!cursor] <> '\t')
              do incr cursor done;
              let value = String.sub attributes start (!cursor - start) in
              if safe_href value then Some value else None)
        else scan (index + 1)
    in
    scan 0
  in
  let open_tags = ref [] in
  let close_tag buffer target =
    let rec close () =
      match !open_tags with
      | [] -> ()
      | name :: rest ->
          open_tags := rest;
          Buffer.add_string buffer ("</" ^ name ^ ">");
          if name <> target then close ()
    in
    if List.mem target !open_tags then close ()
  in
  let render_tag buffer raw =
    let raw = String.trim raw in
    if raw = "" || String.starts_with ~prefix:"!" raw || String.starts_with ~prefix:"?" raw then ()
    else
      let closing = raw.[0] = '/' in
      let raw = if closing then String.sub raw 1 (String.length raw - 1) else raw in
      let finish =
        let rec find i =
          if i < String.length raw then
            match raw.[i] with 'a' .. 'z' | 'A' .. 'Z' | '-' -> find (i + 1) | _ -> i
          else i
        in
        find 0
      in
      let name = String.sub raw 0 finish |> String.lowercase_ascii in
      if name = "action-text-attachment" then
        if closing then ()
        else
          (match (secret, attribute (String.sub raw finish (String.length raw - finish)) "sgid") with
          | Some secret, Some sgid ->
              (match Rails_crypto.verify_attachable_sgid ~secret ~model:"User" sgid with
              | None -> ()
              | Some user_id ->
                  (match resolve_user with
                  | Some resolve ->
                      Option.iter
                        (fun user_name ->
                          Buffer.add_string buffer "<span class=\"mention\">";
                          escape_text buffer user_name;
                          Buffer.add_string buffer "</span>")
                        (resolve user_id)
                  | None ->
                      let escaped_sgid = Buffer.create (String.length sgid) in
                      escape_text escaped_sgid sgid;
                      Buffer.add_string buffer
                        ("<action-text-attachment sgid=\"" ^ Buffer.contents escaped_sgid
                       ^ "\" content-type=\"application/vnd.campfire.mention\"></action-text-attachment>")))
          | _ -> ())
      else if List.mem name allowed then
        if closing then close_tag buffer name
        else if name = "br" then Buffer.add_string buffer "<br>"
        else if name = "a" then
          (match href (String.sub raw finish (String.length raw - finish)) with
          | Some url ->
              let escaped_url = Buffer.create (String.length url) in
              escape_text escaped_url url;
              Buffer.add_string buffer ("<a href=\"" ^ Buffer.contents escaped_url ^ "\">");
              open_tags := "a" :: !open_tags
          | None -> ())
        else (
          Buffer.add_string buffer ("<" ^ name ^ ">");
          open_tags := name :: !open_tags)
  in
  if not (String.contains body '<') then
    let escaped = Buffer.create (String.length body) in
    escape_text escaped body;
    "<div>"
    ^ (Buffer.contents escaped |> String.split_on_char '\n'
      |> String.concat "</div><div>")
    ^ "</div>"
  else
    let output = Buffer.create (String.length body) in
    let length = String.length body in
    let rec scan_body index =
      if index < length then
        if body.[index] = '<' then
          (match String.index_from_opt body index '>' with
          | Some finish when finish - index <= 2048 ->
              render_tag output (String.sub body (index + 1) (finish - index - 1));
              scan_body (finish + 1)
          | _ ->
              escape_text output (String.sub body index 1);
              scan_body (index + 1))
        else
          let finish = Option.value ~default:length (String.index_from_opt body index '<') in
          escape_text output (String.sub body index (finish - index));
          scan_body finish
    in
    scan_body 0;
    List.iter (fun name -> Buffer.add_string output ("</" ^ name ^ ">")) !open_tags;
    let sanitized = Buffer.contents output in
    let trimmed = String.trim sanitized in
    if
      List.exists (fun prefix -> String.starts_with ~prefix trimmed)
        [ "<div"; "<p"; "<ul"; "<ol"; "<blockquote"; "<pre" ]
    then sanitized
    else "<div>" ^ sanitized ^ "</div>"

let mentioned_user_ids ~secret body =
  let attribute_value tag name =
    let length = String.length tag in
    let rec find index =
      if index + String.length name > length then None
      else if String.sub tag index (String.length name) = name then
        let cursor = ref (index + String.length name) in
        while !cursor < length && (tag.[!cursor] = ' ' || tag.[!cursor] = '\t') do incr cursor done;
        if !cursor >= length || tag.[!cursor] <> '=' then find (index + 1)
        else (
          incr cursor;
          while !cursor < length && (tag.[!cursor] = ' ' || tag.[!cursor] = '\t') do incr cursor done;
          if !cursor >= length || (tag.[!cursor] <> '"' && tag.[!cursor] <> '\'') then None
          else
            let quote = tag.[!cursor] in
            incr cursor;
            match String.index_from_opt tag !cursor quote with
            | None -> None
            | Some finish -> Some (String.sub tag !cursor (finish - !cursor)))
      else find (index + 1)
    in
    find 0
  in
  let body_length = String.length body in
  let marker = "<action-text-attachment" in
  let rec collect from ids =
    match String.index_from_opt body from '<' with
    | None -> List.sort_uniq compare ids
    | Some start ->
        if start + String.length marker <= body_length
           && String.lowercase_ascii (String.sub body start (String.length marker)) = marker
        then
          (match String.index_from_opt body start '>' with
          | None -> List.sort_uniq compare ids
          | Some finish ->
              let tag = String.sub body start (finish - start + 1) in
              let ids =
                match attribute_value tag "sgid" with
                | None -> ids
                | Some sgid ->
                    (match Rails_crypto.verify_attachable_sgid ~secret ~model:"User" sgid with
                    | Some id -> id :: ids
                    | None -> ids)
              in
              collect (finish + 1) ids)
        else collect (start + 1) ids
  in
  collect 0 []
