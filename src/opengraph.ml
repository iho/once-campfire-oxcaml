let starts_with ~prefix value =
  String.length value >= String.length prefix
  && String.sub value 0 (String.length prefix) = prefix

let strip_brackets host =
  let length = String.length host in
  if length >= 2 && host.[0] = '[' && host.[length - 1] = ']'
  then String.sub host 1 (length - 2)
  else host

let public_address address =
  let address = String.lowercase_ascii address in
  if not (Push_subscription.public_dns_address address) then false
  else if not (String.contains address ':') then
    (match String.split_on_char '.' address with
    | [ "192"; "88"; "99"; _ ] -> false
    | _ -> true)
  else
    let parts = String.split_on_char ':' address in
    let first = List.nth_opt parts 0 |> Option.value ~default:"" in
    let second = List.nth_opt parts 1 |> Option.value ~default:"" in
    let second =
      if second = "" then 0
      else try int_of_string ("0x" ^ second) with _ -> 0xffff
    in
    not (first = "2002" || first = "2001" && second <= 0x01ff)

let resolve_public_uri ~resolve value =
  try
    let uri = Uri.of_string value in
    let scheme = Uri.scheme uri |> Option.value ~default:"" |> String.lowercase_ascii in
    let host = Uri.host uri |> Option.value ~default:"" |> strip_brackets in
    let port =
      Uri.port uri
      |> Option.value ~default:(if scheme = "https" then 443 else 80)
    in
    if
      (scheme <> "http" && scheme <> "https") || host = ""
      || Uri.userinfo uri <> None || port < 1 || port > 65_535
    then None
    else
      let addresses = resolve host port in
      if addresses = [] || not (List.for_all public_address addresses)
      then None
      else Some (Uri.with_fragment uri None, addresses)
  with _ -> None

let valid_public_uri ~resolve value =
  resolve_public_uri ~resolve value |> Option.map fst

let system_resolve host port =
  try
    Unix.getaddrinfo host (string_of_int port) [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
    |> List.filter_map (fun (info : Unix.addr_info) ->
           match info.Unix.ai_addr with
           | Unix.ADDR_INET (address, _) -> Some (Unix.string_of_inet_addr address)
           | Unix.ADDR_UNIX _ -> None)
  with _ -> []

let media_path path =
  let path = String.lowercase_ascii path in
  List.exists (fun suffix -> String.ends_with ~suffix path)
    [ ".zip"; ".tar"; ".tar.gz"; ".tar.bz2"; ".tar.xz"; ".gz"; ".bz2";
      ".rar"; ".7z"; ".dmg"; ".exe"; ".msi"; ".pkg"; ".deb"; ".iso";
      ".jpg"; ".jpeg"; ".png"; ".gif"; ".bmp"; ".mp4"; ".mov"; ".avi";
      ".mkv"; ".wmv"; ".flv"; ".heic"; ".heif"; ".mp3"; ".wav"; ".ogg";
      ".aac"; ".wma"; ".webm"; ".ogv"; ".mpg"; ".mpeg" ]

let add_utf8 output codepoint =
  if codepoint >= 0 && codepoint <= 0x10ffff
     && not (codepoint >= 0xd800 && codepoint <= 0xdfff)
  then
    if codepoint < 0x80 then Buffer.add_char output (Char.chr codepoint)
    else if codepoint < 0x800 then (
      Buffer.add_char output (Char.chr (0xc0 lor (codepoint lsr 6)));
      Buffer.add_char output (Char.chr (0x80 lor (codepoint land 0x3f)))
    ) else if codepoint < 0x10000 then (
      Buffer.add_char output (Char.chr (0xe0 lor (codepoint lsr 12)));
      Buffer.add_char output (Char.chr (0x80 lor ((codepoint lsr 6) land 0x3f)));
      Buffer.add_char output (Char.chr (0x80 lor (codepoint land 0x3f)))
    ) else (
      Buffer.add_char output (Char.chr (0xf0 lor (codepoint lsr 18)));
      Buffer.add_char output (Char.chr (0x80 lor ((codepoint lsr 12) land 0x3f)));
      Buffer.add_char output (Char.chr (0x80 lor ((codepoint lsr 6) land 0x3f)));
      Buffer.add_char output (Char.chr (0x80 lor (codepoint land 0x3f)))
    )

let decode_entities value =
  let output = Buffer.create (String.length value) in
  let length = String.length value in
  let rec scan index =
    if index >= length then ()
    else if value.[index] <> '&' then (
      Buffer.add_char output value.[index];
      scan (index + 1)
    ) else
      match String.index_from_opt value (index + 1) ';' with
      | None -> Buffer.add_char output '&'; scan (index + 1)
      | Some semicolon when semicolon - index > 12 ->
          Buffer.add_char output '&'; scan (index + 1)
      | Some semicolon ->
          let entity = String.sub value (index + 1) (semicolon - index - 1) in
          let decoded =
            match entity with
            | "amp" -> Some "&"
            | "quot" -> Some "\""
            | "apos" | "#39" -> Some "'"
            | "lt" -> Some "<"
            | "gt" -> Some ">"
            | _ when String.length entity > 1 && entity.[0] = '#' ->
                (try
                   let codepoint =
                     if entity.[1] = 'x' || entity.[1] = 'X' then
                       int_of_string ("0x" ^ String.sub entity 2 (String.length entity - 2))
                     else int_of_string (String.sub entity 1 (String.length entity - 1))
                   in
                   let buffer = Buffer.create 4 in
                   add_utf8 buffer codepoint;
                   Some (Buffer.contents buffer)
                 with _ -> None)
            | _ -> None
          in
          (match decoded with
          | Some decoded -> Buffer.add_string output decoded
          | None -> Buffer.add_substring output value index (semicolon - index + 1));
          scan (semicolon + 1)
  in
  scan 0;
  Buffer.contents output

let strip_tags value =
  let output = Buffer.create (String.length value) in
  let in_tag = ref false in
  String.iter
    (fun character ->
      match character with
      | '<' -> in_tag := true
      | '>' when !in_tag -> in_tag := false
      | _ when not !in_tag -> Buffer.add_char output character
      | _ -> ())
    value;
  Buffer.contents output |> String.trim

let sanitize_content value = value |> decode_entities |> strip_tags

let name_character = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | ':' | '_' | '-' -> true
  | _ -> false

let parse_attributes value start finish =
  let index = ref start in
  let skip_space () =
    while !index < finish
          && (value.[!index] = ' ' || value.[!index] = '\t'
              || value.[!index] = '\r' || value.[!index] = '\n')
    do incr index done
  in
  let attributes = ref [] in
  while !index < finish do
    skip_space ();
    if !index < finish && value.[!index] = '/' then incr index
    else if !index < finish && name_character value.[!index] then (
      let name_start = !index in
      while !index < finish && name_character value.[!index] do incr index done;
      let name = String.sub value name_start (!index - name_start) |> String.lowercase_ascii in
      skip_space ();
      let content =
        if !index >= finish || value.[!index] <> '=' then ""
        else (
          incr index;
          skip_space ();
          if !index < finish && (value.[!index] = '\'' || value.[!index] = '"') then (
            let quote = value.[!index] in
            incr index;
            let start = !index in
            while !index < finish && value.[!index] <> quote do incr index done;
            let result = String.sub value start (!index - start) in
            if !index < finish then incr index;
            result
          ) else (
            let start = !index in
            while !index < finish && value.[!index] <> ' ' && value.[!index] <> '\t'
                  && value.[!index] <> '\r' && value.[!index] <> '\n' do incr index done;
            String.sub value start (!index - start)
          )
        )
      in
      attributes := (name, content) :: !attributes
    ) else incr index
  done;
  List.rev !attributes

let find_meta_end html start =
  let rec scan index quote =
    if index >= String.length html then None
    else
      let character = html.[index] in
      match quote, character with
      | Some quote, character when character = quote -> scan (index + 1) None
      | Some _, _ -> scan (index + 1) quote
      | None, ('\'' | '"' as quote) -> scan (index + 1) (Some quote)
      | None, '>' -> Some index
      | None, _ -> scan (index + 1) None
  in
  scan start None

let extract_open_graph html =
  let length = String.length html in
  let attributes = ref [] in
  let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in
  let rec scan index =
    if index + 5 > length then ()
    else if html.[index] <> '<' then scan (index + 1)
    else
      let candidate = String.sub html (index + 1) 4 |> String.lowercase_ascii in
      if candidate <> "meta" || index + 5 < length
         && not (is_space html.[index + 5] || html.[index + 5] = '/' || html.[index + 5] = '>')
      then scan (index + 1)
      else
        match find_meta_end html (index + 5) with
        | None -> ()
        | Some finish ->
            let attrs = parse_attributes html (index + 5) finish in
            let source =
              match List.assoc_opt "property" attrs with
              | Some property -> property
              | None -> List.assoc_opt "name" attrs |> Option.value ~default:""
            in
            let content = List.assoc_opt "content" attrs |> Option.value ~default:"" in
            let source = String.lowercase_ascii source in
            if starts_with ~prefix:"og:" source && content <> "" then (
              let name = String.sub source 3 (String.length source - 3) in
              if List.mem name [ "title"; "url"; "image"; "description" ] then
                attributes := List.remove_assoc name !attributes @
                              [ (name, sanitize_content content) ]
            );
            scan (finish + 1)
  in
  scan 0;
  List.rev !attributes

let twitter_hosts = [ "twitter.com"; "www.twitter.com"; "x.com"; "www.x.com" ]

let twitter_proxy_uri uri =
  let host = Uri.host uri |> Option.value ~default:"" |> String.lowercase_ascii in
  if List.mem host twitter_hosts && Uri.path uri <> "" && Uri.path uri <> "/" then
    Uri.with_host uri (Some "fxtwitter.com")
  else uri

let metadata ~resolve ~image_content_type url fields =
  let canonical = List.assoc_opt "url" fields |> Option.value ~default:url in
  let canonical =
    match valid_public_uri ~resolve canonical with
    | Some uri -> Uri.to_string uri
    | None -> url
  in
  let image =
    match List.assoc_opt "image" fields with
    | None -> None
    | Some source ->
        (match valid_public_uri ~resolve source with
        | None -> None
        | Some uri ->
            let image = Uri.to_string uri in
            (match image_content_type image with
            | Some content_type
              when List.mem (String.lowercase_ascii content_type)
                     [ "image/jpeg"; "image/png"; "image/gif"; "image/webp" ] -> Some image
            | _ -> None))
  in
  match (List.assoc_opt "title" fields, List.assoc_opt "description" fields) with
  | Some title, Some description when title <> "" && description <> "" ->
      Some (title, canonical, image, description)
  | _ -> None
