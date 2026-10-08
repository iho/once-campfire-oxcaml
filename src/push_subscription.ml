let permitted_hosts =
  [ "jmt17.google.com";
    "fcm.googleapis.com";
    "updates.push.services.mozilla.com";
    "web.push.apple.com";
    "notify.windows.com" ]

let permitted_host host =
  let host = String.lowercase_ascii host in
  List.exists
    (fun permitted ->
      host = permitted
      || String.ends_with ~suffix:("." ^ permitted) host)
    permitted_hosts

let endpoint_host endpoint =
  if not (String.starts_with ~prefix:"https://" (String.lowercase_ascii endpoint))
  then None
  else
    let authority_start = 8 in
    let authority_end =
      let rec find index =
        if index = String.length endpoint then index
        else
          match endpoint.[index] with
          | '/' | '?' | '#' -> index
          | _ -> find (index + 1)
      in
      find authority_start
    in
    let authority = String.sub endpoint authority_start (authority_end - authority_start) in
    if authority = "" || String.contains authority '@' then None
    else
      let host, port =
        match String.rindex_opt authority ':' with
        | None -> (authority, None)
        | Some index ->
            let suffix = String.sub authority (index + 1) (String.length authority - index - 1) in
            (String.sub authority 0 index, Some suffix)
      in
      let valid_host =
        host <> "" &&
        String.for_all
          (function
            | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' -> true
            | _ -> false)
          host
      in
      let valid_port =
        match port with
        | None -> true
        | Some "" -> false
        | Some value -> (match int_of_string_opt value with Some 443 -> true | _ -> false)
      in
      if valid_host && valid_port then Some (String.lowercase_ascii host) else None

let ipv4_public address =
  match String.split_on_char '.' address with
  | [ a; b; c; d ] ->
      (match List.map int_of_string_opt [ a; b; c; d ] with
      | [ Some a; Some b; Some c; Some d ]
        when List.for_all (fun value -> value >= 0 && value <= 255) [ a; b; c; d ] ->
          not
            (a = 0 || a = 10 || a = 127 || a >= 224 || (a = 100 && b >= 64 && b <= 127)
             || (a = 169 && b = 254) || (a = 172 && b >= 16 && b <= 31)
             || (a = 192 && (b = 0 || b = 168 && c = 0 || b = 168 || b = 0 && c = 2))
             || (a = 198 && (b = 18 || b = 19 || b = 51 && c = 100))
             || (a = 203 && b = 0 && c = 113) || (a = 255 && b = 255 && c = 255 && d = 255))
      | _ -> false)
  | _ -> false

let public_dns_address address =
  match String.split_on_char ':' address with
  | [ _; _; _; _ ] when not (String.contains address '.') -> false
  | _ ->
      if String.contains address '.' then
        (match String.split_on_char ':' address with
        | [ ipv4 ] -> ipv4_public ipv4
        | _ ->
            (match String.rindex_opt address ':' with
            | Some index -> ipv4_public (String.sub address (index + 1) (String.length address - index - 1))
            | None -> false))
      else
        let canonical = String.lowercase_ascii address in
        match String.split_on_char ':' canonical with
        | first :: _ when String.length first >= 1 ->
            (try
               let prefix = int_of_string ("0x" ^ String.sub first 0 (min 4 (String.length first))) in
               prefix >= 0x2000 && prefix <= 0x3fff
               && not (String.starts_with ~prefix:"2001:db8:" canonical)
               && not (String.starts_with ~prefix:"2001:10:" canonical)
               && not (String.starts_with ~prefix:"2001:20:" canonical)
             with _ -> false)
        | _ -> false

let resolved_public_addresses ~resolve endpoint =
  match endpoint_host endpoint with
  | None -> None
  | Some host when not (permitted_host host) -> None
  | Some host ->
      match resolve host with
      | [] -> None
      | addresses when List.for_all public_dns_address addresses -> Some (host, addresses)
      | _ -> None

let valid_endpoint ~resolve endpoint =
  Option.is_some (resolved_public_addresses ~resolve endpoint)

let system_resolve host =
  try
    Unix.getaddrinfo host "443" [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
    |> List.filter_map (fun (info : Unix.addr_info) ->
           match info.Unix.ai_addr with
           | Unix.ADDR_INET (address, _) -> Some (Unix.string_of_inet_addr address)
           | Unix.ADDR_UNIX _ -> None)
  with _ -> []
