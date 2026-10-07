let response ?(headers = Cohttp.Header.init ()) status body =
  Cohttp_eio.Server.respond_string ~status ~headers ~body ()

let html_headers =
  Cohttp.Header.init_with "content-type" "text/html; charset=utf-8"

let redirect ?(headers = Cohttp.Header.init ()) target =
  response ~headers:(Cohttp.Header.add headers "location" target) `Found ""

let trim = String.trim

exception Request_body_too_large

let header headers name =
  Cohttp.Header.get headers name |> Option.value ~default:""

let cookie cookies name =
  cookies
  |> String.split_on_char ';'
  |> List.find_map (fun part ->
         match String.index_opt part '=' with
         | None -> None
         | Some index ->
             let key = String.sub part 0 index |> trim in
             if key <> name then None
             else Some (String.sub part (index + 1) (String.length part - index - 1) |> trim))

let parse_form body =
  let decode value =
    value |> String.map (function '+' -> ' ' | c -> c)
    |> Rails_crypto.percent_decode
  in
  String.split_on_char '&' body
  |> List.filter_map (fun pair ->
         match String.index_opt pair '=' with
         | None -> None
         | Some index ->
             Option.bind (decode (String.sub pair 0 index)) (fun key ->
                 Option.map
                   (fun value -> (key, value))
                   (decode
                      (String.sub pair (index + 1)
                         (String.length pair - index - 1)))))
  |> List.fold_left
       (fun values (key, value) ->
         let rec replace = function
           | [] -> [ (key, value) ]
           | (old_key, _) :: tail when old_key = key -> (key, value) :: tail
           | head :: tail -> head :: replace tail
         in
         replace values)
       []

let form_value form key = List.assoc_opt key form |> Option.value ~default:""

let html_escape value =
  let output = Buffer.create (String.length value) in
  String.iter
    (function
      | '&' -> Buffer.add_string output "&amp;"
      | '<' -> Buffer.add_string output "&lt;"
      | '>' -> Buffer.add_string output "&gt;"
      | '"' -> Buffer.add_string output "&quot;"
      | '\'' -> Buffer.add_string output "&#39;"
      | character -> Buffer.add_char output character)
    value;
  Buffer.contents output

let set_cookie headers value = Cohttp.Header.add headers "set-cookie" value

let attach_session_cookie headers ~secret session =
  match Session.set_cookie ~secret session with
  | None -> headers
  | Some cookie -> set_cookie headers cookie

let respond_with_session ?(headers = Cohttp.Header.init ()) ~secret session status body =
  response ~headers:(attach_session_cookie headers ~secret session) status body

let html_with_session ?(headers = html_headers) ~secret session status body =
  respond_with_session ~headers ~secret session status body

let path_of_request request =
  Cohttp.Request.resource request |> String.split_on_char '?' |> List.hd

let query_parameters request =
  match String.index_opt (Cohttp.Request.resource request) '?' with
  | None -> []
  | Some index ->
      let query =
        String.sub (Cohttp.Request.resource request) (index + 1)
          (String.length (Cohttp.Request.resource request) - index - 1)
      in
      parse_form query

let secret_key_base () = Sys.getenv_opt "SECRET_KEY_BASE"

let load_session secret headers =
  let cookies = header headers "cookie" in
  Session.load ~secret (cookie cookies "_campfire_session")

let current_identity database secret headers =
  let cookies = header headers "cookie" in
  let token =
    Option.bind (cookie cookies "session_token")
      (Rails_crypto.verify_cookie ~secret ~name:"session_token")
  in
  Option.bind token (function
    | `String token -> Database.find_session_identity database token
    | _ -> None)

let request_body source =
  let buffer = Buffer.create 1024 in
  let chunk = Cstruct.create 8192 in
  let rec read () =
    let count = Eio.Flow.single_read source chunk in
    if Buffer.length buffer + count > 1_048_576 then raise Request_body_too_large;
    Buffer.add_string buffer (Cstruct.to_string (Cstruct.sub chunk 0 count));
    read ()
  in
  try read () with End_of_file -> Buffer.contents buffer

let timestamp_now () =
  let timestamp = Unix.gettimeofday () in
  let time = Unix.gmtime timestamp in
  let micros = int_of_float ((timestamp -. floor timestamp) *. 1_000_000.) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d.%06d"
    (time.Unix.tm_year + 1900) (time.Unix.tm_mon + 1) time.Unix.tm_mday
    time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec micros

let login_form ?(email = "") ?(error = "") csrf =
  let error_html =
    if error = "" then ""
    else "<p role=\"alert\">" ^ html_escape error ^ "</p>"
  in
  "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
  ^ html_escape csrf
  ^ "\"><title>Sign in · Campfire</title></head><body><main><h1>Sign in to Campfire</h1>"
  ^ error_html
  ^ "<form action=\"/session\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
  ^ html_escape csrf
  ^ "\"><label>Email address<input type=\"email\" name=\"email_address\" autocomplete=\"username\" value=\""
  ^ html_escape email
  ^ "\"></label><label>Password<input type=\"password\" name=\"password\" autocomplete=\"current-password\"></label><button type=\"submit\">Sign in</button></form></main></body></html>"

let first_run_form ?(name = "") ?(email = "") ?(error = "") csrf =
  let error_html =
    if error = "" then ""
    else "<p role=\"alert\">" ^ html_escape error ^ "</p>"
  in
  "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
  ^ html_escape csrf
  ^ "\"><title>Set up Campfire</title></head><body><main><h1>Set up Campfire</h1>"
  ^ error_html
  ^ "<form action=\"/first_run\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
  ^ html_escape csrf
  ^ "\"><label>Name<input name=\"user[name]\" autocomplete=\"name\" required value=\""
  ^ html_escape name
  ^ "\"></label><label>Email address<input type=\"email\" name=\"user[email_address]\" autocomplete=\"username\" required value=\""
  ^ html_escape email
  ^ "\"></label><label>Password<input type=\"password\" name=\"user[password]\" autocomplete=\"new-password\" maxlength=\"72\" required></label><button type=\"submit\">Save</button></form></main></body></html>"

let namespaced_form_value form namespace key =
  let prefix = namespace ^ "[" in
  form
  |> List.find_map (fun (field, value) ->
         if String.starts_with ~prefix field && String.ends_with ~suffix:"]" field
         then
           let field_name =
             String.sub field (String.length prefix)
               (String.length field - String.length prefix - 1)
           in
           if field_name = key then Some value else None
         else None)
  |> Option.value ~default:""

let html_body_to_text body =
  let output = Buffer.create (String.length body) in
  let length = String.length body in
  let rec decode index =
    if index < length then
      if body.[index] = '<' then
        (match String.index_from_opt body index '>' with
        | None ->
            Buffer.add_char output body.[index];
            decode (index + 1)
        | Some finish ->
            let tag = String.sub body index (finish - index + 1) |> String.lowercase_ascii in
            if String.starts_with ~prefix:"<br" tag || String.starts_with ~prefix:"</div" tag
               || String.starts_with ~prefix:"</p" tag || String.starts_with ~prefix:"</li" tag
            then Buffer.add_char output '\n';
            decode (finish + 1))
      else if body.[index] = '&' then
        (match String.index_from_opt body index ';' with
        | Some finish when finish - index <= 10 ->
            let entity = String.sub body index (finish - index + 1) in
            let replacement =
              match entity with
              | "&amp;" -> Some '&'
              | "&lt;" -> Some '<'
              | "&gt;" -> Some '>'
              | "&quot;" -> Some '"'
              | "&#39;" | "&apos;" -> Some '\''
              | "&nbsp;" -> Some ' '
              | _ -> None
            in
            (match replacement with
            | Some character -> Buffer.add_char output character
            | None -> Buffer.add_string output entity);
            decode (finish + 1)
        | _ ->
            Buffer.add_char output body.[index];
            decode (index + 1))
      else (
        Buffer.add_char output body.[index];
        decode (index + 1))
  in
  decode 0;
  Buffer.contents output

let safe_message_body body =
  body |> html_body_to_text |> html_escape |> String.split_on_char '\n'
  |> String.concat "<br>"

let room_page (user : Database.user) (rooms : Database.room list)
    (current : Database.room) csrf (messages : Database.message list)
    ~older_messages ~newer_messages =
  let links =
    rooms
    |> List.map (fun (room : Database.room) ->
           "<li><a href=\"/rooms/" ^ string_of_int room.Database.id ^ "\">"
           ^ html_escape room.Database.name ^ "</a></li>")
    |> String.concat ""
  in
  "<!doctype html><html><head><meta charset=\"utf-8\"><title>"
  ^ html_escape current.Database.name
  ^ " · Campfire</title><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
  ^ html_escape csrf
  ^ "\"></head><body><nav><a href=\"/\">Campfire</a> <a href=\"/searches\">Search</a><p>"
  ^ html_escape user.Database.name ^ "</p><ul>" ^ links
  ^ "</ul></nav><main><h1>"
  ^ html_escape current.Database.name
  ^ "</h1><section aria-label=\"Messages\"><ol>"
  ^ (messages
    |> List.map (fun (message : Database.message) ->
           "<li id=\"message-" ^ string_of_int message.Database.id
           ^ "\"><article><header><strong>" ^ html_escape message.Database.creator_name
           ^ "</strong> <time>" ^ html_escape message.Database.created_at
           ^ "</time></header><div class=\"message-body\">"
           ^ safe_message_body message.Database.body_html ^ "</div></article></li>")
    |> String.concat "")
  ^ "</ol></section><nav aria-label=\"Message history\">"
  ^ (match (older_messages, messages) with
    | true, first :: _ ->
        "<a rel=\"prev\" href=\"/rooms/" ^ string_of_int current.Database.id
        ^ "/messages?before=" ^ string_of_int first.Database.id ^ "\">Older messages</a>"
    | _ -> "")
  ^ (match (newer_messages, List.rev messages) with
    | true, latest :: _ ->
        "<a rel=\"next\" href=\"/rooms/" ^ string_of_int current.Database.id
        ^ "/messages?after=" ^ string_of_int latest.Database.id ^ "\">Newer messages</a>"
    | _ -> "")
  ^ "</nav><form action=\"/rooms/" ^ string_of_int current.Database.id
  ^ "/messages\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
  ^ html_escape csrf
  ^ "\"><label>Write a message<textarea name=\"message[body]\" required maxlength=\"10000\"></textarea></label><button type=\"submit\">Send</button></form></main></body></html>"

let room_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; id ] -> int_of_string_opt id
  | _ -> None

let room_message_collection_id path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; id; "messages" ] -> int_of_string_opt id
  | _ -> None

let room_at_message path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; room_id; message_id ]
    when String.starts_with ~prefix:"@" message_id ->
      Option.bind (int_of_string_opt room_id) (fun room_id ->
          Option.map (fun message_id -> (room_id, message_id))
            (int_of_string_opt
               (String.sub message_id 1 (String.length message_id - 1))))
  | _ -> None

let message_room_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; id; "messages" ] -> int_of_string_opt id
  | _ -> None

let normalize_search_query query =
  let buffer = Buffer.create (String.length query) in
  String.iter
    (fun character ->
      let code = Char.code character in
      if character = '_' || (code >= Char.code 'a' && code <= Char.code 'z')
         || (code >= Char.code 'A' && code <= Char.code 'Z')
         || (code >= Char.code '0' && code <= Char.code '9') || code >= 128
      then Buffer.add_char buffer character
      else Buffer.add_char buffer ' ')
    query;
  Buffer.contents buffer |> String.trim |> String.split_on_char ' '
  |> List.filter (fun token -> token <> "") |> String.concat " "

let fts_expression query =
  query |> String.split_on_char ' '
  |> List.filter (fun word -> word <> "")
  |> List.map (fun word -> "\"" ^ String.concat "\"\"" (String.split_on_char '"' word) ^ "\"")
  |> String.concat " "

let encode_query query =
  let output = Buffer.create (String.length query) in
  let digits = "0123456789ABCDEF" in
  String.iter
    (function
      | ' ' -> Buffer.add_char output '+'
      | character when
          (character >= 'a' && character <= 'z')
          || (character >= 'A' && character <= 'Z')
          || (character >= '0' && character <= '9')
          || String.contains "-_.~" character -> Buffer.add_char output character
      | character ->
          let byte = Char.code character in
          Buffer.add_char output '%';
          Buffer.add_char output digits.[byte lsr 4];
          Buffer.add_char output digits.[byte land 15])
    query;
  Buffer.contents output

let search_page (user : Database.user) csrf query recents results =
  let recent_html =
    recents
    |> List.map (fun recent ->
           "<li><a href=\"/searches?q=" ^ encode_query recent ^ "\">"
           ^ html_escape recent ^ "</a></li>")
    |> String.concat ""
  in
  let result_html =
    results
    |> List.map (fun (result : Database.search_result) ->
           let message = result.Database.message in
           "<li><article><a href=\"/rooms/" ^ string_of_int result.Database.room_id
           ^ "/@" ^ string_of_int message.Database.id
           ^ "\">" ^ html_escape result.Database.room_name ^ "</a><header><strong>"
           ^ html_escape message.Database.creator_name ^ "</strong> <time>"
           ^ html_escape message.Database.created_at ^ "</time></header><div>"
           ^ safe_message_body message.Database.body_html ^ "</div></article></li>")
    |> String.concat ""
  in
  "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
  ^ html_escape csrf ^ "\"><title>Search · Campfire</title></head><body><nav><a href=\"/\">Campfire</a><p>"
  ^ html_escape user.Database.name ^ "</p></nav><main><h1>Search Campfire</h1><form action=\"/searches\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
  ^ html_escape csrf ^ "\"><label>Search messages<input name=\"q\" required value=\""
  ^ html_escape query ^ "\"></label><button>Search</button></form><section><h2>Recent searches</h2><ul>"
  ^ recent_html
  ^ "</ul><form action=\"/searches/clear\" method=\"post\"><input type=\"hidden\" name=\"_method\" value=\"delete\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
  ^ html_escape csrf ^ "\"><button>Clear recent searches</button></form></section><section><h2>Results</h2><ol>"
  ^ result_html ^ "</ol></section></main></body></html>"

let create_message_id () =
  let hex = Rails_crypto.random_bytes 16 |> Rails_crypto.hex in
  String.sub hex 0 8 ^ "-" ^ String.sub hex 8 4 ^ "-"
  ^ String.sub hex 12 4 ^ "-" ^ String.sub hex 16 4 ^ "-"
  ^ String.sub hex 20 12

let valid_origin headers =
  let origin = Cohttp.Header.get headers "origin" in
  match origin with
  | None -> true
  | Some origin ->
      let host = Cohttp.Header.get headers "host" |> Option.value ~default:"" in
      origin = "http://" ^ host

let cookie_attributes expires_header =
  Printf.sprintf
    "Path=/; Max-Age=631152000; Expires=%s; HttpOnly; SameSite=Lax"
    expires_header

let start_session_cookie headers ~secret token =
  let expires_at, expires_header = Rails_crypto.cookie_expiration () in
  let value =
    Rails_crypto.sign_cookie ~secret ~name:"session_token" ~expires_at
      (`String token)
  in
  set_cookie headers ("session_token=" ^ value ^ "; " ^ cookie_attributes expires_header)

let clear_session_cookie headers =
  let _, expires_header = Rails_crypto.cookie_expiration () in
  set_cookie headers
    ("session_token=; Path=/; Max-Age=0; Expires=" ^ expires_header
   ^ "; HttpOnly; SameSite=Lax")

let sanitize_redirect = function
  | Some path when String.starts_with ~prefix:"/" path
                   && not (String.starts_with ~prefix:"//" path) -> path
  | _ -> "/"

let authenticate_form database jobs_database remote_ip secret headers body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not
          (Session.valid_csrf ~path:"/session" ~method_:"POST" session
             authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else if
    not
      (Database.allow_login jobs_database remote_ip
         ~at_ms:(Int64.of_float (Unix.gettimeofday () *. 1000.)))
  then
    html_with_session ~secret session `Too_many_requests
      "Too many requests or unauthorized."
  else
    let email_address = form_value form "email_address" in
    let password = form_value form "password" in
    let user = Database.find_active_user database email_address in
    let user =
      match user with
      | Some user ->
          (match user.Database.password_digest with
          | Some digest when Bcrypt.verify ~hash:digest password -> Some user
          | _ -> None)
      | None -> None
    in
    match user with
    | None ->
        html_with_session ~secret session ~headers:html_headers `Unauthorized
          (login_form ~email:email_address
             ~error:"Too many requests or unauthorized."
             session.Session.csrf_form_token)
    | Some user ->
        let token = Rails_crypto.random_bytes 18 |> Rails_crypto.base64url_encode in
        let timestamp = timestamp_now () in
        Database.create_session database ~user_id:user.Database.id ~token
          ~user_agent:(header headers "user-agent") ~ip_address:remote_ip
          ~timestamp;
        let destination =
          Session.get_string session "return_to_after_authenticating"
          |> sanitize_redirect
        in
        let session = Session.remove session "return_to_after_authenticating" in
        let response_headers =
          Cohttp.Header.init_with "location" destination
          |> fun headers -> attach_session_cookie headers ~secret session
          |> fun headers -> start_session_cookie headers ~secret token
        in
        response ~headers:response_headers `Found ""

let create_first_run database remote_ip secret headers body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not
          (Session.valid_csrf ~path:"/first_run" ~method_:"POST" session
             authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    let name = namespaced_form_value form "user" "name" |> trim in
    let email = namespaced_form_value form "user" "email_address" |> trim in
    let password = namespaced_form_value form "user" "password" in
    if name = "" || email = "" || password = "" || String.length password > 72
    then
      html_with_session ~headers:html_headers ~secret session
        `Unprocessable_entity
        (first_run_form ~name ~email
           ~error:"Please enter a name, email address, and password no longer than 72 bytes."
           session.Session.csrf_form_token)
    else
      try
        let password_digest = Bcrypt.hash password in
        let timestamp = timestamp_now () in
        match
          Database.create_first_run database ~name ~email_address:email
            ~password_digest ~timestamp
        with
        | None -> response `Service_unavailable "Campfire database is unavailable"
        | Some user_id ->
            let token = Rails_crypto.random_bytes 18 |> Rails_crypto.base64url_encode in
            Database.create_session database ~user_id ~token
              ~user_agent:(header headers "user-agent") ~ip_address:remote_ip
              ~timestamp;
            let response_headers =
              Cohttp.Header.init_with "location" "/"
              |> fun headers -> attach_session_cookie headers ~secret session
              |> fun headers -> start_session_cookie headers ~secret token
            in
            response ~headers:response_headers `Found ""
      with _ ->
        html_with_session ~headers:html_headers ~secret session
          `Unprocessable_entity
          (first_run_form ~name ~email
             ~error:"Campfire could not be set up with those details."
             session.Session.csrf_form_token)

let submit_message database secret headers room_id body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  let path = "/rooms/" ^ string_of_int room_id ^ "/messages" in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_:"POST" session authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        (match
           Database.find_room_for_user database identity.Database.user.id room_id
         with
        | None -> response `Not_found "Room not found or inaccessible"
        | Some _ ->
            let content = namespaced_form_value form "message" "body" in
            if String.trim content = "" || String.length content > 10_000 then
              response `Unprocessable_entity "Message must contain 1–10000 bytes"
            else
              try
                Database.create_message database ~room_id
                  ~creator_id:identity.Database.user.id ~body:content
                  ~client_message_id:(create_message_id ())
                  ~timestamp:(timestamp_now ());
                redirect ("/rooms/" ^ string_of_int room_id)
              with _ -> response `Unprocessable_entity "Message could not be saved")

let search_index database secret headers query =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      let session = load_session secret headers in
      let query = normalize_search_query query in
      let results =
        let expression = fts_expression query in
        if expression = "" then []
        else Database.search_messages database identity.Database.user.id expression
      in
      html_with_session ~secret session `OK
        (search_page identity.Database.user session.Session.csrf_form_token query
           (Database.recent_searches database identity.Database.user.id) results)

let record_search_request database secret headers body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path:"/searches" ~method_:"POST" session authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        let query = form_value form "q" |> normalize_search_query in
        if query <> "" then
          Database.record_search database identity.Database.user.id query
            (timestamp_now ());
        redirect ("/searches?q=" ^ encode_query query)

let clear_searches_request database secret headers body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not
          (Session.valid_csrf ~path:"/searches/clear" ~method_:"DELETE" session
             authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        Database.clear_searches database identity.Database.user.id;
        redirect "/searches"

let logout database secret headers body =
  let session = load_session secret headers in
  let form = parse_form body in
  let token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if
    not (valid_origin headers)
    || not (Session.valid_csrf ~path:"/session" ~method_:"DELETE" session token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else (
    Option.iter
      (fun identity -> Database.delete_session database identity.Database.session_id)
      (current_identity database secret headers);
    let headers =
      Cohttp.Header.init_with "location" "/"
      |> fun headers -> attach_session_cookie headers ~secret (Session.clear session)
      |> clear_session_cookie
    in
    response ~headers `Found "")

let room_response database secret headers room_id messages ~older_messages
    ~newer_messages =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      (match Database.find_room_for_user database identity.Database.user.id room_id with
      | None -> response `Not_found "Room not found or inaccessible"
      | Some room ->
          let session = load_session secret headers in
          let older_messages, newer_messages =
            match messages with
            | [] -> (false, false)
            | first :: _ ->
                let last = List.hd (List.rev messages) in
                ( (older_messages
                  && Database.has_messages_before database room_id first.Database.id),
                  (newer_messages
                  && Database.has_messages_after database room_id last.Database.id) )
          in
          html_with_session ~secret session `OK
            (room_page identity.Database.user
               (Database.rooms_for_user database identity.Database.user.id)
               room session.Session.csrf_form_token messages ~older_messages
               ~newer_messages))

let serve_request ~database ~jobs_database ~remote_ip request body =
  let path = path_of_request request in
  let headers = Cohttp.Request.headers request in
  match (Cohttp.Request.meth request, path) with
  | `GET, "/up" ->
      response `OK
        "<!doctype html><html><body style=\"background-color: green\">OK</body></html>"
  | `GET, "/first_run" ->
      if Database.account_exists database then redirect "/"
      else
        (match secret_key_base () with
        | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
        | Some secret ->
            let session = load_session secret headers in
            html_with_session ~secret session `OK
              (first_run_form session.Session.csrf_form_token))
  | `GET, "/searches" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          let query = query_parameters request |> fun params -> form_value params "q" in
          search_index database secret headers query)
  | `POST, "/searches" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             record_search_request database secret headers (request_body body)
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid search request"))
  | (`POST | `DELETE), "/searches/clear" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             clear_searches_request database secret headers (request_body body)
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid search request"))
  | `POST, "/first_run" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             let request_body = request_body body in
             create_first_run database remote_ip secret headers request_body
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid setup request"))
  | (`GET, "/") | (`GET, "/session/new") ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          let session = load_session secret headers in
          if path = "/" then
            if not (Database.account_exists database) then
              redirect
                ~headers:(attach_session_cookie (Cohttp.Header.init ()) ~secret session)
                "/first_run"
            else
              (match current_identity database secret headers with
              | Some identity ->
                  (match Database.rooms_for_user database identity.Database.user.id with
                  | room :: _ -> redirect ("/rooms/" ^ string_of_int room.Database.id)
                  | [] ->
                      html_with_session ~secret session `OK
                        ("<!doctype html><html><body><h1>No rooms yet</h1><p>"
                        ^ html_escape identity.Database.user.name
                        ^ "</p></body></html>"))
              | None ->
                  redirect
                    ~headers:(attach_session_cookie (Cohttp.Header.init ()) ~secret session)
                    "/session/new")
          else if Database.user_exists database then
            html_with_session ~secret session `OK
              (login_form session.Session.csrf_form_token)
          else
            redirect
              ~headers:(attach_session_cookie (Cohttp.Header.init ()) ~secret session)
              "/first_run")
  | `GET, collection_path
    when room_message_collection_id collection_path <> None ->
      (match (secret_key_base (), room_message_collection_id collection_path) with
      | Some secret, Some room_id ->
          (match current_identity database secret headers with
          | None -> redirect "/session/new"
          | Some identity ->
              (match Database.find_room_for_user database identity.Database.user.id room_id with
              | None -> response `Not_found "Room not found or inaccessible"
              | Some _ ->
                  let params = query_parameters request in
                  let before = List.assoc_opt "before" params in
                  let after = List.assoc_opt "after" params in
                  let parse_anchor = function
                    | None -> `Absent
                    | Some value ->
                        (match int_of_string_opt value with
                        | Some id -> `Value id
                        | None -> `Invalid)
                  in
                  (try
                     match (parse_anchor before, parse_anchor after) with
                  | `Invalid, _ | _, `Invalid -> response `Bad_request "Invalid message cursor"
                  | (`Value _ as before), _ ->
                      let before = match before with `Value id -> Some id | _ -> None in
                      let messages = Database.messages_for_room ?before database room_id in
                      room_response database secret headers room_id messages
                        ~older_messages:true ~newer_messages:true
                  | `Absent, (`Value _ as after) ->
                      let after = match after with `Value id -> Some id | _ -> None in
                      let messages = Database.messages_for_room ?after database room_id in
                      room_response database secret headers room_id messages
                        ~older_messages:true ~newer_messages:true
                  | `Absent, `Absent ->
                      let messages = Database.messages_for_room database room_id in
                      room_response database secret headers room_id messages
                        ~older_messages:true ~newer_messages:true
                   with Database.Message_not_found ->
                     response `Not_found "Message not found in room")))
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `GET, at_message_path when room_at_message at_message_path <> None ->
      (match (secret_key_base (), room_at_message at_message_path) with
      | Some secret, Some (room_id, message_id) ->
          let messages =
            Database.messages_for_room ~around:message_id database room_id
          in
          room_response database secret headers room_id messages
            ~older_messages:true ~newer_messages:true
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `POST, message_path when message_room_id_of_path message_path <> None ->
      (match (secret_key_base (), message_room_id_of_path message_path) with
      | Some secret, Some room_id ->
          (try
             let request_body = request_body body in
             submit_message database secret headers room_id request_body
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid message request")
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `GET, room_path ->
      (match (secret_key_base (), room_id_of_path room_path) with
      | Some secret, Some room_id ->
          let messages = Database.messages_for_room database room_id in
          room_response database secret headers room_id messages
            ~older_messages:true ~newer_messages:true
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `POST, "/session" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             let request_body = request_body body in
             if String.length request_body > 1_048_576 then
               response `Request_entity_too_large "Request body too large"
           else
             authenticate_form database jobs_database remote_ip secret headers
               request_body
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid form request"))
  | `DELETE, "/session" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try logout database secret headers (request_body body)
           with Request_body_too_large ->
             response `Request_entity_too_large "Request body too large"
              | _ -> response `Bad_request "Invalid form request"))
  | _ -> response `Not_found "Not found"

let remote_ip = function
  | ((_, (`Tcp (address, _))), _) ->
      Format.asprintf "%a" Eio.Net.Ipaddr.pp address
  | _ -> ""

let serve ~database ~jobs_database ~port ~domains =
  Eio_main.run (fun env ->
      let server =
        Cohttp_eio.Server.make
          ~callback:(fun connection request body ->
            serve_request ~database ~jobs_database
              ~remote_ip:(remote_ip connection) request body)
          ()
      in
      Eio.Switch.run (fun sw ->
          let socket =
            Eio.Net.listen ~reuse_addr:true ~backlog:128 ~sw env#net
              (`Tcp (Eio.Net.Ipaddr.V4.any, port))
          in
          Cohttp_eio.Server.run ~additional_domains:(env#domain_mgr, domains - 1)
            socket server
            ~on_error:(fun error -> prerr_endline (Printexc.to_string error))))

let () =
  let port =
    match Sys.getenv_opt "HTTP_PORT" with
    | None -> 3000
    | Some value -> (try int_of_string value with Failure _ -> 3000)
  in
  let domains =
    match Sys.getenv_opt "WEB_WORKERS" with
    | None -> 4
    | Some value -> (try max 1 (min 64 (int_of_string value)) with Failure _ -> 4)
  in
  let storage_root =
    match Sys.getenv_opt "CAMPFIRE_STORAGE_PATH" with
    | Some value when value <> "" -> value
    | _ -> Sys.getenv_opt "STORAGE_PATH" |> Option.value ~default:"storage"
  in
  let database = Database.open_existing storage_root in
  let jobs_database = Database.open_jobs storage_root in
  Fun.protect
    ~finally:(fun () ->
      Option.iter (fun db -> ignore (Sqlite3.db_close db)) database;
      ignore (Sqlite3.db_close jobs_database))
    (fun () -> serve ~database ~jobs_database ~port ~domains)
