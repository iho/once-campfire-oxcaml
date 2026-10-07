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
        response ~headers:html_headers `OK
          "<!doctype html><html><body><main><h1>Set up Campfire</h1><p>First-run account creation is not implemented in this OxCaml port yet.</p></main></body></html>"
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
              | Some _ ->
                  html_with_session ~secret session `Not_implemented
                    "Campfire screens are not implemented in this OxCaml port yet."
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
