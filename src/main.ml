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

let parse_form_pairs body =
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

let parse_form body =
  parse_form_pairs body
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

let join_form ?(name = "") ?(email = "") ?(error = "") code csrf =
  let error_html =
    if error = "" then ""
    else "<p role=\"alert\">" ^ html_escape error ^ "</p>"
  in
  "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
  ^ html_escape csrf
  ^ "\"><title>Join Campfire</title></head><body><main><h1>Join Campfire</h1>"
  ^ error_html
  ^ "<form action=\"/join/" ^ html_escape code
  ^ "\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
  ^ html_escape csrf
  ^ "\"><label>Name<input name=\"user[name]\" autocomplete=\"name\" required value=\""
  ^ html_escape name
  ^ "\"></label><label>Email address<input type=\"email\" name=\"user[email_address]\" autocomplete=\"username\" required value=\""
  ^ html_escape email
  ^ "\"></label><label>Password<input type=\"password\" name=\"user[password]\" autocomplete=\"new-password\" maxlength=\"72\" required></label><button type=\"submit\">Join</button></form></main></body></html>"

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

let message_edit_form (user : Database.user) csrf room_id
    (message : Database.message) =
  let body = message.Database.body_html |> html_body_to_text |> html_escape in
  "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
  ^ html_escape csrf ^ "\"><title>Edit message · Campfire</title></head><body><main><a href=\"/rooms/"
  ^ string_of_int room_id ^ "\">Back to room</a><h1>Edit message</h1><form action=\"/rooms/"
  ^ string_of_int room_id ^ "/messages/" ^ string_of_int message.Database.id
  ^ "\" method=\"post\"><input type=\"hidden\" name=\"_method\" value=\"patch\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
  ^ html_escape csrf ^ "\"><label>Message<textarea name=\"message[body]\" required maxlength=\"10000\">"
  ^ body ^ "</textarea></label><button type=\"submit\">Save changes</button></form></main></body></html>"

let encode_path_component value =
  let output = Buffer.create (String.length value) in
  let hexadecimal = "0123456789ABCDEF" in
  String.iter
    (fun character ->
      if
        (character >= 'a' && character <= 'z')
        || (character >= 'A' && character <= 'Z')
        || (character >= '0' && character <= '9')
        || String.contains "-_.~" character
      then Buffer.add_char output character
      else (
        let value = Char.code character in
        Buffer.add_char output '%';
        Buffer.add_char output hexadecimal.[value lsr 4];
        Buffer.add_char output hexadecimal.[value land 15]))
    value;
  Buffer.contents output

let render_boost ~secret (boost : Database.boost) =
  let avatar_token =
    Rails_crypto.sign_user_avatar_id ~secret boost.Database.booster_id
    |> encode_path_component
  in
  "<div id=\"boost_" ^ string_of_int boost.Database.id
  ^ "\" class=\"boost boost-item\"><img aria-label=\""
  ^ html_escape (boost.Database.booster_name ^ " boosted " ^ boost.Database.content)
  ^ "\" src=\"/users/" ^ avatar_token ^ "/avatar\" width=\"24\" height=\"24\"><span>"
  ^ html_escape boost.Database.content ^ "</span></div>"

let active_storage_blob_url ~secret (attachment : Database.message_attachment) =
  let signed_id =
    Rails_crypto.sign_active_storage_blob_id ~secret attachment.Database.blob_id
  in
  "/rails/active_storage/blobs/redirect/" ^ signed_id ^ "/"
  ^ encode_path_component attachment.Database.filename

let render_message_attachment ~secret (attachment : Database.message_attachment) =
  let url = active_storage_blob_url ~secret attachment in
  let filename = html_escape attachment.Database.filename in
  if String.starts_with ~prefix:"image/" attachment.Database.content_type then
    "<a href=\"" ^ url
    ^ "\" data-lightbox-target=\"image\"><img class=\"message__attachment\" src=\""
    ^ url ^ "\" alt=\"" ^ filename ^ "\" loading=\"lazy\"></a>"
  else if String.starts_with ~prefix:"video/" attachment.Database.content_type then
    "<video src=\"" ^ url
    ^ "\" controls class=\"message__attachment\"></video>"
  else
    "<a href=\"" ^ url ^ "?disposition=attachment\">" ^ filename ^ "</a>"

let render_message_item ~secret ?(room_name = "") ?(boosts = [])
    ?(attachments = [])
    (user : Database.user) room_id csrf (message : Database.message) =
  let avatar_token =
    Rails_crypto.sign_user_avatar_id ~secret message.Database.creator_id
    |> encode_path_component
  in
  let permalink =
    "/rooms/" ^ string_of_int room_id ^ "/@"
    ^ string_of_int message.Database.id
  in
  let actions =
    if user.Database.role = 1 || user.Database.id = message.Database.creator_id then
      let path =
        "/rooms/" ^ string_of_int room_id ^ "/messages/"
        ^ string_of_int message.Database.id
      in
      "<a href=\"" ^ path ^ "/edit\">Edit</a><form action=\"" ^ path
      ^ "\" method=\"post\" style=\"display:inline\"><input type=\"hidden\" name=\"_method\" value=\"delete\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
      ^ html_escape csrf ^ "\"><button type=\"submit\">Delete</button></form>"
    else ""
  in
  "<div id=\"message_" ^ html_escape message.Database.client_message_id
  ^ "\" class=\"message\" data-message-id=\""
  ^ string_of_int message.Database.id
  ^ "\"><h2 class=\"message__day-separator\"><time>"
  ^ html_escape message.Database.created_at
  ^ "</time></h2><figure class=\"avatar message__avatar\"><img aria-hidden=\"true\" src=\"/users/"
  ^ avatar_token
  ^ "/avatar\" width=\"48\" height=\"48\"></figure><article class=\"message__body\"><header><strong class=\"message__author\">"
  ^ html_escape message.Database.creator_name
  ^ "</strong><a class=\"message__permalink\" href=\"" ^ permalink
  ^ "\"><time class=\"message__timestamp\" datetime=\""
  ^ html_escape message.Database.created_at ^ "\">"
  ^ html_escape message.Database.created_at
  ^ "</time></a><span class=\"message__room\"><a href=\"/rooms/"
  ^ string_of_int room_id ^ "\">" ^ html_escape room_name
  ^ "</a></span><div class=\"message__actions\">" ^ actions
  ^ "</div></header><div class=\"message__presentation\"><div class=\"message-body\">"
  ^ safe_message_body message.Database.body_html
  ^ (attachments |> List.map (render_message_attachment ~secret) |> String.concat "")
  ^ "</div><div class=\"boosts\" id=\"boosts_message_"
  ^ html_escape message.Database.client_message_id ^ "\">"
  ^ (boosts |> List.map (render_boost ~secret) |> String.concat "")
  ^ "</div></div></article></div>"

let room_page ~secret (user : Database.user) (rooms : Database.room list)
    (current : Database.room) csrf (messages : Database.message list)
    ~boosts_by_message ~attachments_by_message ~older_messages ~newer_messages ~room_stream =
  let links =
    rooms
    |> List.map (fun (room : Database.room) ->
           "<li><a href=\"/rooms/" ^ string_of_int room.Database.id ^ "\">"
           ^ html_escape room.Database.name ^ "</a></li>")
    |> String.concat ""
  in
  "<!doctype html><html><head><meta charset=\"utf-8\"><title>"
  ^ html_escape current.Database.name
  ^ " · Campfire</title><link rel=\"stylesheet\" href=\"/assets/campfire.css\"><link rel=\"icon\" href=\"/assets/campfire.png\" type=\"image/png\"><link rel=\"mask-icon\" href=\"/assets/campfire.svg\"><script type=\"importmap\">{\"imports\":{\"application\":\"/assets/application.js\"}}</script><link rel=\"modulepreload\" href=\"/assets/application.js\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
  ^ html_escape csrf
  ^ "\"></head><body><nav><a href=\"/\">Campfire</a> <a href=\"/searches\">Search</a><p>"
  ^ "<a href=\"/users/me/profile\">" ^ html_escape user.Database.name ^ "</a>"
  ^ "</p><a href=\"/rooms/opens/new\">New public room</a> <a href=\"/rooms/closeds/new\">New private room</a> <a href=\"/rooms/directs/new\">New direct conversation</a><ul>"
  ^ links
  ^ "</ul></nav><main><h1>"
  ^ html_escape current.Database.name
  ^ "</h1><p><a href=\"/rooms/" ^ string_of_int current.Database.id
  ^ "/involvement\">Notification preferences</a>"
  ^ (if current.Database.kind = "Rooms::Direct" then ""
     else if current.Database.creator_id = user.Database.id || user.Database.role = 1 then
       let kind = if current.Database.kind = "Rooms::Open" then "opens" else "closeds" in
       " · <a href=\"/rooms/" ^ kind ^ "/" ^ string_of_int current.Database.id
       ^ "/edit\">Edit room</a>"
     else "")
  ^ "</p>"
  ^ (if current.Database.kind = "Rooms::Direct"
        || current.Database.creator_id = user.Database.id || user.Database.role = 1
     then
       let action =
         if current.Database.kind = "Rooms::Direct" then
           "/rooms/directs/" ^ string_of_int current.Database.id
         else "/rooms/" ^ string_of_int current.Database.id
       in
       "<form action=\"" ^ action
       ^ "\" method=\"post\"><input type=\"hidden\" name=\"_method\" value=\"delete\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
       ^ html_escape csrf ^ "\"><button type=\"submit\">"
       ^ (if current.Database.kind = "Rooms::Direct" then "Delete conversation" else "Delete room")
       ^ "</button></form>"
     else "")
  ^ "<section aria-label=\"Messages\"><ol id=\"room_"
  ^ string_of_int current.Database.id ^ "_messages\">"
  ^ (messages
    |> List.map (fun (message : Database.message) ->
           render_message_item ~secret ~room_name:current.Database.name
             ~boosts:(List.assoc_opt message.Database.id boosts_by_message
               |> Option.value ~default:[]) user
             ~attachments:(List.assoc_opt message.Database.id attachments_by_message
               |> Option.to_list)
             current.Database.id csrf message)
    |> String.concat "")
  ^ "</ol></section><turbo-cable-stream-source channel=\"RoomMessagesChannel\" signed-stream-name=\""
  ^ html_escape room_stream
  ^ "\"></turbo-cable-stream-source><nav aria-label=\"Message history\">"
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

let direct_room_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; "directs"; id ] -> int_of_string_opt id
  | _ -> None

let avatar_token_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "users"; token; "avatar" ] when token <> "" -> Some token
  | _ -> None

let active_storage_blob_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rails"; "active_storage"; "blobs"; "redirect"; signed_id; _filename ] ->
      Some signed_id
  | _ -> None

let boost_collection_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "messages"; message_id; "boosts" ] -> int_of_string_opt message_id
  | _ -> None

let boost_new_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "messages"; message_id; "boosts"; "new" ] -> int_of_string_opt message_id
  | _ -> None

let boost_member_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "messages"; message_id; "boosts"; boost_id ] ->
      Option.bind (int_of_string_opt message_id) (fun message_id ->
          Option.map (fun boost_id -> (message_id, boost_id))
            (int_of_string_opt boost_id))
  | _ -> None

let room_message_collection_id path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; id; "messages" ] -> int_of_string_opt id
  | _ -> None

let room_involvement_id path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; id; "involvement" ] -> int_of_string_opt id
  | _ -> None

let room_editor_route_of_path path suffix =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; (("opens" | "closeds") as kind); id ]
    when suffix = "" -> Option.map (fun id -> (kind, id)) (int_of_string_opt id)
  | [ ""; "rooms"; (("opens" | "closeds") as kind); id; "edit" ]
    when suffix = "edit" -> Option.map (fun id -> (kind, id)) (int_of_string_opt id)
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

let join_code_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "join"; code ] when code <> "" -> Some code
  | _ -> None

let room_message_member_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; room_id; "messages"; message_id ] ->
      Option.bind (int_of_string_opt room_id) (fun room_id ->
          Option.map (fun message_id -> (room_id, message_id))
            (int_of_string_opt message_id))
  | _ -> None

let room_message_edit_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; room_id; "messages"; message_id; "edit" ] ->
      Option.bind (int_of_string_opt room_id) (fun room_id ->
          Option.map (fun message_id -> (room_id, message_id))
            (int_of_string_opt message_id))
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
           "<li data-message-id=\"" ^ string_of_int message.Database.id
           ^ "\"><article><a href=\"/rooms/" ^ string_of_int result.Database.room_id
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

let create_join_user_request database remote_ip secret headers path body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_:"POST" session authenticity_token)
  then (
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>")
  else
    match current_identity database secret headers with
    | Some _ -> redirect "/"
    | None ->
        (match join_code_of_path path with
        | None -> response `Not_found "Not found"
        | Some code when not (Database.valid_join_code database code) ->
            response `Not_found "Not found"
        | Some code ->
            let name = namespaced_form_value form "user" "name" |> trim in
            let email = namespaced_form_value form "user" "email_address" |> trim in
            let password = namespaced_form_value form "user" "password" in
            if name = "" || email = "" || password = ""
               || String.length password > 72
            then
              html_with_session ~headers:html_headers ~secret session
                `Unprocessable_entity
                (join_form ~name ~email
                   ~error:"Please enter a name, email address, and password no longer than 72 bytes."
                   code session.Session.csrf_form_token)
            else
              try
                let password_digest = Bcrypt.hash password in
                let timestamp = timestamp_now () in
                match
                  Database.create_join_user database ~join_code:code ~name
                    ~email_address:email ~password_digest ~timestamp
                with
                | None ->
                    response `Service_unavailable
                      "Campfire database is unavailable"
                | Some user_id ->
                    let token =
                      Rails_crypto.random_bytes 18
                      |> Rails_crypto.base64url_encode
                    in
                    Database.create_session database ~user_id ~token
                      ~user_agent:(header headers "user-agent")
                      ~ip_address:remote_ip ~timestamp;
                    let response_headers =
                      Cohttp.Header.init_with "location" "/"
                      |> fun headers -> attach_session_cookie headers ~secret session
                      |> fun headers -> start_session_cookie headers ~secret token
                    in
                    response ~headers:response_headers `Found ""
              with
              | Database.Invalid_join_code -> response `Not_found "Not found"
              | Database.Duplicate_email ->
                  redirect
                    ("/session/new?email_address=" ^ encode_query email)
              | _ ->
                  html_with_session ~headers:html_headers ~secret session
                    `Unprocessable_entity
                    (join_form ~name ~email
                       ~error:"Campfire could not create your account."
                       code session.Session.csrf_form_token))

let contains_substring ~needle value =
  let needle_length = String.length needle in
  let rec find index =
    index + needle_length <= String.length value
    && (String.sub value index needle_length = needle || find (index + 1))
  in
  find 0

let submit_message message_bus database secret headers room_id body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  let path = "/rooms/" ^ string_of_int room_id ^ "/messages" in
  let accepts_turbo_stream =
    Cohttp.Header.get headers "accept"
    |> Option.value ~default:""
    |> String.lowercase_ascii
    |> contains_substring ~needle:"text/vnd.turbo-stream.html"
  in
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
        | Some room ->
            let content = namespaced_form_value form "message" "body" in
            let attachment_token =
              namespaced_form_value form "message" "attachment" |> trim
            in
            let attachment_blob_id =
              Option.bind
                (Rails_crypto.verify_active_storage_blob_id ~secret attachment_token)
                (fun blob_id ->
                  if Database.find_stored_blob database blob_id <> None
                     && Database.authorized_stored_blob database ~blob_id
                          ~user_id:identity.Database.user.id
                  then Some blob_id
                  else None)
            in
            if String.trim content = "" || String.length content > 10_000 then
              response `Unprocessable_entity "Message must contain 1–10000 bytes"
            else if attachment_token <> "" && attachment_blob_id = None then
              response `Unprocessable_entity "Attachment is invalid or inaccessible"
            else
              try
                (match Database.create_message_with_id ?attachment_blob_id database ~room_id
                  ~creator_id:identity.Database.user.id ~body:content
                  ~client_message_id:(create_message_id ())
                  ~timestamp:(timestamp_now ()) with
                | Some message_id ->
                    Option.iter
                      (Cable_bus.publish message_bus ~room_id)
                      (Database.find_message database room_id message_id);
                    Database.room_member_ids database room_id
                    |> List.iter (fun user_id ->
                           Cable_bus.publish_unread message_bus ~user_id ~room_id);
                    if accepts_turbo_stream then
                      let message =
                        Database.find_message database room_id message_id
                        |> Option.get
                      in
                      let html =
                        "<turbo-stream action=\"append\" target=\"room_"
                        ^ string_of_int room_id ^ "_messages\"><template>"
                        ^ render_message_item ~secret
                            ~room_name:(room.Database.name)
                            identity.Database.user room_id
                            session.Session.csrf_form_token message
                        ^ "</template></turbo-stream>"
                      in
                      let headers =
                        Cohttp.Header.init_with "content-type"
                          "text/vnd.turbo-stream.html; charset=utf-8"
                      in
                      html_with_session ~headers ~secret session `OK html
                    else redirect ("/rooms/" ^ string_of_int room_id)
                | None -> response `Service_unavailable "Campfire database is unavailable")
              with error ->
                prerr_endline
                  ("benchmark message POST failed: " ^ Printexc.to_string error);
                response `Unprocessable_entity "Message could not be saved")

let edit_message_page database secret headers room_id message_id =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      (match Database.find_room_for_user database identity.Database.user.id room_id with
      | None -> response `Not_found "Room not found or inaccessible"
      | Some _ ->
          (match Database.find_message database room_id message_id with
          | None -> response `Not_found "Message not found"
          | Some message
            when message.Database.creator_id <> identity.Database.user.id
                 && identity.Database.user.role <> 1 ->
              response `Forbidden "Message editing is not allowed"
          | Some message ->
              let session = load_session secret headers in
              html_with_session ~secret session `OK
                (message_edit_form identity.Database.user
                   session.Session.csrf_form_token room_id message)))

let mutate_message_request message_bus database secret headers room_id message_id method_
    body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  let path =
    "/rooms/" ^ string_of_int room_id ^ "/messages/" ^ string_of_int message_id
  in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_ session authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        (try
           (match method_ with
           | "PATCH" | "PUT" ->
               let content = namespaced_form_value form "message" "body" in
               if String.trim content = "" || String.length content > 10_000 then
                 response `Unprocessable_entity
                   "Message must contain 1–10000 bytes"
               else (
                 Database.update_message database ~room_id ~message_id
                   ~user_id:identity.Database.user.id
                   ~role:identity.Database.user.role ~body:content
                   ~timestamp:(timestamp_now ());
                 Option.iter (Cable_bus.publish_replace message_bus ~room_id)
                   (Database.find_message database room_id message_id);
                 redirect path)
           | "DELETE" ->
               let message_dom_id =
                 Database.find_message database room_id message_id
                 |> Option.map (fun (message : Database.message) ->
                        "message_" ^ message.Database.client_message_id)
               in
               Database.delete_message database ~room_id ~message_id
                 ~user_id:identity.Database.user.id
                 ~role:identity.Database.user.role;
               Option.iter
                 (fun message_dom_id ->
                   Cable_bus.publish_remove message_bus ~room_id ~message_dom_id)
                 message_dom_id;
               redirect ("/rooms/" ^ string_of_int room_id)
           | _ -> response `Method_not_allowed "Method not allowed")
         with
        | Database.Message_not_found -> response `Not_found "Message not found"
        | Database.Message_not_authorized ->
            response `Forbidden "Message editing is not allowed"
        | Database.Message_has_attachments ->
            response `Unprocessable_entity
              "Editing or deleting messages with attachments is not yet supported"
        | _ -> response `Unprocessable_entity "Message could not be saved")

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

let room_creation_forbidden database (user : Database.user) =
  Database.room_creation_restricted database && user.Database.role <> 1

let closed_room_form database secret headers =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity when room_creation_forbidden database identity.Database.user ->
      response `Forbidden "Room creation is restricted to administrators"
  | Some identity ->
      let session = load_session secret headers in
      let users =
        Database.active_users database
        |> List.map (fun (user : Database.user_option) ->
               let selected = user.Database.id = identity.Database.user.id in
               let control =
                 if selected then
                   "<input type=\"hidden\" name=\"user_ids[]\" value=\""
                   ^ string_of_int user.Database.id ^ "\">"
                 else
                   "<input type=\"checkbox\" name=\"user_ids[]\" value=\""
                   ^ string_of_int user.Database.id ^ "\">"
               in
               "<li><label>" ^ html_escape user.Database.name ^ control
               ^ "</label></li>")
        |> String.concat ""
      in
      html_with_session ~secret session `OK
        ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
        ^ html_escape session.Session.csrf_form_token
        ^ "\"><title>New private room · Campfire</title></head><body><main><a href=\"/\">Campfire</a><h1>New private room</h1><form action=\"/rooms/closeds\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
        ^ html_escape session.Session.csrf_form_token
        ^ "\"><label>Room name<input name=\"room[name]\" required autofocus></label><fieldset><legend>People with access</legend><ul>"
        ^ users
        ^ "</ul></fieldset><button type=\"submit\">Create private room</button></form></main></body></html>")

let new_direct_room database secret headers =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      let session = load_session secret headers in
      let users =
        Database.active_users database
        |> List.filter (fun (user : Database.user_option) ->
               user.Database.id <> identity.Database.user.id)
        |> List.map (fun (user : Database.user_option) ->
               "<li><label><input type=\"checkbox\" name=\"user_ids[]\" value=\""
               ^ string_of_int user.Database.id ^ "\">"
               ^ html_escape user.Database.name ^ "</label></li>")
        |> String.concat ""
      in
      html_with_session ~secret session `OK
        ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
        ^ html_escape session.Session.csrf_form_token
        ^ "\"><title>New direct conversation · Campfire</title></head><body><main><a href=\"/\">Campfire</a><h1>Start a direct conversation</h1><form action=\"/rooms/directs\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
        ^ html_escape session.Session.csrf_form_token
        ^ "\"><fieldset><legend>People to ping</legend><ul>"
        ^ users
        ^ "</ul></fieldset><button type=\"submit\">Start conversation</button></form></main></body></html>")

let new_open_room database secret headers =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity when room_creation_forbidden database identity.Database.user ->
      response `Forbidden "Room creation is restricted to administrators"
  | Some _ ->
      let session = load_session secret headers in
      html_with_session ~secret session `OK
        ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
        ^ html_escape session.Session.csrf_form_token
        ^ "\"><title>New room · Campfire</title></head><body><main><a href=\"/\">Campfire</a><h1>New room</h1><form action=\"/rooms/opens\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
        ^ html_escape session.Session.csrf_form_token
        ^ "\"><label>Room name<input name=\"room[name]\" required autofocus></label><button type=\"submit\">Create room</button></form></main></body></html>")

let create_open_room_request database secret headers body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not
          (Session.valid_csrf ~path:"/rooms/opens" ~method_:"POST" session
             authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity when room_creation_forbidden database identity.Database.user ->
        response `Forbidden "Room creation is restricted to administrators"
    | Some identity ->
        let name = namespaced_form_value form "room" "name" in
        (match
           Database.create_open_room database ~name
             ~creator_id:identity.Database.user.id ~timestamp:(timestamp_now ())
         with
        | Some room_id -> redirect ("/rooms/" ^ string_of_int room_id)
        | None -> response `Unprocessable_entity "Room could not be saved")

let create_closed_room_request database secret headers body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not
          (Session.valid_csrf ~path:"/rooms/closeds" ~method_:"POST" session
             authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity when room_creation_forbidden database identity.Database.user ->
        response `Forbidden "Room creation is restricted to administrators"
    | Some identity ->
        let member_ids =
          parse_form_pairs body
          |> List.filter_map (fun (key, value) ->
                 if key = "user_ids[]" then int_of_string_opt value else None)
        in
        let name = namespaced_form_value form "room" "name" in
        (match
           Database.create_closed_room database ~name
             ~creator_id:identity.Database.user.id ~member_ids
             ~timestamp:(timestamp_now ())
         with
        | Some room_id -> redirect ("/rooms/" ^ string_of_int room_id)
        | None -> response `Unprocessable_entity "Room could not be saved")

let create_direct_room_request database secret headers body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not
          (Session.valid_csrf ~path:"/rooms/directs" ~method_:"POST" session
             authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        let member_ids =
          parse_form_pairs body
          |> List.filter_map (fun (key, value) ->
                 if key = "user_ids[]" then int_of_string_opt value else None)
        in
        (match
           Database.find_or_create_direct_room database
             ~creator_id:identity.Database.user.id ~member_ids
             ~timestamp:(timestamp_now ())
         with
        | Some room_id -> redirect ("/rooms/" ^ string_of_int room_id)
        | None -> response `Service_unavailable "Campfire database is unavailable")

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
          let boosts_by_message = Database.boosts_for_messages database messages in
          let attachments_by_message =
            Database.attachments_for_messages database messages
          in
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
          let room_gid =
            Rails_crypto.base64url_encode
              ("gid://campfire/" ^ room.Database.kind ^ "/"
             ^ string_of_int room.Database.id)
          in
          let room_stream =
            Rails_crypto.sign_turbo_stream_name ~secret
              (room_gid ^ ":messages")
          in
          html_with_session ~secret session `OK
            (room_page ~secret identity.Database.user
               (Database.rooms_for_user database identity.Database.user.id)
               room session.Session.csrf_form_token messages ~boosts_by_message
               ~attachments_by_message ~older_messages
               ~newer_messages ~room_stream))

let sidebar_page database secret headers =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      let session = load_session secret headers in
      let rooms = Database.sidebar_rooms database identity.Database.user.id in
      let render_room direct (entry : Database.sidebar_room) =
        let room = entry.Database.room in
        let direct_class = if direct then " direct" else " room" in
        let unread_class = if entry.Database.unread then " unread" else "" in
        "<a id=\"room_" ^ string_of_int room.Database.id
        ^ "_list\" class=\"btn align-center gap txt-nowrap" ^ direct_class
        ^ unread_class ^ "\" href=\"/rooms/" ^ string_of_int room.Database.id
        ^ "\"><span class=\"overflow-ellipsis\">"
        ^ html_escape room.Database.name ^ "</span></a>"
      in
      let directs, shared =
        List.partition
          (fun (entry : Database.sidebar_room) ->
            entry.Database.room.Database.kind = "Rooms::Direct")
          rooms
      in
      let render_list direct values =
        values |> List.map (render_room direct) |> String.concat ""
      in
      let global_stream = Rails_crypto.sign_turbo_stream_name ~secret "rooms" in
      let user_gid =
        Rails_crypto.base64url_encode
          ("gid://campfire/User/" ^ string_of_int identity.Database.user.id)
      in
      let user_stream =
        Rails_crypto.sign_turbo_stream_name ~secret (user_gid ^ ":rooms")
      in
      html_with_session ~secret session `OK
        ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
        ^ html_escape session.Session.csrf_form_token
        ^ "\"></head><body><turbo-frame id=\"user_sidebar\"><turbo-cable-stream-source channel=\"Turbo::StreamsChannel\" signed-stream-name=\""
        ^ html_escape global_stream
        ^ "\"></turbo-cable-stream-source><turbo-cable-stream-source channel=\"Turbo::StreamsChannel\" signed-stream-name=\""
        ^ html_escape user_stream
        ^ "\"></turbo-cable-stream-source><div class=\"sidebar__container\"><section class=\"directs\"><a class=\"direct direct__new\" href=\"/rooms/directs/new\">Ping</a><div id=\"direct_rooms\" contents data-controller=\"sorted-list\">"
        ^ render_list true directs
        ^ "</div></section><section class=\"rooms\"><div id=\"shared_rooms\" contents data-controller=\"sorted-list\">"
        ^ render_list false shared
        ^ "</div></section></div><div class=\"sidebar__tools\"><a href=\"/users/me/profile\">"
        ^ html_escape identity.Database.user.name
        ^ "</a></div></turbo-frame></body></html>")

let avatar_svg name =
  let initials =
    name |> String.split_on_char ' '
    |> List.filter (fun word -> word <> "")
    |> List.filter_map (fun word ->
           if String.length word = 0 then None
           else Some (String.sub word 0 1 |> String.uppercase_ascii))
    |> String.concat ""
  in
  let initials = if initials = "" then "?" else initials in
  "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 512 512\" class=\"avatar\" aria-hidden=\"true\"><rect width=\"100%\" height=\"100%\" rx=\"50\" fill=\"#3B4B59\"/><text x=\"50%\" y=\"50%\" fill=\"#FFFFFF\" text-anchor=\"middle\" dy=\"0.35em\" font-family=\"sans-serif\" font-size=\"230\" font-weight=\"800\">"
  ^ html_escape initials ^ "</text></svg>"

let safe_storage_key key =
  String.length key >= 4
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
         | _ -> false)
       key

let read_avatar_file key =
  if not (safe_storage_key key) then None
  else
    let root =
      Sys.getenv_opt "CAMPFIRE_STORAGE_PATH"
      |> Option.value ~default:"/rails/storage"
    in
    let path =
      Filename.concat root
        (Filename.concat "files"
           (Filename.concat (String.sub key 0 2)
              (Filename.concat (String.sub key 2 2) key)))
    in
    try
      let channel = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () -> Some (really_input_string channel (in_channel_length channel)))
    with _ -> None

let read_storage_file = read_avatar_file

let show_active_storage_blob database secret headers signed_id ~range ~attachment =
  match current_identity database secret headers with
  | None -> response `Unauthorized "Authentication required"
  | Some identity ->
  match
    Option.bind (Rails_crypto.percent_decode signed_id)
      (Rails_crypto.verify_active_storage_blob_id ~secret)
  with
  | None -> response `Not_found "Not found"
  | Some blob_id ->
      (match Database.find_stored_blob database blob_id with
      | None -> response `Not_found "Not found"
      | Some blob when not (Database.authorized_stored_blob database ~blob_id
                             ~user_id:identity.Database.user.Database.id) ->
          response `Forbidden "Forbidden"
      | Some blob ->
          (match read_storage_file blob.Database.key with
          | None -> response `Not_found "Not found"
          | Some body ->
              let headers =
                Cohttp.Header.init_with "content-type" blob.Database.content_type
                |> fun headers -> Cohttp.Header.add headers "accept-ranges" "bytes"
                |> fun headers ->
                Cohttp.Header.add headers "content-disposition"
                  (if attachment then
                     "attachment; filename=\"" ^ blob.Database.filename ^ "\""
                   else "inline; filename=\"" ^ blob.Database.filename ^ "\"")
              in
              let partial start finish =
                let length = finish - start + 1 in
                let partial_body = String.sub body start length in
                let headers =
                  headers
                  |> fun headers ->
                  Cohttp.Header.add headers "content-range"
                    (Printf.sprintf "bytes %d-%d/%d" start finish (String.length body))
                  |> fun headers ->
                  Cohttp.Header.add headers "content-length" (string_of_int length)
                in
                response ~headers (Cohttp.Code.status_of_code 206) partial_body
              in
              let parse_range range =
                match String.split_on_char '=' range with
                | [ "bytes"; limits ] when not (String.contains limits ',') ->
                    (match String.split_on_char '-' limits with
                    | [ ""; suffix ] ->
                        (match int_of_string_opt suffix with
                        | Some suffix when suffix > 0 && body <> "" ->
                            Some (max 0 (String.length body - suffix), String.length body - 1)
                        | _ -> None)
                    | [ first; "" ] ->
                        (match int_of_string_opt first with
                        | Some first when first >= 0 && first < String.length body ->
                            Some (first, String.length body - 1)
                        | _ -> None)
                    | [ first; last ] ->
                        (match (int_of_string_opt first, int_of_string_opt last) with
                        | Some first, Some last
                          when first >= 0 && first < String.length body && last >= first ->
                            Some (first, min last (String.length body - 1))
                        | _ -> None)
                    | _ -> None)
                | _ -> None
              in
              match range with
              | Some range ->
                  (match parse_range range with
                  | Some (start, finish) -> partial start finish
                  | None ->
                      let headers =
                        Cohttp.Header.add headers "content-range"
                          (Printf.sprintf "bytes */%d" (String.length body))
                      in
                      response ~headers (Cohttp.Code.status_of_code 416) "")
              | None -> response ~headers `OK body))

let read_public_asset name =
  let root =
    Sys.getenv_opt "CAMPFIRE_ASSET_PATH"
    |> Option.value ~default:(Filename.concat (Sys.getcwd ()) "assets")
  in
  try
    let channel = open_in_bin (Filename.concat root name) in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> Some (really_input_string channel (in_channel_length channel)))
  with _ -> None

let show_avatar database secret path =
  match avatar_token_of_path path with
  | None -> response `Not_found "Not found"
  | Some token ->
      (match
         Option.bind (Rails_crypto.percent_decode token)
           (Rails_crypto.verify_user_avatar_id ~secret)
       with
      | None -> response `Not_found "Not found"
      | Some user_id ->
          (match Database.find_avatar_user database user_id with
          | None -> response `Not_found "Not found"
          | Some user ->
              let headers content_type =
                Cohttp.Header.init_with "content-type" content_type
                |> fun headers ->
                Cohttp.Header.add headers "cache-control"
                  "public, max-age=1800, stale-while-revalidate=604800"
              in
              (match Database.find_avatar_blob database user_id with
              | Some blob
                when String.starts_with ~prefix:"image/" blob.Database.content_type ->
                  (match read_avatar_file blob.Database.key with
                  | Some image ->
                      response ~headers:(headers blob.Database.content_type) `OK image
                  | None -> response `Not_found "Not found")
              | _ ->
                  response ~headers:(headers "image/svg+xml; charset=utf-8") `OK
                    (avatar_svg user.Database.name))))

let profile_page database secret headers ?(error = "") () =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      (match Database.find_profile database identity.Database.user.id with
      | None -> response `Not_found "User not found"
      | Some profile ->
          let session = load_session secret headers in
          let memberships =
            Database.rooms_for_user database profile.Database.id
            |> List.sort (fun (left : Database.room) (right : Database.room) ->
                   compare (String.lowercase_ascii left.Database.name)
                     (String.lowercase_ascii right.Database.name))
            |> List.map (fun (room : Database.room) ->
                   "<li><a href=\"/rooms/" ^ string_of_int room.Database.id
                   ^ "\">" ^ html_escape room.Database.name ^ "</a></li>")
            |> String.concat ""
          in
          let error_html =
            if error = "" then "" else "<p role=\"alert\">" ^ html_escape error ^ "</p>"
          in
          html_with_session ~secret session `OK
            ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
            ^ html_escape session.Session.csrf_form_token
            ^ "\"><title>" ^ html_escape profile.Database.name
            ^ " · Campfire</title></head><body><nav><a href=\"/\">Campfire</a></nav><main><a href=\"/\">Back to Campfire</a><h1>Profile</h1>"
            ^ error_html ^ "<form action=\"/users/me/profile\" method=\"post\"><input type=\"hidden\" name=\"_method\" value=\"patch\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
            ^ html_escape session.Session.csrf_form_token
            ^ "\"><label>Name<input name=\"user[name]\" autocomplete=\"name\" required value=\""
            ^ html_escape profile.Database.name
            ^ "\"></label><label>Email address<input type=\"email\" name=\"user[email_address]\" autocomplete=\"username\" value=\""
            ^ html_escape profile.Database.email_address
            ^ "\"></label><label>Change password<input type=\"password\" name=\"user[password]\" autocomplete=\"new-password\" maxlength=\"72\"></label><label>Bio<textarea name=\"user[bio]\" maxlength=\"200\" rows=\"3\">"
            ^ html_escape profile.Database.bio
            ^ "</textarea></label><button type=\"submit\">Save profile</button></form><section><h2>Your rooms</h2><ul>"
            ^ memberships ^ "</ul></section></main></body></html>"))

let update_profile_request database secret headers body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  let path = "/users/me/profile" in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_:"PATCH" session authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        let name = namespaced_form_value form "user" "name" in
        let email = namespaced_form_value form "user" "email_address" in
        let password = namespaced_form_value form "user" "password" in
        let bio = namespaced_form_value form "user" "bio" in
        if String.length password > 72 then
          profile_page database secret headers
            ~error:"Password must be no longer than 72 bytes." ()
        else
          let password_digest =
            if password = "" then None else Some (Bcrypt.hash password)
          in
          if
            Database.update_profile database ~user_id:identity.Database.user.id
              ~name ~email_address:email ~password_digest ~bio
              ~timestamp:(timestamp_now ())
          then redirect path
          else
            profile_page database secret headers
              ~error:"Profile could not be saved. The email address may already be in use." ()

let delete_room_request database secret headers path room_id is_direct body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_:"DELETE" session authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        (match Database.find_room_for_user database identity.Database.user.id room_id with
        | None -> response `Not_found "Room not found or inaccessible"
        | Some room
          when (room.Database.kind = "Rooms::Direct") <> is_direct ->
            response `Not_found "Room not found or inaccessible"
        | Some room
          when not is_direct
               && not
                    (Database.can_administer_room database ~room_id
                       ~user_id:identity.Database.user.id ~role:identity.Database.user.role) ->
            response `Forbidden "Room administration is not allowed"
        | Some _ ->
            (try
               if Database.delete_room database ~room_id
                    ~user_id:identity.Database.user.id ~role:identity.Database.user.role
               then redirect "/"
               else response `Service_unavailable "Campfire database is unavailable"
             with
            | Database.Room_not_found -> response `Not_found "Room not found"
            | Database.Room_not_authorized ->
                response `Forbidden "Room deletion is not allowed"
            | Database.Room_has_attachments ->
                response `Unprocessable_entity
                  "Deleting rooms with attachments is not yet supported"))

let edit_room_page database secret headers room_id =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      (match Database.find_room_for_user database identity.Database.user.id room_id with
      | None -> response `Not_found "Room not found or inaccessible"
      | Some room when room.Database.kind = "Rooms::Direct" ->
          response `Not_found "Room not found or inaccessible"
      | Some room
        when not
               (Database.can_administer_room database ~room_id
                  ~user_id:identity.Database.user.id ~role:identity.Database.user.role) ->
          response `Forbidden "Room administration is not allowed"
      | Some room ->
          let session = load_session secret headers in
          let member_ids = Database.room_member_ids database room_id in
          let users =
            Database.active_users database
            |> List.map (fun (user : Database.user_option) ->
                   let selected =
                     room.Database.kind = "Rooms::Open"
                     || List.mem user.Database.id member_ids
                   in
                   let checked = if selected then " checked" else "" in
                   "<li><label><input type=\"checkbox\" name=\"user_ids[]\" value=\""
                   ^ string_of_int user.Database.id ^ "\"" ^ checked ^ ">"
                   ^ html_escape user.Database.name ^ "</label></li>")
            |> String.concat ""
          in
          let selected_type =
            if room.Database.kind = "Rooms::Open" then "Rooms::Open" else "Rooms::Closed"
          in
          let open_selected = if selected_type = "Rooms::Open" then " selected" else "" in
          let closed_selected = if selected_type = "Rooms::Closed" then " selected" else "" in
          let kind = if room.Database.kind = "Rooms::Open" then "opens" else "closeds" in
          html_with_session ~secret session `OK
            ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
            ^ html_escape session.Session.csrf_form_token
            ^ "\"><title>Edit room · Campfire</title></head><body><main><a href=\"/rooms/"
            ^ string_of_int room_id ^ "\">Back to room</a><h1>Edit room</h1><form action=\"/rooms/"
            ^ kind ^ "/" ^ string_of_int room_id
            ^ "\" method=\"post\"><input type=\"hidden\" name=\"_method\" value=\"patch\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
            ^ html_escape session.Session.csrf_form_token
            ^ "\"><label>Room name<input name=\"room[name]\" required value=\""
            ^ html_escape room.Database.name
            ^ "\"></label><label>Access<select name=\"room[type]\"><option value=\"Rooms::Open\""
            ^ open_selected
            ^ ">Everyone</option><option value=\"Rooms::Closed\"" ^ closed_selected
            ^ ">Selected people</option></select></label><fieldset><legend>People with access (used for private rooms)</legend><ul>"
            ^ users
            ^ "</ul></fieldset><button type=\"submit\">Save room</button></form></main></body></html>"))

let update_room_request database secret headers path room_id body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_:"PATCH" session authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        (match Database.find_room_for_user database identity.Database.user.id room_id with
        | None -> response `Not_found "Room not found or inaccessible"
        | Some room when room.Database.kind = "Rooms::Direct" ->
            response `Not_found "Room not found or inaccessible"
        | Some _
          when not
                 (Database.can_administer_room database ~room_id
                    ~user_id:identity.Database.user.id ~role:identity.Database.user.role) ->
            response `Forbidden "Room administration is not allowed"
        | Some _ ->
            let name = namespaced_form_value form "room" "name" in
            let kind = namespaced_form_value form "room" "type" in
            let member_ids =
              parse_form_pairs body
              |> List.filter_map (fun (key, value) ->
                     if key = "user_ids[]" then int_of_string_opt value else None)
            in
            if
              Database.update_shared_room database ~room_id
                ~user_id:identity.Database.user.id ~role:identity.Database.user.role
                ~name ~kind ~member_ids ~timestamp:(timestamp_now ())
            then redirect ("/rooms/" ^ string_of_int room_id)
            else response `Unprocessable_entity "Room could not be saved")

let involvement_page database secret headers room_id =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      (match Database.find_room_for_user database identity.Database.user.id room_id with
      | None -> response `Not_found "Room not found or inaccessible"
      | Some room ->
          let session = load_session secret headers in
          (match Database.membership_involvement database ~room_id
                   ~user_id:identity.Database.user.id with
          | None -> response `Not_found "Membership not found"
          | Some current ->
              let options =
                [ ("invisible", "Hide this room");
                  ("nothing", "No notifications");
                  ("mentions", "Only when mentioned");
                  ("everything", "All messages") ]
                |> List.map (fun (value, label) ->
                       let selected = if value = current then " selected" else "" in
                       "<option value=\"" ^ value ^ "\"" ^ selected ^ ">"
                       ^ label ^ "</option>")
                |> String.concat ""
              in
              html_with_session ~secret session `OK
                ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
                ^ html_escape session.Session.csrf_form_token
                ^ "\"><title>Notification preferences · Campfire</title></head><body><main><a href=\"/rooms/"
                ^ string_of_int room.Database.id
                ^ "\">Back to room</a><h1>Notification preferences</h1><form action=\"/rooms/"
                ^ string_of_int room_id
                ^ "/involvement\" method=\"post\"><input type=\"hidden\" name=\"_method\" value=\"patch\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
                ^ html_escape session.Session.csrf_form_token
                ^ "\"><label>Notify me<select name=\"involvement\">" ^ options
                ^ "</select></label><button type=\"submit\">Save preferences</button></form></main></body></html>")))

let update_involvement_request database secret headers room_id body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  let path = "/rooms/" ^ string_of_int room_id ^ "/involvement" in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_:"PATCH" session authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        (match Database.find_room_for_user database identity.Database.user.id room_id with
        | None -> response `Not_found "Room not found or inaccessible"
        | Some _ ->
            let involvement = form_value form "involvement" in
            if Database.update_membership_involvement database ~room_id
                 ~user_id:identity.Database.user.id ~involvement
                 ~timestamp:(timestamp_now ())
            then redirect path
            else response `Unprocessable_entity "Invalid notification preference")

let boosts_page database secret headers message_id ~new_boost =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      (match
         Database.room_id_for_message_user database ~message_id
           ~user_id:identity.Database.user.id
       with
      | None -> response `Not_found "Message not found"
      | Some room_id ->
          (match Database.find_message database room_id message_id with
          | None -> response `Not_found "Message not found"
          | Some message ->
              let session = load_session secret headers in
              let action = "/messages/" ^ string_of_int message_id ^ "/boosts" in
              let content =
                if new_boost then
                  "<form id=\"new_boost_message_" ^ string_of_int message_id
                  ^ "\" action=\"" ^ action
                  ^ "\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
                  ^ html_escape session.Session.csrf_form_token
                  ^ "\"><label>Boost<textarea name=\"boost[content]\" required maxlength=\"500\"></textarea></label><button type=\"submit\">Boost</button></form>"
                else
                  let boosts =
                    Database.boosts_for_messages database [ message ]
                    |> List.assoc_opt message_id
                    |> Option.value ~default:[]
                  in
                  "<main><h1>Boosts</h1>"
                  ^ (boosts |> List.map (render_boost ~secret) |> String.concat "")
                  ^ "</main>"
              in
              html_with_session ~secret session `OK
                ("<!doctype html><html><head><meta name=\"csrf-token\" content=\""
                ^ html_escape session.Session.csrf_form_token
                ^ "\"></head><body>"
                ^ (if new_boost then "" else "<a href=\"" ^ action ^ "/new\">New boost</a>")
                ^ "<p>" ^ html_escape message.Database.body_html ^ "</p>" ^ content
                ^ "</body></html>")))

let mutate_boost_request database secret headers message_id boost_id method_ body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token = form_value form "authenticity_token" in
  let path = "/messages/" ^ string_of_int message_id ^ "/boosts/" ^ string_of_int boost_id in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_ session authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        (match Database.room_id_for_message_user database ~message_id
                 ~user_id:identity.Database.user.id with
        | None -> response `Not_found "Message not found"
        | Some _ ->
            if method_ <> "DELETE" then response `Method_not_allowed "Method not allowed"
            else if not (Database.delete_boost database ~boost_id ~message_id
                           ~booster_id:identity.Database.user.id) then
              response `Not_found "Boost not found"
            else
              let html =
                "<turbo-stream action=\"remove\" target=\"boost_"
                ^ string_of_int boost_id ^ "\"></turbo-stream>"
              in
              let response_headers =
                Cohttp.Header.init_with "content-type"
                  "text/vnd.turbo-stream.html; charset=utf-8"
              in
              html_with_session ~headers:response_headers ~secret session `OK html)

let create_boost_request database secret headers message_id body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token = form_value form "authenticity_token" in
  let path = "/messages/" ^ string_of_int message_id ^ "/boosts" in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_:"POST" session authenticity_token)
  then
    html_with_session ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        (match Database.room_id_for_message_user database ~message_id
                 ~user_id:identity.Database.user.id with
        | None -> response `Not_found "Message not found"
        | Some room_id ->
            let content = namespaced_form_value form "boost" "content" |> String.trim in
            if content = "" || String.length content > 500 then
              response `Unprocessable_entity "Boost must contain 1–500 bytes"
            else
              try
                match Database.create_boost database ~message_id
                        ~booster_id:identity.Database.user.id ~content
                        ~timestamp:(timestamp_now ()) with
                | None -> response `Service_unavailable "Campfire database is unavailable"
                | Some boost_id ->
                    let message =
                      Database.find_message database room_id message_id |> Option.get
                    in
                    let card =
                      Database.boosts_for_messages database [ message ]
                      |> List.assoc_opt message_id
                      |> Option.value ~default:[]
                      |> List.find (fun (boost : Database.boost) -> boost.Database.id = boost_id)
                      |> render_boost ~secret
                    in
                    if
                      Cohttp.Header.get headers "accept"
                      |> Option.value ~default:""
                      |> String.lowercase_ascii
                      |> contains_substring ~needle:"text/vnd.turbo-stream.html"
                    then
                      let html =
                        "<turbo-stream action=\"append\" target=\"boosts_message_"
                        ^ html_escape message.Database.client_message_id
                        ^ "\"><template>" ^ card ^ "</template></turbo-stream>"
                      in
                      let response_headers =
                        Cohttp.Header.init_with "content-type"
                          "text/vnd.turbo-stream.html; charset=utf-8"
                      in
                      html_with_session ~headers:response_headers ~secret session `OK html
                    else redirect path
              with _ -> response `Unprocessable_entity "Boost could not be saved")

let serve_request ~database ~jobs_database ~message_bus ~remote_ip request body =
  let path = path_of_request request in
  let headers = Cohttp.Request.headers request in
  match (Cohttp.Request.meth request, path) with
  | `GET, "/up" ->
      response `OK
        "<!doctype html><html><body style=\"background-color: green\">OK</body></html>"
  | `GET, "/assets/campfire.css" ->
      response ~headers:(Cohttp.Header.init_with "content-type" "text/css; charset=utf-8")
        `OK
        ":root{color-scheme:light dark}body{margin:0;font-family:system-ui,sans-serif}.sidebar__container{display:flex;flex-direction:column;gap:.75rem}.room,.direct{display:flex;align-items:center;padding:.5rem;text-decoration:none}.unread{font-weight:700}"
  | `GET, "/assets/application.js" ->
      (match read_public_asset "application.js" with
      | Some body ->
          response
            ~headers:(Cohttp.Header.init_with "content-type" "text/javascript; charset=utf-8")
            `OK body
      | None -> response `Not_found "Asset not found")
  | `GET, "/assets/campfire.svg" ->
      (match read_public_asset "campfire.svg" with
      | Some body ->
          response
            ~headers:(Cohttp.Header.init_with "content-type" "image/svg+xml")
            `OK body
      | None -> response `Not_found "Asset not found")
  | `GET, "/assets/campfire.png" ->
      (match read_public_asset "campfire.png" with
      | Some body ->
          response ~headers:(Cohttp.Header.init_with "content-type" "image/png")
            `OK body
      | None -> response `Not_found "Asset not found")
  | `GET, join_path when join_code_of_path join_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (match current_identity database secret headers with
          | Some _ -> redirect "/"
          | None ->
              let code = Option.get (join_code_of_path join_path) in
              if not (Database.valid_join_code database code) then
                response `Not_found "Not found"
              else
                let session = load_session secret headers in
                html_with_session ~secret session `OK
                  (join_form code session.Session.csrf_form_token)))
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
  | `GET, "/users/me/sidebar" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret -> sidebar_page database secret headers)
  | `GET, avatar_path when avatar_token_of_path avatar_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret -> show_avatar database secret avatar_path)
  | `GET, blob_path when active_storage_blob_id_of_path blob_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          show_active_storage_blob database secret
            headers (Option.get (active_storage_blob_id_of_path blob_path))
            ~range:(Cohttp.Header.get headers "range")
            ~attachment:(
              query_parameters request |> fun params ->
              form_value params "disposition" = "attachment"))
  | `GET, "/users/me/profile" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret -> profile_page database secret headers ())
  | (`POST | `PATCH), "/users/me/profile" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             let body = request_body body in
             let method_ =
               match Cohttp.Request.meth request with
               | `PATCH -> "PATCH"
               | `POST ->
                   parse_form body |> fun form -> form_value form "_method"
                   |> String.uppercase_ascii
               | _ -> ""
             in
             if method_ <> "PATCH" then response `Method_not_allowed "Method not allowed"
             else update_profile_request database secret headers body
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid profile update request"))
  | `GET, involvement_path when room_involvement_id involvement_path <> None ->
      (match (secret_key_base (), room_involvement_id involvement_path) with
      | Some secret, Some room_id -> involvement_page database secret headers room_id
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | (`POST | `PATCH), involvement_path
    when room_involvement_id involvement_path <> None ->
      (match (secret_key_base (), room_involvement_id involvement_path) with
      | Some secret, Some room_id ->
          (try
             let body = request_body body in
             let method_ =
               match Cohttp.Request.meth request with
               | `PATCH -> "PATCH"
               | `POST ->
                   let override = form_value (parse_form body) "_method" in
                   String.uppercase_ascii override
               | _ -> ""
             in
             if method_ <> "PATCH" then response `Method_not_allowed "Method not allowed"
             else update_involvement_request database secret headers room_id body
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid notification preference request")
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `GET, room_edit_path when room_editor_route_of_path room_edit_path "edit" <> None ->
      (match (secret_key_base (), room_editor_route_of_path room_edit_path "edit") with
      | Some secret, Some (_, room_id) -> edit_room_page database secret headers room_id
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | (`POST | `PATCH), room_update_path
    when room_editor_route_of_path room_update_path "" <> None ->
      (match (secret_key_base (), room_editor_route_of_path room_update_path "") with
      | Some secret, Some (kind, room_id) ->
          (try
             let body = request_body body in
             let method_ =
               match Cohttp.Request.meth request with
               | `PATCH -> "PATCH"
               | `POST ->
                   parse_form body |> fun form -> form_value form "_method"
                   |> String.uppercase_ascii
               | _ -> ""
             in
             if method_ <> "PATCH" then response `Method_not_allowed "Method not allowed"
             else
               update_room_request database secret headers
                 ("/rooms/" ^ kind ^ "/" ^ string_of_int room_id) room_id body
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid room update request")
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `GET, "/rooms/opens/new" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret -> new_open_room database secret headers)
  | `POST, "/rooms/opens" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try create_open_room_request database secret headers (request_body body)
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid room request"))
  | `GET, "/rooms/closeds/new" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret -> closed_room_form database secret headers)
  | `GET, "/rooms/directs/new" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret -> new_direct_room database secret headers)
  | `POST, "/rooms/closeds" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try create_closed_room_request database secret headers (request_body body)
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid room request"))
  | `POST, "/rooms/directs" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             create_direct_room_request database secret headers (request_body body)
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid direct-room request"))
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
  | `POST, join_path when join_code_of_path join_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             create_join_user_request database remote_ip secret headers join_path
               (request_body body)
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid invitation request"))
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
              (login_form
                 ~email:(query_parameters request
                         |> fun params -> form_value params "email_address")
                 session.Session.csrf_form_token)
          else
            redirect
              ~headers:(attach_session_cookie (Cohttp.Header.init ()) ~secret session)
              "/first_run")
  | `GET, boost_new_path when boost_new_of_path boost_new_path <> None ->
      (match (secret_key_base (), boost_new_of_path boost_new_path) with
      | Some secret, Some message_id -> boosts_page database secret headers message_id ~new_boost:true
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `GET, boost_path when boost_collection_of_path boost_path <> None ->
      (match (secret_key_base (), boost_collection_of_path boost_path) with
      | Some secret, Some message_id -> boosts_page database secret headers message_id ~new_boost:false
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | (`POST | `DELETE), boost_member_path when boost_member_of_path boost_member_path <> None ->
      (match (secret_key_base (), boost_member_of_path boost_member_path) with
      | Some secret, Some (message_id, boost_id) ->
          (try
             let body = request_body body in
             let method_ =
               match Cohttp.Request.meth request with
               | `DELETE -> "DELETE"
               | `POST ->
                   parse_form body |> fun form -> form_value form "_method"
                   |> String.uppercase_ascii
               | _ -> ""
             in
             mutate_boost_request database secret headers message_id boost_id method_ body
           with Request_body_too_large -> response `Request_entity_too_large "Request body too large"
              | _ -> response `Bad_request "Invalid boost request")
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `POST, boost_path when boost_collection_of_path boost_path <> None ->
      (match (secret_key_base (), boost_collection_of_path boost_path) with
      | Some secret, Some message_id ->
          (try create_boost_request database secret headers message_id (request_body body)
           with Request_body_too_large -> response `Request_entity_too_large "Request body too large"
              | _ -> response `Bad_request "Invalid boost request")
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
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
  | `GET, edit_path when room_message_edit_of_path edit_path <> None ->
      (match (secret_key_base (), room_message_edit_of_path edit_path) with
      | Some secret, Some (room_id, message_id) ->
          edit_message_page database secret headers room_id message_id
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `GET, message_path when room_message_member_of_path message_path <> None ->
      (match (secret_key_base (), room_message_member_of_path message_path) with
      | Some secret, Some (room_id, message_id) ->
          (match current_identity database secret headers with
          | None -> redirect "/session/new"
          | Some identity ->
              (match Database.find_room_for_user database identity.Database.user.id room_id with
              | None -> response `Not_found "Room not found or inaccessible"
              | Some _ ->
                  if Database.find_message database room_id message_id = None then
                    response `Not_found "Message not found"
                  else redirect ("/rooms/" ^ string_of_int room_id ^ "/@" ^ string_of_int message_id)))
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
  | (`POST | `PATCH | `PUT | `DELETE), message_path
    when room_message_member_of_path message_path <> None ->
      (match (secret_key_base (), room_message_member_of_path message_path) with
      | Some secret, Some (room_id, message_id) ->
          (try
             let body = request_body body in
             let method_ =
               match Cohttp.Request.meth request with
               | `POST ->
                   parse_form body |> fun form -> form_value form "_method"
                   |> String.uppercase_ascii
                   |> fun override -> if override = "" then "POST" else override
               | `PATCH -> "PATCH"
               | `PUT -> "PUT"
               | `DELETE -> "DELETE"
               | _ -> "POST"
             in
             mutate_message_request message_bus database secret headers room_id message_id
               method_ body
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid message request")
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `POST, message_path when message_room_id_of_path message_path <> None ->
      (match (secret_key_base (), message_room_id_of_path message_path) with
      | Some secret, Some room_id ->
          (try
             let request_body = request_body body in
             submit_message message_bus database secret headers room_id request_body
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid message request")
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | (`POST | `DELETE), room_path when room_id_of_path room_path <> None ->
      (match (secret_key_base (), room_id_of_path room_path) with
      | Some secret, Some room_id ->
          (try
             let body = request_body body in
             let method_ =
               match Cohttp.Request.meth request with
               | `DELETE -> "DELETE"
               | `POST ->
                   parse_form body |> fun form -> form_value form "_method"
                   |> String.uppercase_ascii
               | _ -> ""
             in
             if method_ <> "DELETE" then response `Method_not_allowed "Method not allowed"
             else
               delete_room_request database secret headers room_path room_id false body
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid room deletion request")
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | (`POST | `DELETE), direct_room_path
    when direct_room_id_of_path direct_room_path <> None ->
      (match (secret_key_base (), direct_room_id_of_path direct_room_path) with
      | Some secret, Some room_id ->
          (try
             let body = request_body body in
             let method_ =
               match Cohttp.Request.meth request with
               | `DELETE -> "DELETE"
               | `POST ->
                   parse_form body |> fun form -> form_value form "_method"
                   |> String.uppercase_ascii
               | _ -> ""
             in
             if method_ <> "DELETE" then response `Method_not_allowed "Method not allowed"
             else
               delete_room_request database secret headers direct_room_path room_id true body
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid direct-room deletion request")
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

let header_has_token headers name wanted =
  Cohttp.Header.get_multi headers name
  |> List.concat_map (String.split_on_char ',')
  |> List.exists (fun value -> String.trim value |> String.lowercase_ascii = wanted)

let cable_room_id database user_id channel signed_name secret =
  match Rails_crypto.verify_turbo_stream_name ~secret signed_name with
  | None -> None
  | Some "rooms" when channel = "Turbo::StreamsChannel" -> Some `Sidebar
  | Some stream_name
    when channel = "Turbo::StreamsChannel" && String.ends_with ~suffix:":rooms" stream_name ->
      let gid = String.sub stream_name 0 (String.length stream_name - 6) in
      (match Rails_crypto.base64_decode gid |> Option.map (String.split_on_char '/') with
      | Some [ "gid:"; ""; "campfire"; "User"; raw_id ]
        when int_of_string_opt raw_id = Some user_id -> Some `Sidebar
      | _ -> None)
  | Some stream_name ->
      let suffix = ":messages" in
      if channel <> "RoomMessagesChannel" || not (String.ends_with ~suffix stream_name) then None
      else
        let gid = String.sub stream_name 0 (String.length stream_name - String.length suffix) in
        Option.bind (Rails_crypto.base64_decode gid) (fun gid ->
            match String.split_on_char '/' gid with
            | [ "gid:"; ""; "campfire"; ("Rooms::Open" | "Rooms::Closed" | "Rooms::Direct"); raw_id ] ->
                Option.bind (int_of_string_opt raw_id) (fun room_id ->
                    Option.map (fun _ -> `Room room_id)
                      (Database.find_room_for_user database user_id room_id))
            | _ -> None)

let cable_identifier identifier =
  try
    match Yojson.Basic.from_string identifier with
    | `Assoc fields ->
        (match List.assoc_opt "channel" fields with
        | Some (`String ("RoomMessagesChannel" | "Turbo::StreamsChannel" as channel)) ->
            Option.bind (List.assoc_opt "signed_stream_name" fields)
              (function `String signed_name -> Some (channel, `Signed signed_name) | _ -> None)
        | Some (`String "PresenceChannel") ->
            Option.bind (List.assoc_opt "room_id" fields)
              (function `Int room_id -> Some ("PresenceChannel", `Room_id room_id) | _ -> None)
        | Some (`String ("UnreadRoomsChannel" | "ReadRoomsChannel" | "HeartbeatChannel" as channel)) ->
            Some (channel, `No_params)
        | _ -> None)
    | _ -> None
  with _ -> None

let authorize_cable_subscription database identity secret (channel, params) =
  match (channel, params) with
  | ("RoomMessagesChannel" | "Turbo::StreamsChannel"), `Signed signed_name ->
      cable_room_id database identity.Database.user.Database.id channel signed_name secret
  | "PresenceChannel", `Room_id room_id ->
      Option.map (fun _ -> `Presence room_id)
        (Database.find_room_for_user database identity.Database.user.Database.id room_id)
  | "UnreadRoomsChannel", `No_params ->
      Some (`Unread identity.Database.user.Database.id)
  | "ReadRoomsChannel", `No_params ->
      Some (`Read identity.Database.user.Database.id)
  | "HeartbeatChannel", `No_params -> Some `Control
  | _ -> None

let cable_websocket env database message_bus database_lock identity secret csrf ic oc =
  let database_read f = Eio.Mutex.use_ro database_lock f in
  let database_write f = Eio.Mutex.use_rw ~protect:true database_lock f in
  let subscriptions = ref [] in
  let events = Eio.Stream.create max_int in
  let write_lock = Eio.Mutex.create () in
  let write_frame opcode payload =
    Eio.Mutex.use_rw ~protect:true write_lock (fun () ->
        Eio.Buf_write.string oc (Websocket.encode_server_frame ~opcode payload);
        Eio.Buf_write.flush oc)
  in
  let write_json json = write_frame 1 (Yojson.Basic.to_string json) in
  let send_stream identifier html =
    write_json
      (`Assoc
        [ ("identifier", `String identifier); ("message", `String html) ])
  in
  let room_name room_id =
    database_read (fun () ->
      Database.find_room_for_user database identity.Database.user.Database.id room_id)
    |> Option.map (fun (room : Database.room) -> room.Database.name)
    |> Option.value ~default:""
  in
  let subscribe identifier (channel, signed_name) =
    match database_read (fun () ->
      authorize_cable_subscription database identity secret (channel, signed_name)) with
    | None ->
        write_json
          (`Assoc [ ("identifier", `String identifier); ("type", `String "reject_subscription") ])
    | Some target ->
        if not (List.exists (fun (known, _, _) -> known = identifier) !subscriptions) then
          let subscription =
            match target with
            | `Sidebar -> None
            | `Room room_id ->
                Some
                  (Cable_bus.subscribe message_bus ~room_id ~identifier ~queue:events)
            | `Unread user_id ->
                Some
                  (Cable_bus.subscribe_unreads message_bus ~user_id ~identifier
                     ~queue:events)
            | `Read user_id ->
                Some
                  (Cable_bus.subscribe_reads message_bus ~user_id ~identifier
                     ~queue:events)
            | `Presence room_id ->
                database_write (fun () ->
                  ignore
                    (Database.membership_present database ~room_id
                       ~user_id:identity.Database.user.Database.id
                       ~timestamp:(timestamp_now ())));
                Cable_bus.publish_read message_bus
                  ~user_id:identity.Database.user.Database.id ~room_id;
        None
            | `Control -> None
          in
          subscriptions := (identifier, target, subscription) :: !subscriptions;
        write_json
          (`Assoc [ ("identifier", `String identifier); ("type", `String "confirm_subscription") ])
  in
  let notify () =
    while true do
      match Eio.Stream.take events with
      | Cable_bus.Message (identifier, room_id, (message : Database.message))
        when List.exists (fun (known, _, _) -> known = identifier) !subscriptions ->
          let html =
            "<turbo-stream action=\"append\" target=\"room_"
            ^ string_of_int room_id ^ "_messages\"><template>"
            ^ render_message_item ~secret ~room_name:(room_name room_id)
                identity.Database.user room_id csrf message
            ^ "</template></turbo-stream>"
          in
          send_stream identifier html
      | Cable_bus.Replace (identifier, room_id, (message : Database.message))
        when List.exists (fun (known, _, _) -> known = identifier) !subscriptions ->
      let html =
            "<turbo-stream action=\"replace\" target=\"message_"
            ^ html_escape message.Database.client_message_id ^ "\"><template>"
            ^ render_message_item ~secret ~room_name:(room_name room_id)
                identity.Database.user room_id csrf message
            ^ "</template></turbo-stream>"
          in
          send_stream identifier html
      | Cable_bus.Remove (identifier, message_dom_id)
        when List.exists (fun (known, _, _) -> known = identifier) !subscriptions ->
          send_stream identifier
            ("<turbo-stream action=\"remove\" target=\""
            ^ html_escape message_dom_id ^ "\"></turbo-stream>")
      | Cable_bus.Unread (identifier, room_id)
        when List.exists (fun (known, _, _) -> known = identifier) !subscriptions ->
          write_json
            (`Assoc
              [ ("identifier", `String identifier);
                ("message", `Assoc [ ("roomId", `Int room_id) ]) ])
      | Cable_bus.Read (identifier, room_id)
        when List.exists (fun (known, _, _) -> known = identifier) !subscriptions ->
          write_json
            (`Assoc
              [ ("identifier", `String identifier);
                ("message", `Assoc [ ("room_id", `Int room_id) ]) ])
      | _ -> ()
    done
  in
  let presence_action identifier action =
    match List.find_opt (fun (known, _, _) -> known = identifier) !subscriptions with
    | Some (_, `Presence room_id, _) ->
        let user_id = identity.Database.user.Database.id in
        let timestamp = timestamp_now () in
        (match action with
        | "present" ->
            database_write (fun () ->
              ignore (Database.membership_present database ~room_id ~user_id ~timestamp));
            Cable_bus.publish_read message_bus ~user_id ~room_id
        | "absent" ->
            database_write (fun () ->
              ignore (Database.membership_absent database ~room_id ~user_id ~timestamp))
        | "refresh" ->
            database_write (fun () ->
              ignore (Database.membership_refresh database ~room_id ~user_id ~timestamp))
        | _ -> ())
    | _ -> ()
  in
  Eio.Switch.run (fun sw ->
      write_json (`Assoc [ ("type", `String "welcome") ]);
      Eio.Fiber.fork ~sw (fun () -> try notify () with _ -> ());
      Eio.Fiber.fork ~sw (fun () ->
          let rec heartbeat () =
            Eio.Time.sleep env#clock 3.0;
            write_json
              (`Assoc
                [ ("type", `String "ping");
                  ("message", `Int (int_of_float (Unix.gettimeofday ()))) ]);
            heartbeat ()
          in
          try heartbeat () with _ -> ());
      let rec loop () =
        let frame = Websocket.read_frame_buffered ic in
        (match frame with
        | { Websocket.opcode = 8; payload } ->
            (try write_frame 8 payload with _ -> ());
            raise Websocket.Closed
        | { Websocket.opcode = 9; payload } -> write_frame 10 payload
        | { Websocket.opcode = 10; _ } -> ()
        | { Websocket.opcode = 1; payload } ->
            (try
               match Yojson.Basic.from_string payload with
               | `Assoc fields ->
                   (match (List.assoc_opt "command" fields, List.assoc_opt "identifier" fields) with
                   | Some (`String "subscribe"), Some (`String identifier) ->
                       (match cable_identifier identifier with
                       | Some params -> subscribe identifier params
                       | None ->
                           write_json
                             (`Assoc
                               [ ("identifier", `String identifier);
                                 ("type", `String "reject_subscription") ]))
                   | Some (`String "unsubscribe"), Some (`String identifier) ->
                       List.iter
                         (fun (known, target, subscription) ->
                           if known = identifier then (
                             Option.iter Cable_bus.unsubscribe subscription;
                             match target with
                             | `Presence room_id ->
                                 database_write (fun () ->
                                   ignore
                                     (Database.membership_absent database ~room_id
                                        ~user_id:identity.Database.user.Database.id
                                        ~timestamp:(timestamp_now ())))
                             | _ -> ()))
                         !subscriptions;
                       subscriptions := List.filter (fun (known, _, _) -> known <> identifier) !subscriptions;
                       write_json
                         (`Assoc [ ("identifier", `String identifier); ("type", `String "confirm_unsubscription") ])
                   | Some (`String "message"), Some (`String identifier) ->
                       (match List.assoc_opt "data" fields with
                       | Some (`String data) ->
                           (try
                              match Yojson.Basic.from_string data with
                              | `Assoc payload ->
                                  (match List.assoc_opt "action" payload with
                                  | Some (`String action) -> presence_action identifier action
                                  | _ -> ())
                              | _ -> ()
                            with _ -> ())
                       | _ -> ())
                   | _ -> ())
               | _ -> ()
             with error -> prerr_endline ("Action Cable command rejected: " ^ Printexc.to_string error))
        | _ -> raise Websocket.Closed);
        loop ()
      in
      Fun.protect
        ~finally:(fun () ->
          List.iter
            (fun (_, target, subscription) ->
              Option.iter Cable_bus.unsubscribe subscription;
              match target with
              | `Presence room_id ->
                  database_write (fun () ->
                    ignore
                      (Database.membership_absent database ~room_id
                         ~user_id:identity.Database.user.Database.id
                         ~timestamp:(timestamp_now ())))
              | _ -> ())
            !subscriptions)
        (fun () -> try loop () with _ -> ()))

let serve_cable ~env ~database ~message_bus ~database_lock ~secret request =
  let headers = Cohttp.Request.headers request in
  let invalid status body = `Response (response status body) in
  let key = Cohttp.Header.get headers "sec-websocket-key" in
  let version = Cohttp.Header.get headers "sec-websocket-version" in
  let protocols = Cohttp.Header.get_multi headers "sec-websocket-protocol" in
  let supports_actioncable =
    protocols |> List.concat_map (String.split_on_char ',')
    |> List.exists (fun protocol -> String.trim protocol = "actioncable-v1-json")
  in
  if Cohttp.Request.meth request <> `GET
     || not (header_has_token headers "upgrade" "websocket")
     || not (header_has_token headers "connection" "upgrade")
     || version <> Some "13" || not supports_actioncable || not (valid_origin headers)
  then invalid `Bad_request "Invalid Action Cable WebSocket upgrade"
  else
    match (key, current_identity database secret headers) with
    | None, _ -> invalid `Bad_request "Missing WebSocket key"
    | _, None -> invalid `Unauthorized "Authentication required"
    | Some key, Some identity ->
        (match Rails_crypto.base64_decode key with
        | Some nonce when String.length nonce = 16 ->
            let response_headers =
              Cohttp.Header.of_list
                [ ("upgrade", "websocket"); ("connection", "Upgrade");
                  ("sec-websocket-accept", Websocket.websocket_accept key);
                  ("sec-websocket-protocol", "actioncable-v1-json") ]
            in
            let response =
              Cohttp.Response.make ~status:`Switching_protocols ~headers:response_headers ()
            in
            let session = load_session secret headers in
            `Expert
              (response, fun ic oc ->
                try
                  cable_websocket env database message_bus database_lock identity secret
                    session.Session.csrf_form_token ic oc
                with _ -> ())
        | _ -> invalid `Bad_request "Invalid WebSocket key")

let remote_ip = function
  | ((_, (`Tcp (address, _))), _) ->
      Format.asprintf "%a" Eio.Net.Ipaddr.pp address
  | _ -> ""

let serve ~database ~jobs_database ~port ~domains =
  Eio_main.run (fun env ->
      let message_bus = Cable_bus.create () in
      let database_lock = Eio.Mutex.create () in
      let server =
        Cohttp_eio.Server.make_response_action
          ~callback:(fun connection request body ->
            if path_of_request request = "/cable" then
              Eio.Mutex.use_ro database_lock (fun () ->
                match secret_key_base () with
                | None -> `Response (response `Internal_server_error "SECRET_KEY_BASE is required")
                | Some secret ->
                    serve_cable ~env ~database ~message_bus ~database_lock ~secret request)
            else
              let respond () =
                `Response
                  (serve_request ~database ~jobs_database ~message_bus
                     ~remote_ip:(remote_ip connection) request body)
              in
              match Cohttp.Request.meth request with
              | `GET | `HEAD ->
                  Eio.Mutex.use_ro database_lock respond
              | _ -> Eio.Mutex.use_rw ~protect:true database_lock respond)
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
