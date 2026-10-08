let response ?(headers = Cohttp.Header.init ()) ?(gzip = false) status body =
  let content_type =
    Cohttp.Header.get headers "content-type" |> Option.value ~default:""
  in
  let textual = Gzip.textual_content_type content_type in
  let headers =
    if textual then Cohttp.Header.add headers "vary" "Accept-Encoding" else headers
  in
  let body, compressed = Gzip.encode ~accepted:gzip ~content_type body in
  let headers =
    if compressed then Cohttp.Header.add headers "content-encoding" "gzip" else headers
  in
  Cohttp_eio.Server.respond_string ~status ~headers ~body ()

let json_response ?(gzip = false) status value =
  response ~gzip ~headers:(Cohttp.Header.init_with "content-type" "application/json; charset=utf-8")
    status (Yojson.Basic.to_string value)

let html_headers =
  Cohttp.Header.init_with "content-type" "text/html; charset=utf-8"

let current_custom_styles = ref None

let inject_custom_styles body =
  match !current_custom_styles with
  | None -> body
  | Some "" -> body
  | Some styles ->
      let closing = "</head>" in
      let rec find index =
        if index + String.length closing > String.length body then None
        else if String.sub body index (String.length closing) = closing then Some index
        else find (index + 1)
      in
      (match find 0 with
      | None -> body
      | Some index ->
          String.sub body 0 index
          ^ "<style data-turbo-track=\"reload\">" ^ styles ^ "</style>"
          ^ String.sub body index (String.length body - index))

let redirect ?(headers = Cohttp.Header.init ()) target =
  response ~headers:(Cohttp.Header.add headers "location" target) `Found ""

let trim = String.trim

exception Request_body_too_large

let header headers name =
  Cohttp.Header.get headers name |> Option.value ~default:""

let gzip_requested headers =
  Gzip.accepts_encoding (header headers "accept-encoding")

let request_scheme headers =
  Forwarded_request.scheme (header headers "x-forwarded-proto")

let valid_origin headers =
  match Cohttp.Header.get headers "origin" with
  | None -> true
  | Some origin ->
      Forwarded_request.valid_origin ~origin ~host:(header headers "host")
        ~forwarded_proto:(header headers "x-forwarded-proto")

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

let parse_request_form headers body =
  match Multipart.boundary_of_content_type (header headers "content-type") with
  | Some boundary -> Multipart.parse ~boundary body
  | None -> (parse_form body, [])

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

let request_origin headers =
  let scheme = request_scheme headers in
  let host = header headers "host" in
  if host = "" || String.exists (function '\r' | '\n' | '/' | '\\' -> true | _ -> false) host
  then scheme ^ "://localhost"
  else scheme ^ "://" ^ host

let qr_code_path_for_url url =
  "/qr_code/" ^ Rails_crypto.base64url_encode url

let set_cookie headers value = Cohttp.Header.add headers "set-cookie" value

let attach_session_cookie ?(secure = false) headers ~secret session =
  match Session.set_cookie ~secure ~secret session with
  | None -> headers
  | Some cookie -> set_cookie headers cookie

let respond_with_session ?(headers = Cohttp.Header.init ()) ?(secure = false) ?(gzip = false) ~secret session status body =
  response ~gzip ~headers:(attach_session_cookie ~secure headers ~secret session) status body

let html_with_session ?(headers = html_headers) ?(secure = false) ?(gzip = false) ~secret session status body =
  respond_with_session ~headers ~secure ~gzip ~secret session status (inject_custom_styles body)

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

let request_body ?(max_size = 1_048_576) source =
  let buffer = Buffer.create 1024 in
  let chunk = Cstruct.create 8192 in
  let rec read () =
    let count = Eio.Flow.single_read source chunk in
    if Buffer.length buffer + count > max_size then raise Request_body_too_large;
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

let html_body_to_text ?secret ?database body =
  let body =
    match (secret, database) with
    | Some secret, Some database ->
        Action_text.sanitize ~secret
          ~resolve_user:(fun user_id ->
            Database.find_user_detail database user_id
            |> Option.map (fun (user : Database.user_detail) -> user.Database.name))
          body
    | _ -> body
  in
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

let safe_message_body ~secret database body =
  Action_text.sanitize ~secret
    ~resolve_user:(fun user_id ->
      Database.find_user_detail database user_id
      |> Option.map (fun (user : Database.user_detail) -> user.Database.name))
    body

let message_edit_form ~secret database csrf room_id (message : Database.message) =
  let body = message.Database.body_html |> html_body_to_text ~secret ~database |> html_escape in
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

let active_storage_representation_url ~secret (attachment : Database.message_attachment)
    ~format ~width ~height =
  let signed_id =
    Rails_crypto.sign_active_storage_blob_id ~secret attachment.Database.blob_id
    |> encode_path_component
  in
  let variation =
    Rails_crypto.sign_active_storage_variation ~secret
      { Rails_crypto.format; width; height }
    |> encode_path_component
  in
  "/rails/active_storage/representations/redirect/" ^ signed_id ^ "/"
  ^ variation ^ "/" ^ encode_path_component attachment.Database.filename

let render_message_attachment ~secret (attachment : Database.message_attachment) =
  let url = active_storage_blob_url ~secret attachment in
  let filename = html_escape attachment.Database.filename in
  let width, height = (attachment.Database.width, attachment.Database.height) in
  let dimensions = Attachment_presentation.preview_dimensions ~width ~height in
  let image_dimensions =
    match dimensions with
    | None -> ""
    | Some (width, height) ->
        " width=\"" ^ Attachment_presentation.number width ^ "\" height=\""
        ^ Attachment_presentation.number height ^ "\""
  in
  let wrap_preview html =
    Attachment_presentation.wrap_preview ~width ~height html
  in
  let representation_url ~format ~width ~height =
    active_storage_representation_url ~secret attachment ~format ~width ~height
  in
  if String.starts_with ~prefix:"image/" attachment.Database.content_type then
    let format =
      match String.lowercase_ascii attachment.Database.content_type with
      | "image/jpeg" -> "jpg"
      | "image/webp" -> "webp"
      | _ -> "png"
    in
    let thumbnail = representation_url ~format ~width:1200 ~height:800 in
    wrap_preview
      ("<a href=\"" ^ url
       ^ "\" data-lightbox-target=\"image\"><img class=\"message__attachment\" src=\""
       ^ thumbnail ^ "\" alt=\"" ^ filename ^ "\"" ^ image_dimensions
       ^ " loading=\"lazy\"></a>")
  else if String.starts_with ~prefix:"video/" attachment.Database.content_type then
    let poster = representation_url ~format:"webp" ~width:1200 ~height:800 in
    wrap_preview
      ("<video src=\"" ^ url ^ "\" poster=\"" ^ poster
       ^ "\" controls preload=\"none\" width=\"100%\" height=\"100%\" class=\"message__attachment\"></video>")
  else if attachment.Database.content_type = "application/pdf" then
    let preview = representation_url ~format:"png" ~width:1200 ~height:800 in
    wrap_preview
      ("<a href=\"" ^ url
       ^ "\" data-lightbox-target=\"image\"><img class=\"message__attachment\" src=\""
       ^ preview ^ "\" alt=\"" ^ filename ^ "\"" ^ image_dimensions
       ^ " loading=\"lazy\"></a>")
  else
    "<a href=\"" ^ url ^ "?disposition=attachment\">" ^ filename ^ "</a>"

let render_message_item ~secret ~database ?(room_name = "") ?(boosts = [])
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
  let timestamp_value = message.Database.created_at in
  let timestamp = html_escape timestamp_value in
  let timestamp_epoch =
    Timestamp.epoch_milliseconds timestamp_value
    |> Option.value ~default:0 |> string_of_int
  in
  let timestamp_iso =
    Timestamp.iso8601_utc timestamp_value
    |> Option.value ~default:timestamp_value |> html_escape
  in
  let updated_timestamp_epoch =
    Timestamp.epoch_milliseconds message.Database.updated_at
    |> Option.value ~default:0 |> string_of_int
  in
  let escaped_id = html_escape message.Database.client_message_id in
  let actions =
    let path =
      "/rooms/" ^ string_of_int room_id ^ "/messages/"
      ^ string_of_int message.Database.id
    in
    let reaction_forms =
      [ ("👍", "Thumbs up"); ("👏", "Clapping"); ("👋", "Waving hand");
        ("💪", "Muscle"); ("❤️", "Red heart"); ("😂", "Face with tears of joy");
        ("🎉", "Party popper"); ("🔥", "Fire") ]
      |> List.map (fun (emoji, label) ->
             "<form data-turbo-frame=\"boosting_message_" ^ escaped_id
             ^ "\" data-action=\"popup#close\" action=\"/messages/"
             ^ string_of_int message.Database.id
             ^ "/boosts\" accept-charset=\"UTF-8\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
             ^ html_escape csrf ^ "\"><input type=\"hidden\" id=\"boost_content\" name=\"boost[content]\" value=\""
             ^ emoji ^ "\"><button name=\"button\" type=\"submit\" title=\"" ^ label
             ^ "\" class=\"btn message__action-btn\""
             ^ " data-emoji=\"" ^ emoji ^ "\"><figure class=\"margin-none boost-character\">"
             ^ emoji ^ "</figure><span class=\"for-screen-reader\">" ^ label
             ^ "</span></button></form>")
      |> String.concat ""
    in
    let edit_link =
      if user.Database.role = 1 || user.Database.id = message.Database.creator_id then
        "<a href=\"" ^ path ^ "/edit\" class=\"btn message__action-btn center full-width message__edit-btn\" data-turbo-frame=\"edit_message_"
        ^ escaped_id ^ "\" title=\"Edit\" aria-label=\"Edit\"><img class=\"colorize--black\" aria-hidden=\"true\" src=\"/assets/pencil.svg\" width=\"20\" height=\"20\"></a>"
      else ""
    in
    "<div class=\"message__actions\" data-controller=\"soft-keyboard\"><details class=\"position-relative\" data-controller=\"popup\" data-action=\"keydown.esc-&gt;popup#close toggle-&gt;popup#toggle click@document-&gt;popup#closeOnClickOutside\" data-popup-orientation-top-class=\"popup-orientation-top\"><summary class=\"btn message__action-btn message__options-btn\"><img class=\"colorize--black\" aria-hidden=\"true\" src=\"/assets/menu-dots-horizontal.svg\" width=\"20\" height=\"20\"><span class=\"for-screen-reader\">Message options</span></summary><div class=\"message__actions-menu border shadow\" data-popup-target=\"menu\"><div class=\"quick-boosts\">"
    ^ reaction_forms ^ "<a href=\"/messages/"
    ^ string_of_int message.Database.id ^ "/boosts/new\" class=\"btn message__action-btn message__boost-btn\" data-turbo-frame=\"new_boost_message_"
    ^ escaped_id ^ "\" data-action=\"soft-keyboard#open popup#close\"><img class=\"colorize--black\" aria-hidden=\"true\" src=\"/assets/boost.svg\" width=\"20\" height=\"20\"><span class=\"for-screen-reader\">New boost</span></a></div><div class=\"flex flex-wrap border-top margin-block-start-half pad-block-start-half message__actions-grid\"><button type=\"button\" class=\"btn message__action-btn center full-width\" data-action=\"reply#reply\" title=\"Reply\" aria-label=\"Reply\"><img class=\"colorize--black\" aria-hidden=\"true\" src=\"/assets/reply.svg\" width=\"20\" height=\"20\"></button><button type=\"button\" class=\"btn message__action-btn center full-width\" title=\"Copy link\" aria-label=\"Copy link\" data-controller=\"copy-to-clipboard\" data-action=\"copy-to-clipboard#copy\" data-copy-to-clipboard-success-class=\"btn--success\" data-copy-to-clipboard-content-value=\""
    ^ permalink ^ "\"><img class=\"colorize--black\" aria-hidden=\"true\" src=\"/assets/link.svg\" width=\"20\" height=\"20\"></button>" ^ edit_link
    ^ "</div></div></details></div>"
  in
  "<div id=\"message_" ^ escaped_id
  ^ "\" class=\"message\" data-controller=\"reply\" data-user-id=\""
  ^ string_of_int message.Database.creator_id ^ "\" data-message-id=\""
  ^ string_of_int message.Database.id
  ^ "\" data-message-timestamp=\"" ^ timestamp_epoch
  ^ "\" data-message-updated-at=\"" ^ updated_timestamp_epoch ^ "\" data-sort-value=\""
  ^ timestamp_epoch ^ "\" data-messages-target=\"message\" data-search-results-target=\"message\" data-refresh-room-target=\"message\" data-reply-composer-outlet=\"#composer\"><h2 class=\"message__day-separator\"><time datetime=\""
  ^ timestamp_iso ^ "\" data-local-time-target=\"date\">" ^ timestamp
  ^ "</time></h2><figure class=\"avatar message__avatar\"><a href=\"/users/"
  ^ string_of_int message.Database.creator_id ^ "\" title=\""
  ^ html_escape message.Database.creator_name ^ "\" data-turbo-frame=\"_top\"><img aria-hidden=\"true\" src=\"/users/"
  ^ avatar_token
  ^ "/avatar\" width=\"48\" height=\"48\"></a></figure><turbo-frame id=\"edit_message_"
  ^ escaped_id ^ "\"><div class=\"message__body\"><div class=\"message__body-content\"><div class=\"message__meta\"><h3 class=\"message__heading\"><span class=\"message__author\" title=\""
  ^ html_escape message.Database.creator_name ^ "\"><strong data-reply-target=\"author\">"
  ^ html_escape message.Database.creator_name
  ^ "</strong></span><a class=\"message__permalink\" href=\"" ^ permalink
  ^ "\" target=\"_top\"><time class=\"message__timestamp\" datetime=\""
  ^ timestamp_iso ^ "\" data-local-time-target=\"time\">" ^ timestamp
  ^ "</time></a><span class=\"message__room\"><a href=\"" ^ permalink
  ^ "\" target=\"_top\" data-reply-target=\"link\">" ^ html_escape room_name
  ^ "</a></span></h3>" ^ actions ^ "</div><div id=\"presentation_" ^ escaped_id
  ^ "\" dir=\"auto\" data-reply-target=\"body\" data-messages-target=\"body\">"
  ^ safe_message_body ~secret database message.Database.body_html
  ^ (attachments |> List.map (render_message_attachment ~secret) |> String.concat "")
  ^ "</div><turbo-frame id=\"boosting_message_" ^ escaped_id
  ^ "\"><div class=\"boosts flex flex-wrap align-center gap full-width\" data-controller=\"turbo-streaming\" data-action=\"turbo:submit-start->turbo-streaming#unsubscribe\"><div class=\"flex-inline flex-wrap gap\" id=\"boosts_message_"
  ^ escaped_id ^ "\" data-turbo-streaming-target=\"container\">"
  ^ (boosts |> List.map (render_boost ~secret) |> String.concat "")
  ^ "</div><turbo-frame id=\"new_boost_message_" ^ escaped_id
  ^ "\"><div class=\"flex-inline message__boost-inline\" data-controller=\"soft-keyboard\"><a class=\"boost__action txt-small btn\" href=\"/messages/"
  ^ string_of_int message.Database.id ^ "/boosts/new\" data-action=\"soft-keyboard#open\"><span class=\"for-screen-reader\">Add a boost</span></a></div></turbo-frame></div></turbo-frame></div></div></turbo-frame></div>"

let render_live_message ~secret ~database ~room_name ~boosts ~attachments user room_id csrf
    (message : Database.message) =
  render_message_item ~secret ~database ~room_name ~boosts ~attachments user room_id csrf
    message

let live_message_details database (message : Database.message) =
  let boosts =
    Database.boosts_for_messages database [ message ]
    |> List.assoc_opt message.Database.id |> Option.value ~default:[]
  in
  let attachments =
    Database.attachments_for_messages database [ message ]
    |> List.assoc_opt message.Database.id |> Option.to_list
  in
  (boosts, attachments)

let create_message_id () =
  let hex = Rails_crypto.random_bytes 16 |> Rails_crypto.hex in
  String.sub hex 0 8 ^ "-" ^ String.sub hex 8 4 ^ "-"
  ^ String.sub hex 12 4 ^ "-" ^ String.sub hex 16 4 ^ "-"
  ^ String.sub hex 20 12

let dispatch_message_push =
  ref (fun ~sw:_ ~process_mgr:_ ~database:_ ~database_lock:_ ~secret:_ ~room:_
         ~creator_id:_ ~body:_ ~message:_ ~timestamp:_ -> ())

let iso8601_millis timestamp =
  let normalized = String.concat "T" (String.split_on_char ' ' timestamp) in
  let millis =
    match String.index_opt normalized '.' with
    | Some dot when String.length normalized >= dot + 4 ->
        String.sub normalized 0 (dot + 4)
    | _ -> normalized
  in
  millis ^ "Z"

let bot_message_json ~secret ~database ?attachment_filename room_id (message : Database.message) =
  let plain_text = String.trim (html_body_to_text ~secret ~database message.Database.body_html) in
  let plain_text =
    if plain_text = "" then Option.value attachment_filename ~default:"" else plain_text
  in
  `Assoc
    [ ("id", `Int message.Database.id);
      ("body",
       `Assoc [ ("plain_text", `String plain_text);
                ("html", `String message.Database.body_html) ]);
      ("created_at", `String (iso8601_millis message.Database.created_at));
      ("creator",
       `Assoc
         [ ("id", `Int message.Database.creator_id);
           ("name", `String message.Database.creator_name);
           ("role", `String "bot") ]);
      ("room", `Assoc [ ("id", `Int room_id) ]);
      ("url", `String (Printf.sprintf "/rooms/%d/messages/%d" room_id message.Database.id)) ]

let bot_messages_request ~sw ~process_mgr ~database_lock ~store_upload ~cleanup_upload ~remove_file message_bus database secret
    request body room_id bot_key message_id =
  match Database.authenticate_bot database (String.trim bot_key) with
  | None -> response `Unauthorized "Authentication required"
  | Some bot ->
      (match Database.find_room_for_user database bot.Database.id room_id with
      | None -> response `Not_found "Room not found"
      | Some room ->
          let method_ = Cohttp.Request.meth request in
          let params = query_parameters request in
          let find_message id = Database.find_message database room_id id in
          let json_message (message : Database.message) =
            let attachment_filename =
              Database.attachments_for_messages database [ message ]
              |> List.assoc_opt message.Database.id
              |> Option.map (fun (attachment : Database.message_attachment) -> attachment.Database.filename)
            in
            bot_message_json ~secret ~database ?attachment_filename room_id message
          in
          let publish_message replace message =
            let boosts, attachments = live_message_details database message in
            if replace then
              Cable_bus.publish_replace message_bus ~room_id ~room_name:room.Database.name
                ~boosts ~attachments message
            else
              Cable_bus.publish message_bus ~room_id ~room_name:room.Database.name
                ~boosts ~attachments message
          in
          let publish_unread () =
            Database.room_member_ids database room_id
            |> List.iter (fun user_id ->
                   if user_id <> bot.Database.id then
                     Cable_bus.publish_unread message_bus ~user_id ~room_id)
          in
          match (method_, message_id) with
          | `GET, None ->
              let before = Option.bind (List.assoc_opt "before" params) int_of_string_opt in
              let after = Option.bind (List.assoc_opt "after" params) int_of_string_opt in
              let messages = Database.messages_for_room ?before ?after database room_id in
              let count = Database.message_count_for_room database room_id in
              let headers =
                Cohttp.Header.init_with "content-type" "application/json; charset=utf-8"
                |> fun headers -> Cohttp.Header.add headers "x-total-count" (string_of_int count)
              in
              let link =
                match messages with
                | [] -> None
                | _ when after <> None ->
                    let last = List.hd (List.rev messages) in
                    if Database.has_messages_after database room_id last.Database.id then
                      Some (Printf.sprintf "</rooms/%d/%s/messages?after=%d>; rel=\"next\""
                              room_id bot_key last.Database.id)
                    else None
                | first :: _ ->
                    if Database.has_messages_before database room_id first.Database.id then
                      Some (Printf.sprintf "</rooms/%d/%s/messages?before=%d>; rel=\"next\""
                              room_id bot_key first.Database.id)
                    else None
              in
              let headers = Option.fold ~none:headers
                  ~some:(fun link -> Cohttp.Header.add headers "link" link) link in
              response ~headers `OK
                (Yojson.Basic.to_string (`List (List.map json_message messages)))
          | `POST, None ->
              let text = request_body body in
              let _, files = parse_request_form (Cohttp.Request.headers request) text in
              let attachment_file =
                List.find_opt (fun (file : Multipart.file_part) -> file.Multipart.name = "attachment") files
              in
              if String.trim text = "" && attachment_file = None then
                response `Unprocessable_entity "Message body or attachment is required"
              else
                let uploaded = Option.map (store_upload ~user_id:bot.Database.id) attachment_file in
                let message_body = if attachment_file = None then text else "" in
                (try
                   let attachment_blob_id = Option.map fst uploaded in
                   match Database.create_message_with_id ~secret ?attachment_blob_id database ~room_id
                           ~creator_id:bot.Database.id ~body:message_body
                           ~client_message_id:(create_message_id ())
                           ~timestamp:(timestamp_now ()) with
                   | None ->
                       Option.iter cleanup_upload uploaded;
                       response `Service_unavailable "Campfire database is unavailable"
                   | Some message_id ->
                       (match find_message message_id with
                       | None -> response `Internal_server_error "Message not found"
                       | Some message ->
                           !dispatch_message_push ~sw ~process_mgr ~database ~database_lock
                             ~secret ~room ~creator_id:bot.Database.id ~body:message_body
                             ~message ~timestamp:message.Database.created_at;
                           publish_message false message;
                           publish_unread ();
                           let headers =
                             Cohttp.Header.init_with "content-type" "application/json; charset=utf-8"
                             |> fun headers -> Cohttp.Header.add headers "location"
                                  (Printf.sprintf "/rooms/%d/messages/%d" room_id message_id)
                           in
                           response ~headers `Created (Yojson.Basic.to_string (json_message message)))
                 with error ->
                   Option.iter cleanup_upload uploaded;
                   raise error)
          | (`PATCH | `PUT), Some message_id ->
              (match find_message message_id with
              | None -> response `Not_found "Message not found"
              | Some message when message.Database.creator_id <> bot.Database.id ->
                  response `Forbidden "Forbidden"
              | Some _ ->
                  let text = request_body body in
                  (try
                     Database.update_message ~secret database ~room_id ~message_id
                       ~user_id:bot.Database.id ~role:bot.Database.role ~body:text
                       ~timestamp:(timestamp_now ());
                     (match find_message message_id with
                     | None -> response `Not_found "Message not found"
                     | Some updated ->
                         publish_message true updated;
                         response `OK (Yojson.Basic.to_string (json_message updated)))
                   with Database.Message_not_authorized -> response `Forbidden "Forbidden"
                      | _ -> response `Unprocessable_entity "Message could not be saved"))
          | `DELETE, Some message_id ->
              (match find_message message_id with
              | None -> response `Not_found "Message not found"
              | Some message when message.Database.creator_id <> bot.Database.id ->
                  response `Forbidden "Forbidden"
              | Some message ->
                  (try
                     let files = Database.delete_message database ~room_id ~message_id
                         ~user_id:bot.Database.id ~role:bot.Database.role in
                     List.iter remove_file files;
                     Cable_bus.publish_remove message_bus ~room_id
                       ~message_dom_id:message.Database.client_message_id;
                     response `No_content ""
                   with Database.Message_not_authorized -> response `Forbidden "Forbidden"
                      | _ -> response `Unprocessable_entity "Message could not be deleted"))
          | _ -> response `Method_not_allowed "Method not allowed")

let bot_boost_request message_bus database secret request body room_id bot_key message_id boost_id =
  match Database.authenticate_bot database (String.trim bot_key) with
  | None -> response `Unauthorized "Authentication required"
  | Some bot ->
      (match Database.find_room_for_user database bot.Database.id room_id with
      | None -> response `Not_found "Room not found"
      | Some _ ->
          (match Database.find_message database room_id message_id with
          | None -> response `Not_found "Message not found"
          | Some message ->
              let avatar_token =
                Rails_crypto.sign_user_avatar_id ~secret bot.Database.id
                |> encode_path_component
              in
              let json boost_id content created_at =
                `Assoc
                  [ ("id", `Int boost_id);
                    ("content", `String content);
                    ("created_at", `String (iso8601_millis created_at));
                    ("booster", `Assoc
                      [ ("id", `Int bot.Database.id);
                        ("name", `String bot.Database.name);
                        ("role", `String "bot");
                        ("avatar_url", `String ("/users/" ^ avatar_token ^ "/avatar")) ]);
                    ("message", `Assoc
                      [ ("id", `Int message_id);
                        ("url", `String (Printf.sprintf "/rooms/%d/messages/%d" room_id message_id)) ]) ]
              in
              match (Cohttp.Request.meth request, boost_id) with
              | `POST, None ->
                  let content = request_body body |> String.trim in
                  if content = "" then response `Unprocessable_entity "Boost content is required"
                  else
                    (match Database.create_boost database ~message_id
                             ~booster_id:bot.Database.id ~content ~timestamp:(timestamp_now ()) with
                    | None -> response `Service_unavailable "Campfire database is unavailable"
                    | Some id ->
                        let boost =
                          Database.boosts_for_messages database [ message ]
                          |> List.assoc_opt message_id |> Option.value ~default:[]
                          |> List.find_opt (fun (boost : Database.boost) -> boost.Database.id = id)
                        in
                        (match boost with
                        | None -> response `Internal_server_error "Boost not found"
                        | Some boost ->
                            Cable_bus.publish_boost_append message_bus ~room_id
                              ~target:("boosts_message_" ^ message.Database.client_message_id)
                              ~html:(render_boost ~secret boost);
                            json_response `Created
                              (json boost.Database.id boost.Database.content boost.Database.created_at)))
              | `DELETE, Some boost_id ->
                  if Database.delete_boost database ~boost_id ~message_id
                       ~booster_id:bot.Database.id
                  then (
                    Cable_bus.publish_boost_remove message_bus ~room_id
                      ~boost_dom_id:("boost_" ^ string_of_int boost_id);
                    response `No_content "")
                  else response `Not_found "Boost not found"
              | _ -> response `Method_not_allowed "Method not allowed"))

let room_page ~secret ~database (user : Database.user)
    (sidebar : Database.sidebar_room list)
    (current : Database.room) csrf (messages : Database.message list)
    ~boosts_by_message ~attachments_by_message ~older_messages ~newer_messages ~room_stream =
  let sidebar_links direct =
    sidebar
    |> List.filter (fun (entry : Database.sidebar_room) ->
           (entry.Database.room.Database.kind = "Rooms::Direct") = direct)
    |> List.map (fun (entry : Database.sidebar_room) ->
           let room = entry.Database.room in
           let room_class = if direct then "direct" else "room" in
           let unread_class = if entry.Database.unread then " unread" else "" in
           "<a id=\"room_" ^ string_of_int room.Database.id
           ^ "_list\" class=\"btn align-center gap txt-nowrap " ^ room_class
           ^ unread_class ^ "\" data-room-id=\"" ^ string_of_int room.Database.id
           ^ "\" href=\"/rooms/" ^ string_of_int room.Database.id ^ "\">"
           ^ html_escape room.Database.name ^ "</a>")
    |> String.concat ""
  in
  let global_sidebar_stream =
    Rails_crypto.sign_turbo_stream_name ~secret "rooms"
  in
  let user_gid =
    Rails_crypto.base64url_encode
      ("gid://campfire/User/" ^ string_of_int user.Database.id)
  in
  let user_sidebar_stream =
    Rails_crypto.sign_turbo_stream_name ~secret (user_gid ^ ":rooms")
  in
  "<!doctype html><html><head><meta charset=\"utf-8\"><title>"
  ^ html_escape current.Database.name
  ^ " · Campfire</title><meta name=\"viewport\" content=\"width=device-width, initial-scale=1, user-scalable=no, interactive-widget=resizes-content\"><meta name=\"view-transition\" content=\"same-origin\"><meta name=\"color-scheme\" content=\"light dark\"><meta name=\"theme-color\" content=\"#ffffff\" media=\"(prefers-color-scheme: light)\"><meta name=\"theme-color\" content=\"#000000\" media=\"(prefers-color-scheme: dark)\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
  ^ html_escape csrf
  ^ "\"><meta name=\"turbo-prefetch\" content=\"true\"><link rel=\"manifest\" href=\"/webmanifest\"><link rel=\"icon\" href=\"/account/logo?size=small\" type=\"image/png\"><link rel=\"apple-touch-icon\" href=\"/account/logo?size=small\"><link rel=\"stylesheet\" href=\"/assets/campfire.css\" data-turbo-track=\"reload\"><link rel=\"mask-icon\" href=\"/assets/campfire.svg\"><script type=\"importmap\">{\"imports\":{\"application\":\"/assets/application.js\"}}</script><link rel=\"modulepreload\" href=\"/assets/application.js\"><script type=\"module\">import \"application\"</script></head><body class=\"application\" data-controller=\"local-time lightbox\"><turbo-cable-stream-source channel=\"Turbo::StreamsChannel\" signed-stream-name=\""
  ^ html_escape global_sidebar_stream
  ^ "\"></turbo-cable-stream-source><turbo-cable-stream-source channel=\"Turbo::StreamsChannel\" signed-stream-name=\""
  ^ html_escape user_sidebar_stream
  ^ "\"></turbo-cable-stream-source><a href=\"#main-content\" class=\"skip-navigation btn\">Skip to main content</a><nav id=\"nav\"><span class=\"btn btn--reversed btn--faux room--current\"><h1 class=\"room__contents txt-medium overflow-ellipsis\">"
  ^ html_escape current.Database.name
  ^ "</h1></span><a class=\"btn\" href=\"/rooms/"
  ^ string_of_int current.Database.id ^ "/involvement\">Notification preferences</a>"
  ^ (if current.Database.kind = "Rooms::Direct" then ""
     else if current.Database.creator_id = user.Database.id || user.Database.role = 1 then
       let kind = if current.Database.kind = "Rooms::Open" then "opens" else "closeds" in
       "<a class=\"btn\" href=\"/rooms/" ^ kind ^ "/"
       ^ string_of_int current.Database.id ^ "/edit\">Settings</a>"
     else "")
  ^ "</nav><main id=\"main-content\" data-room-id=\"" ^ string_of_int current.Database.id
  ^ "\" data-user-id=\"" ^ string_of_int user.Database.id ^ "\"><h1 class=\"for-screen-reader\">"
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
           render_message_item ~secret ~database ~room_name:current.Database.name
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
  ^ "</nav><footer id=\"footer\"><div class=\"composer flex align-end gap position-relative\" data-controller=\"typing-notifications\"><form action=\"/rooms/" ^ string_of_int current.Database.id
  ^ "/messages\" method=\"post\" enctype=\"multipart/form-data\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
  ^ html_escape csrf
  ^ "\"><fieldset data-composer-target=\"fields\"><label>Write a message<textarea name=\"message[body]\" maxlength=\"10000\" aria-label=\"Write a message\"></textarea></label><label>Attach a file<input type=\"file\" name=\"message[attachment]\" multiple></label><button type=\"submit\">Send</button></fieldset></form><div id=\"typing-indicator\" class=\"typing-indicator\" aria-live=\"polite\" hidden></div></div></footer></main><aside id=\"sidebar\" data-controller=\"toggle-class\" data-toggle-class-toggle-class=\"open\"><div class=\"sidebar__container overflow-y overflow-hide-scrollbar\"><a href=\"/searches\" class=\"btn\">Search</a><section class=\"directs\"><a class=\"direct direct__new\" href=\"/rooms/directs/new\">Ping</a><div id=\"direct_rooms\" contents>"
  ^ sidebar_links true
  ^ "</div></section><section class=\"rooms\"><div id=\"shared_rooms\" contents>"
  ^ sidebar_links false
  ^ "</div></section><button class=\"btn sidebar__toggle\" data-action=\"toggle-class#toggle\"><span class=\"for-screen-reader\">Open menu</span></button></div><div class=\"flex align-end sidebar__tools gap justify-end\"><a class=\"btn avatar sidebar__tool\" href=\"/users/me/profile\">"
  ^ html_escape user.Database.name
  ^ "</a><a class=\"btn sidebar__tool\" href=\"/account/edit\">Account Settings</a></div></aside><div id=\"lightbox\" data-controller=\"lightbox\"></div><a href=\"https://once.com\" id=\"app-logo\" target=\"_blank\" aria-label=\"Once software from 37signals home page\"><img src=\"/assets/campfire.png\" alt=\"Campfire logo\" width=\"256\" height=\"216\"></a></body></html>"

let room_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; id ] -> int_of_string_opt id
  | _ -> None

let room_refresh_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; id; "refresh" ] -> int_of_string_opt id
  | _ -> None

let direct_room_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; "directs"; id ] -> int_of_string_opt id
  | _ -> None

let avatar_token_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "users"; token; "avatar" ] when token <> "" -> Some token
  | _ -> None

let avatar_user_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "users"; user_id; "avatar" ] -> int_of_string_opt user_id
  | _ -> None

let user_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "users"; user_id ] -> int_of_string_opt user_id
  | _ -> None

let user_ban_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "users"; user_id; "ban" ] -> int_of_string_opt user_id
  | _ -> None

let push_subscriptions_route_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "users"; "me"; "push_subscriptions" ] -> Some None
  | [ ""; "users"; "me"; "push_subscriptions"; id ] ->
      Option.map (fun id -> Some id) (int_of_string_opt id)
  | _ -> None

let push_test_notification_route_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "users"; "me"; "push_subscriptions"; id; "test_notifications" ] ->
      int_of_string_opt id
  | _ -> None

let account_bot_route_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "account"; "bots" ] -> Some `Index
  | [ ""; "account"; "bots"; "new" ] -> Some `New
  | [ ""; "account"; "bots"; id ] ->
      Option.map (fun id -> `Bot id) (int_of_string_opt id)
  | [ ""; "account"; "bots"; id; "edit" ] ->
      Option.map (fun id -> `Edit id) (int_of_string_opt id)
  | [ ""; "account"; "bots"; id; "key" ] ->
      Option.map (fun id -> `Key id) (int_of_string_opt id)
  | _ -> None

let account_custom_styles_route_of_path path =
  match path with
  | "/account/custom_styles/edit" -> Some `Edit
  | "/account/custom_styles" -> Some `Update
  | _ -> None

let active_storage_blob_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rails"; "active_storage"; "blobs"; "redirect"; signed_id; _filename ] ->
      Some signed_id
  | _ -> None

let active_storage_representation_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rails"; "active_storage"; "representations"; "redirect";
      signed_id; variation; _filename ] ->
      Some (signed_id, variation)
  | _ -> None

let active_storage_disk_token_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rails"; "active_storage"; "disk"; token ] when token <> "" ->
      Rails_crypto.percent_decode token
  | _ -> None

let safe_storage_key key =
  String.length key >= 4
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
         | _ -> false)
       key

let avatar_storage_path key =
  if not (safe_storage_key key) then None
  else
    let root =
      Sys.getenv_opt "CAMPFIRE_STORAGE_PATH"
      |> Option.value ~default:"/rails/storage"
    in
    Some
      (Filename.concat root
         (Filename.concat "files"
            (Filename.concat (String.sub key 0 2)
               (Filename.concat (String.sub key 2 2) key))))

let avatar_variant_path key =
  if not (safe_storage_key key) then None
  else
    let root =
      Sys.getenv_opt "CAMPFIRE_STORAGE_PATH"
      |> Option.value ~default:"/rails/storage"
    in
    Some
      (Filename.concat root
         (Filename.concat "variants/avatars"
            (Filename.concat (String.sub key 0 2)
               (Filename.concat (String.sub key 2 2) (key ^ ".square.webp")))))

let account_logo_variant_path key size =
  if not (safe_storage_key key) then None
  else
    let root =
      Sys.getenv_opt "CAMPFIRE_STORAGE_PATH"
      |> Option.value ~default:"/rails/storage"
    in
    let dimension = if size = `Small then 192 else 512 in
    Some
      (Filename.concat root
         (Filename.concat "variants/account_logos"
            (Filename.concat (String.sub key 0 2)
               (Filename.concat (String.sub key 2 2)
                  (Printf.sprintf "%s-%d.png" key dimension)))))

let remove_storage_file key =
  (match avatar_storage_path key with
  | None -> ()
  | Some _ ->
      let root =
        Sys.getenv_opt "CAMPFIRE_STORAGE_PATH"
        |> Option.value ~default:"/rails/storage"
      in
      let variant_dir =
        Filename.concat root
          (Filename.concat "variants/messages"
             (Filename.concat (String.sub key 0 2) (String.sub key 2 2)))
      in
      let preview_dir =
        Filename.concat root
          (Filename.concat "variants/previews"
             (Filename.concat (String.sub key 0 2) (String.sub key 2 2)))
      in
      let logo_dir =
        Filename.concat root
          (Filename.concat "variants/account_logos"
             (Filename.concat (String.sub key 0 2) (String.sub key 2 2)))
      in
      List.iter
        (fun (directory, prefix) ->
          try
            Sys.readdir directory
            |> Array.iter (fun name ->
                   if String.starts_with ~prefix name then
                     try Sys.remove (Filename.concat directory name) with _ -> ())
          with _ -> ())
        [ (variant_dir, key ^ "-"); (preview_dir, key ^ ".");
          (logo_dir, key ^ "-") ]);
  List.iter
    (function
      | Some path -> (try Sys.remove path with _ -> ())
      | None -> ())
    [ avatar_storage_path key; avatar_variant_path key ]

let ensure_directory path =
  let rec create path =
    if Sys.file_exists path then (
      if not (Sys.is_directory path) then failwith "storage path is not a directory")
    else (
      let parent = Filename.dirname path in
      if parent <> path then create parent;
      try Unix.mkdir path 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  create path

let run_thumbnail process_mgr ~source ~destination ~width ~height =
  let sink () = Eio.Flow.buffer_sink (Buffer.create 128) in
  try
    Eio.Process.run process_mgr ~stdout:(sink ()) ~stderr:(sink ())
      [ "vipsthumbnail"; source; "--size";
        Printf.sprintf "%dx%d>" width height; "--output"; destination;
        "--vips-concurrency=1" ]
  with _ ->
    Eio.Process.run process_mgr ~stdout:(sink ()) ~stderr:(sink ())
      [ "ffmpeg"; "-hide_banner"; "-loglevel"; "error"; "-nostdin"; "-y";
        "-i"; source; "-frames:v"; "1"; "-vf";
        Printf.sprintf
          "scale=w='min(%d,iw)':h='min(%d,ih)':force_original_aspect_ratio=decrease"
          width height;
        destination ]

let ensure_avatar_variant process_mgr key =
  match (avatar_storage_path key, avatar_variant_path key) with
  | Some source, Some destination ->
      let usable path =
        try Sys.file_exists path && (Unix.stat path).Unix.st_size > 0
        with _ -> false
      in
      if usable destination then destination
      else (
        ensure_directory (Filename.dirname destination);
        let temporary =
          destination ^ "." ^ (Rails_crypto.random_bytes 8 |> Rails_crypto.hex)
          ^ ".tmp.webp"
        in
        (try
           run_thumbnail process_mgr ~source ~destination:temporary ~width:512
             ~height:512;
           if not (usable temporary) then failwith "avatar variant is empty";
           Sys.rename temporary destination;
           destination
         with error ->
           (try Sys.remove temporary with _ -> ());
           raise error))
  | _ -> invalid_arg "invalid avatar storage key"

let ensure_account_logo_variant process_mgr key size =
  match (avatar_storage_path key, account_logo_variant_path key size) with
  | Some source, Some destination ->
      let usable path =
        try Sys.file_exists path && (Unix.stat path).Unix.st_size > 0
        with _ -> false
      in
      if usable destination then destination
      else (
        ensure_directory (Filename.dirname destination);
        let temporary =
          destination ^ "." ^ (Rails_crypto.random_bytes 8 |> Rails_crypto.hex)
          ^ ".tmp.png"
        in
        (try
           let dimension = if size = `Small then 192 else 512 in
           run_thumbnail process_mgr ~source ~destination:temporary
             ~width:dimension ~height:dimension;
           if not (usable temporary) then failwith "account logo variant is empty";
           Sys.rename temporary destination;
           destination
         with error ->
           (try Sys.remove temporary with _ -> ());
           raise error))
  | _ -> invalid_arg "invalid account logo storage key"

let message_variant_path key (variation : Rails_crypto.active_storage_variation) =
  if not (safe_storage_key key) then None
  else
    let root =
      Sys.getenv_opt "CAMPFIRE_STORAGE_PATH"
      |> Option.value ~default:"/rails/storage"
    in
    Some
      (Filename.concat root
         (Filename.concat "variants/messages"
            (Filename.concat (String.sub key 0 2)
               (Filename.concat (String.sub key 2 2)
                  (Printf.sprintf "%s-%dx%d.%s" key variation.width
                     variation.height variation.format)))))

let message_preview_path key extension =
  if not (safe_storage_key key) then None
  else
    let root =
      Sys.getenv_opt "CAMPFIRE_STORAGE_PATH"
      |> Option.value ~default:"/rails/storage"
    in
    Some
      (Filename.concat root
         (Filename.concat "variants/previews"
            (Filename.concat (String.sub key 0 2)
               (Filename.concat (String.sub key 2 2)
                  (Printf.sprintf "%s.%s.jpg" key extension)))))

let ensure_message_preview process_mgr key content_type =
  let source = Option.get (avatar_storage_path key) in
  let extension, destination =
    if String.starts_with ~prefix:"video/" content_type then
      ("video", Option.get (message_preview_path key "video"))
    else if content_type = "application/pdf" then
      ("pdf", Option.get (message_preview_path key "pdf"))
    else invalid_arg "unsupported preview type"
  in
  let usable path =
    try Sys.file_exists path && (Unix.stat path).Unix.st_size > 0
    with _ -> false
  in
  if usable destination then destination
  else (
    ensure_directory (Filename.dirname destination);
    let temporary =
      destination ^ "." ^ (Rails_crypto.random_bytes 8 |> Rails_crypto.hex)
      ^ ".tmp.jpg"
    in
    let sink () = Eio.Flow.buffer_sink (Buffer.create 128) in
    (try
       if extension = "video" then
         Eio.Process.run process_mgr ~stdout:(sink ()) ~stderr:(sink ())
           [ "ffmpeg"; "-hide_banner"; "-loglevel"; "error"; "-nostdin";
             "-y"; "-i"; source; "-frames:v"; "1"; "-vf";
             "scale=w='min(1200,iw)':h='min(800,ih)':force_original_aspect_ratio=decrease";
             temporary ]
       else
         Eio.Process.run process_mgr ~stdout:(sink ()) ~stderr:(sink ())
           [ "pdftoppm"; "-f"; "1"; "-l"; "1"; "-singlefile"; "-scale-to";
             "1200"; "-jpeg"; source; Filename.chop_suffix temporary ".jpg" ];
       if not (usable temporary) then failwith "message attachment preview is empty";
       Sys.rename temporary destination;
       destination
     with error ->
       (try Sys.remove temporary with _ -> ());
       raise error))

let ensure_message_variant process_mgr key source
    (variation : Rails_crypto.active_storage_variation) =
  let destination = Option.get (message_variant_path key variation) in
  let usable path =
    try Sys.file_exists path && (Unix.stat path).Unix.st_size > 0
    with _ -> false
  in
  if usable destination then destination
  else (
    ensure_directory (Filename.dirname destination);
    let extension = if variation.format = "jpeg" then "jpg" else variation.format in
    let temporary =
      destination ^ "." ^ (Rails_crypto.random_bytes 8 |> Rails_crypto.hex)
      ^ ".tmp." ^ extension
    in
    (try
       run_thumbnail process_mgr ~source ~destination:temporary
         ~width:variation.width ~height:variation.height;
       if not (usable temporary) then failwith "message attachment variant is empty";
       Sys.rename temporary destination;
       destination
     with error ->
       (try Sys.remove temporary with _ -> ());
       raise error))

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let upload_filename filename =
  let filename =
    String.map (function '\\' -> '/' | character -> character) filename
    |> String.split_on_char '/' |> List.rev |> List.hd |> String.trim
  in
  let filename =
    String.to_seq filename
    |> Seq.filter (fun character -> Char.code character >= 32 && Char.code character <> 127)
    |> String.of_seq
  in
  if filename = "" || filename = "." || filename = ".." then "upload"
  else if String.length filename > 255 then String.sub filename 0 255
  else filename

let upload_content_type hint data =
  let hint = String.lowercase_ascii hint |> String.split_on_char ';' |> List.hd |> String.trim in
  let starts prefix = String.starts_with ~prefix data in
  match hint with
  | "image/png" when starts "\137PNG\r\n\026\n" -> hint
  | "image/jpeg" when String.length data >= 3 && String.sub data 0 3 = "\255\216\255" -> hint
  | "image/gif" when starts "GIF87a" || starts "GIF89a" -> hint
  | "image/bmp" when starts "BM" -> hint
  | "image/webp" when String.length data >= 12 && String.sub data 0 4 = "RIFF" && String.sub data 8 4 = "WEBP" -> hint
  | image_type when String.starts_with ~prefix:"image/" image_type
                    && Media_dimensions.image_content_type_of_bytes data = Some image_type -> image_type
  | "application/pdf" when starts "%PDF-" -> hint
  | "video/mp4" when String.length data >= 12 && String.sub data 4 4 = "ftyp" -> hint
  | "video/webm" when starts "\026\069\223\163" -> hint
  | "audio/mpeg" when starts "ID3" -> hint
  | "text/plain" when not (String.contains data '\000') -> hint
  | _ -> "application/octet-stream"

let probe_video_dimensions process_mgr path =
  try
    let stdout = Buffer.create 32 in
    let stdout_sink = Eio.Flow.buffer_sink stdout in
    let stderr_sink = Eio.Flow.buffer_sink (Buffer.create 128) in
    Eio.Process.run process_mgr ~stdout:stdout_sink ~stderr:stderr_sink
      [ "ffprobe"; "-v"; "error"; "-select_streams"; "v:0";
        "-show_entries"; "stream=width,height"; "-of"; "csv=s=x:p=0"; path ];
    match String.split_on_char 'x' (String.trim (Buffer.contents stdout)) with
    | [ width; height ] ->
        (match (int_of_string_opt width, int_of_string_opt height) with
        | Some width, Some height when width > 0 && height > 0 -> Some (width, height)
        | _ -> None)
    | _ -> None
  with _ -> None

let probe_image_dimensions process_mgr path =
  try
    let stdout = Buffer.create 128 in
    let stdout_sink = Eio.Flow.buffer_sink stdout in
    let stderr_sink = Eio.Flow.buffer_sink (Buffer.create 128) in
    Eio.Process.run process_mgr ~stdout:stdout_sink ~stderr:stderr_sink
      [ "vipsheader"; "-a"; path ];
    Media_dimensions.vipsheader_output (Buffer.contents stdout)
  with _ -> None

let probe_pdf_dimensions process_mgr path =
  try
    let stdout = Buffer.create 128 in
    let stdout_sink = Eio.Flow.buffer_sink stdout in
    let stderr_sink = Eio.Flow.buffer_sink (Buffer.create 128) in
    Eio.Process.run process_mgr ~stdout:stdout_sink ~stderr:stderr_sink
      [ "pdfinfo"; "-f"; "1"; "-l"; "1"; path ];
    Media_dimensions.pdfinfo_output (Buffer.contents stdout)
  with _ -> None

let store_upload ?process_mgr database ~user_id (file : Multipart.file_part) =
  let byte_size = String.length file.Multipart.data in
  if byte_size = 0 || byte_size > 52_428_800 then invalid_arg "upload size is invalid";
  let filename = upload_filename file.Multipart.filename in
  let content_type = upload_content_type file.Multipart.content_type file.Multipart.data in
  let key = Rails_crypto.random_bytes 18 |> Rails_crypto.base64url_encode in
  let path = Option.get (avatar_storage_path key) in
  ensure_directory (Filename.dirname path);
  let channel = open_out_gen [ Open_wronly; Open_creat; Open_excl; Open_binary ] 0o600 path in
  (try
     output_string channel file.Multipart.data;
     close_out channel
   with error ->
     close_out_noerr channel;
     remove_storage_file key;
     raise error);
  try
    let dimensions =
      match Media_dimensions.of_bytes file.Multipart.data with
      | Some dimensions -> Some dimensions
      | None when String.starts_with ~prefix:"video/" content_type ->
          Option.bind process_mgr (fun process_mgr -> probe_video_dimensions process_mgr path)
      | None when content_type = "application/pdf" ->
          Option.bind process_mgr (fun process_mgr -> probe_pdf_dimensions process_mgr path)
      | None when String.starts_with ~prefix:"image/" content_type ->
          Option.bind process_mgr (fun process_mgr -> probe_image_dimensions process_mgr path)
      | None -> None
    in
    let dimension_fields =
      match dimensions with
      | None -> []
      | Some (width, height) ->
          [ ("width", `Int width); ("height", `Int height) ]
    in
    let metadata =
      Yojson.Basic.to_string
        (`Assoc
          ([ ("identified", `Bool true); ("analyzed", `Bool true);
             ("campfire_upload_user_id", `Int user_id) ] @ dimension_fields))
    in
    let checksum = Digest.string file.Multipart.data |> Rails_crypto.base64_encode in
    match
      Database.create_stored_blob database ~key ~filename ~content_type ~byte_size
        ~checksum ~metadata ~timestamp:(timestamp_now ())
    with
    | Some blob_id -> (blob_id, key)
    | None -> failwith "Campfire database is unavailable"
  with error ->
    remove_storage_file key;
    raise error

let cleanup_uploaded_blob database (blob_id, _key) =
  match Database.delete_blob_if_unattached database blob_id with
  | Some key -> remove_storage_file key
  | None -> ()

let persist_storage_bytes key data =
  match avatar_storage_path key with
  | None -> invalid_arg "invalid storage key"
  | Some path ->
      ensure_directory (Filename.dirname path);
      let temporary = path ^ "." ^ (Rails_crypto.random_bytes 8 |> Rails_crypto.hex) ^ ".upload" in
      let channel =
        open_out_gen [ Open_wronly; Open_creat; Open_excl; Open_binary ] 0o600 temporary
      in
      (try
         output_string channel data;
         close_out channel;
         Sys.rename temporary path
       with error ->
         close_out_noerr channel;
         (try Sys.remove temporary with _ -> ());
         raise error)

let strict_md5_checksum checksum =
  match Rails_crypto.base64_decode checksum with
  | Some bytes when String.length bytes = 16 && Rails_crypto.base64_encode bytes = checksum -> true
  | _ -> false

let direct_upload_metadata_request database secret headers body =
  let session = load_session secret headers in
  let token = header headers "x-csrf-token" in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path:"/rails/active_storage/direct_uploads"
               ~method_:"POST" session token)
  then response `Unprocessable_entity "Invalid request"
  else
    match current_identity database secret headers with
    | None -> response `Unauthorized "Authentication required"
    | Some identity ->
        (try
           match Yojson.Basic.from_string body with
           | `Assoc root ->
               (match List.assoc_opt "blob" root with
               | Some (`Assoc fields) ->
                   let field name = List.assoc_opt name fields in
                   let filename, content_type, byte_size, checksum, metadata =
                     (field "filename", field "content_type", field "byte_size",
                      field "checksum", field "metadata")
                   in
                   (match (filename, byte_size, checksum) with
                   | Some (`String filename), Some (`Int byte_size), Some (`String checksum)
                     when filename <> "" && String.length filename <= 255
                          && not (String.exists (function '\r' | '\n' | '\000' -> true | _ -> false) filename)
                          && byte_size >= 0 && byte_size <= 52_428_800
                          && strict_md5_checksum checksum ->
                       let content_type =
                         match content_type with
                         | Some (`String value) when value <> "" && String.length value <= 255
                               && not (String.exists (function '\r' | '\n' -> true | _ -> false) value) -> value
                         | _ -> "application/octet-stream"
                       in
                       let filename = upload_filename filename in
                       let key = Rails_crypto.random_bytes 14 |> Rails_crypto.hex in
                       let metadata =
                         let fields = match metadata with Some (`Assoc fields) -> fields | _ -> [] in
                         Yojson.Basic.to_string
                           (`Assoc (List.remove_assoc "campfire_upload_user_id" fields
                             @ [ ("campfire_upload_user_id", `Int identity.Database.user.Database.id) ]))
                       in
                       (match
                          Database.create_stored_blob database ~key ~filename ~content_type
                            ~byte_size ~checksum ~metadata ~timestamp:(timestamp_now ())
                        with
                       | None -> response `Service_unavailable "Database unavailable"
                       | Some id ->
                           let signed_id = Rails_crypto.sign_active_storage_blob_id ~secret id in
                           let upload_token =
                             Rails_crypto.sign_active_storage_disk_upload ~secret ~key
                               ~content_type ~content_length:byte_size ~checksum
                           in
                           let origin =
                             Cohttp.Header.get headers "origin"
                             |> Option.value ~default:("http://" ^ header headers "host")
                           in
                           json_response `OK
                             (`Assoc
                               [ ("id", `Int id); ("key", `String key);
                                 ("filename", `String filename); ("content_type", `String content_type);
                                 ("byte_size", `Int byte_size); ("checksum", `String checksum);
                                 ("signed_id", `String signed_id);
                                 ("attachable_sgid", `String
                                    (Rails_crypto.sign_active_storage_attachable_sgid ~secret id));
                                 ("direct_upload", `Assoc
                                    [ ("url", `String (origin ^ "/rails/active_storage/disk/"
                                      ^ encode_path_component upload_token));
                                      ("headers", `Assoc [ ("Content-Type", `String content_type) ]) ]) ]) )
                   | _ -> response `Unprocessable_entity "Invalid blob metadata")
               | _ -> response `Unprocessable_entity "Invalid blob metadata")
           | _ -> response `Unprocessable_entity "Invalid blob metadata"
         with _ -> response `Unprocessable_entity "Invalid blob metadata")

let direct_upload_bytes_request secret token body =
  match Rails_crypto.verify_active_storage_disk_upload ~secret token with
  | None -> response `Not_found "Not found"
  | Some upload ->
      if String.length body <> upload.Rails_crypto.content_length
         || Rails_crypto.base64_encode (Digest.string body) <> upload.Rails_crypto.checksum
      then response `Unprocessable_entity "Upload checksum mismatch"
      else
        (try
           persist_storage_bytes upload.Rails_crypto.key body;
           response (Cohttp.Code.status_of_code 204) ""
         with error ->
           prerr_endline ("Active Storage direct upload failed: " ^ Printexc.to_string error);
           response `Unprocessable_entity "Upload could not be stored")

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

let bot_messages_route_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; room_id; bot_key; "messages" ] ->
      Option.map (fun room_id -> (room_id, bot_key, None))
        (int_of_string_opt room_id)
  | [ ""; "rooms"; room_id; bot_key; "messages"; message_id ] ->
      (match (int_of_string_opt room_id, int_of_string_opt message_id) with
      | Some room_id, Some message_id -> Some (room_id, bot_key, Some message_id)
      | _ -> None)
  | _ -> None

let qr_code_route_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "qr_code"; encoded_url ] when encoded_url <> "" -> Some encoded_url
  | _ -> None

let bot_boost_route_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "rooms"; room_id; bot_key; "messages"; message_id; "boosts" ] ->
      (match (int_of_string_opt room_id, int_of_string_opt message_id) with
      | Some room_id, Some message_id -> Some (room_id, bot_key, message_id, None)
      | _ -> None)
  | [ ""; "rooms"; room_id; bot_key; "messages"; message_id; "boosts"; boost_id ] ->
      (match (int_of_string_opt room_id, int_of_string_opt message_id,
              int_of_string_opt boost_id) with
      | Some room_id, Some message_id, Some boost_id ->
          Some (room_id, bot_key, message_id, Some boost_id)
      | _ -> None)
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

let session_transfer_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "session"; "transfers"; signed_id ] when signed_id <> "" -> Some signed_id
  | _ -> None

let account_user_id_of_path path =
  match String.split_on_char '/' path with
  | [ ""; "account"; "users"; id ] -> int_of_string_opt id
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

let search_page ~secret ~database (user : Database.user) csrf query recents results =
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
           ^ safe_message_body ~secret database message.Database.body_html ^ "</div></article></li>")
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

let cookie_attributes ?(secure = false) expires_header =
  Printf.sprintf
    "Path=/; Max-Age=631152000; Expires=%s; HttpOnly; SameSite=Lax%s"
    expires_header (if secure then "; Secure" else "")

let start_session_cookie ?(secure = false) headers ~secret token =
  let expires_at, expires_header = Rails_crypto.cookie_expiration () in
  let value =
    Rails_crypto.sign_cookie ~secret ~name:"session_token" ~expires_at
      (`String token)
  in
  set_cookie headers
    ("session_token=" ^ value ^ "; " ^ cookie_attributes ~secure expires_header)

let clear_session_cookie ?(secure = false) headers =
  let _, expires_header = Rails_crypto.cookie_expiration () in
  set_cookie headers
    ("session_token=; Path=/; Max-Age=0; Expires=" ^ expires_header
   ^ "; HttpOnly; SameSite=Lax" ^ if secure then "; Secure" else "")

let session_transfer_page secret headers signed_id =
  let session = load_session secret headers in
  let action = "/session/transfers/" ^ signed_id in
  let html =
    "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
    ^ html_escape session.Session.csrf_form_token
    ^ "\"><title>Transfer Campfire session</title></head><body><main><h1>Opening your Campfire account</h1><form action=\""
    ^ html_escape action
    ^ "\" method=\"post\"><input type=\"hidden\" name=\"_method\" value=\"put\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
    ^ html_escape session.Session.csrf_form_token
    ^ "\"><button type=\"submit\">Continue</button></form><script>document.querySelector('form').requestSubmit()</script></main></body></html>"
  in
  html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK html

let update_session_transfer_request database secret headers remote_ip signed_id
    method_ body =
  let secure = request_scheme headers = "https" in
  let session = load_session secret headers in
  let form = parse_form body in
  let token = match Cohttp.Header.get headers "x-csrf-token" with
    | Some value -> value | None -> form_value form "authenticity_token" in
  let path = "/session/transfers/" ^ signed_id in
  if not (valid_origin headers
          && Session.valid_csrf ~path ~method_ session token)
  then html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else match Rails_crypto.verify_user_transfer_id ~secret signed_id with
    | None -> response `Bad_request "Invalid or expired transfer link"
    | Some user_id ->
        (match Database.find_active_user_by_id database user_id with
        | None -> response `Bad_request "Invalid or expired transfer link"
        | Some user ->
            let token = Rails_crypto.random_bytes 18 |> Rails_crypto.base64url_encode in
            let timestamp = timestamp_now () in
            Database.create_session database ~user_id:user.Database.id ~token
              ~user_agent:(header headers "user-agent") ~ip_address:remote_ip ~timestamp;
            let destination =
              match Session.get_string session "return_to_after_authenticating" with
              | Some path when String.starts_with ~prefix:"/" path
                               && not (String.starts_with ~prefix:"//" path) -> path
              | _ -> "/"
            in
            let session = Session.remove session "return_to_after_authenticating" in
            let response_headers =
              Cohttp.Header.init_with "location" destination
              |> fun headers -> attach_session_cookie ~secure headers ~secret session
              |> fun headers -> start_session_cookie ~secure headers ~secret token
            in
            response ~headers:response_headers `Found "")

let sanitize_redirect = function
  | Some path when String.starts_with ~prefix:"/" path
                   && not (String.starts_with ~prefix:"//" path) -> path
  | _ -> "/"

let authenticate_form database jobs_database remote_ip secret headers body =
  let secure = request_scheme headers = "https" in
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else if
    not
      (Database.allow_login jobs_database remote_ip
         ~at_ms:(Int64.of_float (Unix.gettimeofday () *. 1000.)))
  then
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Too_many_requests
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
        html_with_session ~secure:(request_scheme headers = "https") ~secret session ~headers:html_headers `Unauthorized
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
          |> fun headers -> attach_session_cookie ~secure headers ~secret session
          |> fun headers -> start_session_cookie ~secure headers ~secret token
        in
        response ~headers:response_headers `Found ""

let create_first_run database remote_ip secret headers body =
  let secure = request_scheme headers = "https" in
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    let name = namespaced_form_value form "user" "name" |> trim in
    let email = namespaced_form_value form "user" "email_address" |> trim in
    let password = namespaced_form_value form "user" "password" in
    if name = "" || email = "" || password = "" || String.length password > 72
    then
      html_with_session ~secure:(request_scheme headers = "https") ~headers:html_headers ~secret session
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
              |> fun headers -> attach_session_cookie ~secure headers ~secret session
              |> fun headers -> start_session_cookie ~secure headers ~secret token
            in
            response ~headers:response_headers `Found ""
      with _ ->
        html_with_session ~secure:(request_scheme headers = "https") ~headers:html_headers ~secret session
          `Unprocessable_entity
          (first_run_form ~name ~email
             ~error:"Campfire could not be set up with those details."
             session.Session.csrf_form_token)

let create_join_user_request database remote_ip secret headers path body =
  let secure = request_scheme headers = "https" in
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
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
              html_with_session ~secure:(request_scheme headers = "https") ~headers:html_headers ~secret session
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
                      |> fun headers -> attach_session_cookie ~secure headers ~secret session
                      |> fun headers -> start_session_cookie ~secure headers ~secret token
                    in
                    response ~headers:response_headers `Found ""
              with
              | Database.Invalid_join_code -> response `Not_found "Not found"
              | Database.Duplicate_email ->
                  redirect
                    ("/session/new?email_address=" ^ encode_query email)
              | _ ->
                  html_with_session ~secure:(request_scheme headers = "https") ~headers:html_headers ~secret session
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

let update_blob_dimensions_from_disk ?process_mgr database blob_id =
  try
    match Database.find_stored_blob database blob_id with
    | None -> ()
    | Some blob ->
        (match avatar_storage_path blob.Database.key with
        | None -> ()
        | Some path ->
            let dimensions =
              match Media_dimensions.of_file path with
              | Some dimensions -> Some dimensions
              | None when String.starts_with ~prefix:"video/" blob.Database.content_type ->
                  Option.bind process_mgr (fun process_mgr -> probe_video_dimensions process_mgr path)
              | None when blob.Database.content_type = "application/pdf" ->
                  Option.bind process_mgr (fun process_mgr -> probe_pdf_dimensions process_mgr path)
              | None when String.starts_with ~prefix:"image/" blob.Database.content_type ->
                  Option.bind process_mgr (fun process_mgr -> probe_image_dimensions process_mgr path)
              | None -> None
            in
            (match dimensions with
            | None -> ()
            | Some (width, height) ->
                ignore
                  (Database.update_blob_dimensions database ~blob_id ~width ~height)))
  with _ -> ()

let submit_message ~sw ~process_mgr ~database_lock message_bus database secret headers room_id body =
  let request_headers = headers in
  let session = load_session secret headers in
  let form, files = parse_request_form headers body in
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
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
            let client_message_id =
              namespaced_form_value form "message" "client_message_id" |> trim
            in
            let client_message_id =
              if client_message_id = "" then create_message_id () else client_message_id
            in
            let attachment_token =
              namespaced_form_value form "message" "attachment" |> trim
            in
            let uploads =
              List.filter (fun (file : Multipart.file_part) ->
                  file.Multipart.name = "message[attachment]") files
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
            if List.length uploads > 1 then
              response `Unprocessable_entity "Only one attachment is allowed"
            else if uploads <> [] && attachment_token <> "" then
              response `Unprocessable_entity "Attachment is invalid"
            else if attachment_token <> "" && attachment_blob_id = None then
              response `Unprocessable_entity "Attachment is invalid or inaccessible"
            else if
              (String.trim content = "" && uploads = [] && attachment_blob_id = None)
              || String.length content > 10_000
            then
              response `Unprocessable_entity "Message must contain 1–10000 bytes"
            else
              let uploaded =
                match uploads with
                | [] -> None
                | [ file ] -> Some (store_upload ~process_mgr database ~user_id:identity.Database.user.id file)
                | _ -> assert false
              in
              let attachment_blob_id =
                match uploaded with Some (blob_id, _) -> Some blob_id | None -> attachment_blob_id
              in
              (match (uploaded, attachment_blob_id) with
              | None, Some blob_id ->
                  update_blob_dimensions_from_disk ~process_mgr database blob_id
              | _ -> ());
              try
                (match Database.create_message_with_id ~secret ?attachment_blob_id database ~room_id
                  ~creator_id:identity.Database.user.id ~body:content
                  ~client_message_id
                  ~timestamp:(timestamp_now ()) with
                | Some message_id ->
                    let message = Database.find_message database room_id message_id in
                    Option.iter
                      (fun message ->
                        !dispatch_message_push ~sw ~process_mgr ~database ~database_lock
                          ~secret ~room ~creator_id:identity.Database.user.id ~body:content
                          ~message ~timestamp:message.Database.created_at)
                      message;
                    let boosts, attachments =
                      Option.fold ~none:([], [])
                        ~some:(live_message_details database) message
                    in
                    Option.iter
                      (Cable_bus.publish message_bus ~room_id
                         ~room_name:room.Database.name ~boosts ~attachments)
                      message;
                    Database.room_member_ids database room_id
                    |> List.iter (fun user_id ->
                           Cable_bus.publish_unread message_bus ~user_id ~room_id);
                    if accepts_turbo_stream then
                      let message = Option.get message in
                      let html =
                        "<turbo-stream action=\"append\" target=\"room_"
                        ^ string_of_int room_id ^ "_messages\"><template>"
                        ^ render_live_message ~secret ~database
                            ~room_name:room.Database.name ~boosts ~attachments
                            identity.Database.user room_id
                            session.Session.csrf_form_token message
                        ^ "</template></turbo-stream>"
                      in
                      let headers =
                        Cohttp.Header.init_with "content-type"
                          "text/vnd.turbo-stream.html; charset=utf-8"
                      in
                      html_with_session ~secure:(request_scheme request_headers = "https") ~headers ~secret session `OK html
                    else redirect ("/rooms/" ^ string_of_int room_id)
                | None ->
                    Option.iter (cleanup_uploaded_blob database) uploaded;
                    response `Service_unavailable "Campfire database is unavailable")
              with error ->
                Option.iter (cleanup_uploaded_blob database) uploaded;
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
              html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
                (message_edit_form ~secret database session.Session.csrf_form_token room_id message)))

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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
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
                 Database.update_message ~secret database ~room_id ~message_id
                   ~user_id:identity.Database.user.id
                   ~role:identity.Database.user.role ~body:content
                   ~timestamp:(timestamp_now ());
                 let message = Database.find_message database room_id message_id in
                 let boosts, attachments =
                   Option.fold ~none:([], [])
                     ~some:(live_message_details database) message
                 in
                 let room_name =
                   Database.find_room_for_user database identity.Database.user.id room_id
                   |> Option.map (fun (room : Database.room) -> room.Database.name)
                   |> Option.value ~default:""
                 in
                 Option.iter
                   (Cable_bus.publish_replace message_bus ~room_id ~room_name
                      ~boosts ~attachments)
                   message;
                 redirect path)
           | "DELETE" ->
               let message_dom_id =
                 Database.find_message database room_id message_id
                 |> Option.map (fun (message : Database.message) ->
                        "message_" ^ message.Database.client_message_id)
               in
               Database.delete_message database ~room_id ~message_id
                 ~user_id:identity.Database.user.id
                 ~role:identity.Database.user.role
               |> List.iter (fun key ->
                      remove_storage_file key);
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
        | _ -> response `Unprocessable_entity "Message could not be saved")

let search_index ?(gzip = false) database secret headers query =
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
      html_with_session ~gzip ~secure:(request_scheme headers = "https") ~secret session `OK
        (search_page ~secret ~database identity.Database.user session.Session.csrf_form_token query
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
      html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
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
      html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
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
      html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
        ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
        ^ html_escape session.Session.csrf_form_token
        ^ "\"><title>New room · Campfire</title></head><body><main><a href=\"/\">Campfire</a><h1>New room</h1><form action=\"/rooms/opens\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
        ^ html_escape session.Session.csrf_form_token
        ^ "\"><label>Room name<input name=\"room[name]\" required autofocus></label><button type=\"submit\">Create room</button></form></main></body></html>")

let sidebar_room_item_named ~room_id ~name ~direct ~unread =
  let direct_class = if direct then " direct" else " room" in
  let unread_class = if unread then " unread" else "" in
  "<a id=\"room_" ^ string_of_int room_id
  ^ "_list\" class=\"btn align-center gap txt-nowrap" ^ direct_class
  ^ unread_class ^ "\" data-room-id=\"" ^ string_of_int room_id
  ^ "\" href=\"/rooms/" ^ string_of_int room_id
  ^ "\"><span class=\"overflow-ellipsis\">"
  ^ html_escape name ^ "</span></a>"

let sidebar_room_item ~direct ~unread (room : Database.room) =
  sidebar_room_item_named ~room_id:room.Database.id ~name:room.Database.name
    ~direct ~unread

let sidebar_turbo_stream action target html =
  "<turbo-stream action=\"" ^ action ^ "\" target=\""
  ^ html_escape target ^ "\"><template>" ^ html ^ "</template></turbo-stream>"

let publish_global_room_sidebar_named message_bus action ~room_id ~name =
  let target =
    if action = "prepend" then "shared_rooms"
    else "room_" ^ string_of_int room_id ^ "_list"
  in
  let html =
    if action = "remove" then ""
    else sidebar_room_item_named ~room_id ~name ~direct:false ~unread:false
  in
  Cable_bus.publish_sidebar_global message_bus
    (sidebar_turbo_stream action target html)

let publish_global_room_sidebar message_bus action (room : Database.room) =
  publish_global_room_sidebar_named message_bus action ~room_id:room.Database.id
    ~name:room.Database.name

let publish_user_room_sidebar message_bus database ~user_id ~room_id ~direct action =
  let list_target = if direct then "direct_rooms" else "shared_rooms" in
  let target =
    if action = "prepend" then list_target
    else "room_" ^ string_of_int room_id ^ "_list"
  in
  let html =
    if action = "remove" then ""
    else
      Database.sidebar_rooms database user_id
      |> List.find_opt (fun (entry : Database.sidebar_room) ->
             entry.Database.room.Database.id = room_id)
      |> Option.map (fun (entry : Database.sidebar_room) ->
             sidebar_room_item ~direct ~unread:entry.Database.unread entry.Database.room)
      |> Option.value ~default:""
  in
  Cable_bus.publish_sidebar_user message_bus ~user_id
    (sidebar_turbo_stream action target html)

let create_open_room_request message_bus database secret headers body =
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
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
        | Some room_id ->
            Option.iter
              (publish_global_room_sidebar message_bus "prepend")
              (Database.find_room_for_user database identity.Database.user.id room_id);
            redirect ("/rooms/" ^ string_of_int room_id)
        | None -> response `Unprocessable_entity "Room could not be saved")

let create_closed_room_request message_bus database secret headers body =
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
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
        | Some room_id ->
            Database.room_member_ids database room_id
            |> List.iter (fun user_id ->
                   publish_user_room_sidebar message_bus database ~user_id ~room_id
                     ~direct:false "prepend");
            redirect ("/rooms/" ^ string_of_int room_id)
        | None -> response `Unprocessable_entity "Room could not be saved")

let create_direct_room_request message_bus database secret headers body =
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
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
        let participants =
          identity.Database.user.id :: member_ids |> List.sort_uniq compare
        in
        let existing_room = Database.find_direct_room database ~member_ids:participants in
        (match
           Database.find_or_create_direct_room database
             ~creator_id:identity.Database.user.id ~member_ids
             ~timestamp:(timestamp_now ())
         with
        | Some room_id ->
            if existing_room = None then
              Database.room_member_ids database room_id
              |> List.iter (fun user_id ->
                     publish_user_room_sidebar message_bus database ~user_id ~room_id
                       ~direct:true "prepend");
            redirect ("/rooms/" ^ string_of_int room_id)
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        Database.clear_searches database identity.Database.user.id;
        redirect "/searches"

let logout database secret headers body =
  let secure = request_scheme headers = "https" in
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else (
    Option.iter
      (fun identity -> Database.delete_session database identity.Database.session_id)
      (current_identity database secret headers);
    let headers =
      Cohttp.Header.init_with "location" "/"
      |> fun headers -> attach_session_cookie ~secure headers ~secret (Session.clear session)
      |> clear_session_cookie ~secure
    in
    response ~headers `Found "")

let room_response ?(gzip = false) database secret headers room_id messages ~older_messages
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
          html_with_session ~gzip ~secure:(request_scheme headers = "https") ~secret session `OK
            (room_page ~secret ~database identity.Database.user
               (Database.sidebar_rooms database identity.Database.user.id)
               room session.Session.csrf_form_token messages ~boosts_by_message
               ~attachments_by_message ~older_messages
               ~newer_messages ~room_stream))

let room_messages_index_response ?(gzip = false) database secret headers (user : Database.user)
    (room : Database.room) messages =
  match messages with
  | [] -> response `No_content ""
  | messages ->
      let session = load_session secret headers in
      let boosts_by_message = Database.boosts_for_messages database messages in
      let attachments_by_message = Database.attachments_for_messages database messages in
      let body =
        messages
        |> List.map (fun (message : Database.message) ->
               let boosts =
                 List.assoc_opt message.Database.id boosts_by_message
                 |> Option.value ~default:[]
               in
               let attachments =
                 List.assoc_opt message.Database.id attachments_by_message
                 |> Option.to_list
               in
               render_message_item ~secret ~database ~room_name:room.Database.name ~boosts
                 ~attachments user room.Database.id session.Session.csrf_form_token message)
        |> String.concat ""
      in
      html_with_session ~gzip ~secure:(request_scheme headers = "https") ~secret session `OK
        body

let refresh_room_request database secret headers room_id request =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      (match Database.find_room_for_user database identity.Database.user.id room_id with
      | None -> response `Not_found "Room not found or inaccessible"
      | Some room ->
          let params = query_parameters request in
          let since_ms =
            Option.bind (List.assoc_opt "since" params) Int64.of_string_opt
            |> Option.value ~default:0L
          in
          let created, updated =
            Database.messages_changed_since database room_id since_ms
          in
          let session = load_session secret headers in
          let changed_messages = created @ updated in
          let boosts_by_message =
            Database.boosts_for_messages database changed_messages
          in
          let attachments_by_message =
            Database.attachments_for_messages database changed_messages
          in
          let render (message : Database.message) =
            let boosts =
              List.assoc_opt message.Database.id boosts_by_message
              |> Option.value ~default:[]
            in
            let attachments =
              List.assoc_opt message.Database.id attachments_by_message
              |> Option.to_list
            in
            render_live_message ~secret ~database ~room_name:room.Database.name ~boosts
              ~attachments identity.Database.user room_id
              session.Session.csrf_form_token message
          in
          let stream action target html =
            "<turbo-stream action=\"" ^ action ^ "\" target=\""
            ^ html_escape target ^ "\"><template>" ^ html
            ^ "</template></turbo-stream>"
          in
          let append =
            match created with
            | [] -> ""
            | messages ->
                stream "append" ("room_" ^ string_of_int room_id ^ "_messages")
                  (messages |> List.map render |> String.concat "")
          in
          let replace =
            updated
            |> List.map (fun (message : Database.message) ->
                   stream "replace"
                     ("message_" ^ message.Database.client_message_id)
                     (render message))
            |> String.concat ""
          in
          response
            ~headers:(Cohttp.Header.init_with "content-type"
                        "text/vnd.turbo-stream.html; charset=utf-8")
            `OK (append ^ replace))

let sidebar_page ?(gzip = false) database secret headers =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      let session = load_session secret headers in
      let rooms = Database.sidebar_rooms database identity.Database.user.id in
      let render_room direct (entry : Database.sidebar_room) =
        sidebar_room_item ~direct ~unread:entry.Database.unread entry.Database.room
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
      html_with_session ~gzip ~secure:(request_scheme headers = "https") ~secret session `OK
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

let autocomplete_users_page ?(gzip = false) database secret headers request =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      let params = query_parameters request in
      let scope =
        match List.assoc_opt "room_id" params with
        | None | Some "" -> `Global
        | Some raw ->
            (match int_of_string_opt raw with
            | Some room_id
              when Database.find_room_for_user database identity.Database.user.id room_id <> None ->
                `Room room_id
            | _ -> `Not_found)
      in
      (match scope with
      | `Not_found -> response `Not_found "Not found"
      | `Global | `Room _ ->
          let query =
            match List.assoc_opt "filter" params with
            | Some query -> query
            | None -> List.assoc_opt "query" params |> Option.value ~default:""
          in
          let query = if String.trim query = "" then "" else query in
          let room_id = match scope with `Room id -> Some id | _ -> None in
          let users = Database.autocompletable_users database ~room_id ~query in
          let avatar_url (user : Database.autocompletable_user) =
            let token = Rails_crypto.sign_user_avatar_id ~secret user.Database.id in
            let digits =
              user.Database.updated_at |> String.to_seq
              |> Seq.filter (function '0' .. '9' -> true | _ -> false)
              |> String.of_seq
            in
            let version =
              if String.length digits > 14 then String.sub digits 0 14 else digits
            in
            "/users/" ^ token ^ "/avatar"
            ^ if version = "" then "" else "?v=" ^ version
          in
          let user_sgid (user : Database.autocompletable_user) =
            Rails_crypto.sign_attachable_sgid ~secret ~model:"User" user.Database.id
          in
          let accept = header headers "accept" |> String.lowercase_ascii in
          if contains_substring ~needle:"application/json" accept then
            users
            |> List.map (fun (user : Database.autocompletable_user) ->
                   let avatar = avatar_url user in
                   `Assoc
                     [ ("id", `Int user.Database.id);
                       ("name", `String (html_escape user.Database.name));
                       ("label", `String user.Database.name);
                       ("avatar", `String avatar); ("avatar_url", `String avatar);
                       ("sgid", `String (user_sgid user));
                       ("value", `Int user.Database.id) ])
            |> fun users -> json_response ~gzip `OK (`List users)
          else
            let rendered =
              users
              |> List.map (fun (user : Database.autocompletable_user) ->
                     let name = html_escape user.Database.name in
                     let sgid = user_sgid user |> html_escape in
                     let avatar = avatar_url user |> html_escape in
                     "<lexxy-prompt-item search=\"" ^ name ^ "\" sgid=\"" ^ sgid
                     ^ "\"><template type=\"menu\"><span class=\"autocomplete__item flex align-center gap unpad\"><img class=\"avatar\" src=\""
                     ^ avatar ^ "\"><span class=\"autocompletable__name\">" ^ name
                     ^ "</span></span></template><template type=\"editor\"><span class=\"mention\" sgid=\""
                     ^ sgid ^ "\"><img class=\"avatar\" src=\"" ^ avatar ^ "\"> "
                     ^ name ^ "</span></template></lexxy-prompt-item>")
              |> String.concat ""
            in
            response ~gzip
              ~headers:(Cohttp.Header.init_with "content-type" "text/html; charset=utf-8")
              `OK rendered)

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

let read_avatar_file key =
  match avatar_storage_path key with
  | None -> None
  | Some path ->
    try
      let channel = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () -> Some (really_input_string channel (in_channel_length channel)))
    with _ -> None

let read_storage_file = read_avatar_file

let active_storage_serving_attributes content_type requested_attachment =
  let content_type =
    content_type |> String.split_on_char ';' |> List.hd |> String.trim
    |> String.lowercase_ascii
  in
  let binary_types =
    [ "text/html"; "image/svg+xml"; "application/postscript";
      "application/x-shockwave-flash"; "text/xml"; "application/xml";
      "application/xhtml+xml"; "application/mathml+xml"; "text/cache-manifest" ]
  in
  let inline_types =
    [ "image/webp"; "image/avif"; "image/png"; "image/gif"; "image/jpeg";
      "image/tiff"; "image/bmp"; "image/vnd.adobe.photoshop";
      "image/vnd.microsoft.icon"; "application/pdf" ]
  in
  if List.mem content_type binary_types then ("application/octet-stream", false)
  else (content_type, List.mem content_type inline_types && not requested_attachment)

let show_active_storage_representation process_mgr database secret headers signed_id
    variation_token =
  match current_identity database secret headers with
  | None -> response `Unauthorized "Authentication required"
  | Some identity ->
      let verified_id =
        Option.bind (Rails_crypto.percent_decode signed_id)
          (Rails_crypto.verify_active_storage_blob_id ~secret)
      in
      let variation =
        Option.bind (Rails_crypto.percent_decode variation_token)
          (Rails_crypto.verify_active_storage_variation ~secret)
      in
      (match (verified_id, variation) with
      | Some blob_id, Some variation ->
          (match Database.find_stored_blob database blob_id with
          | None -> response `Not_found "Not found"
          | Some blob
            when not
                   (Database.authorized_stored_blob database ~blob_id
                      ~user_id:identity.Database.user.Database.id) ->
              response `Forbidden "Forbidden"
          | Some blob ->
              (try
                 let source =
                   if String.starts_with ~prefix:"image/" blob.Database.content_type then
                     Option.get (avatar_storage_path blob.Database.key)
                   else if
                     String.starts_with ~prefix:"video/" blob.Database.content_type
                     || blob.Database.content_type = "application/pdf"
                   then
                     ensure_message_preview process_mgr blob.Database.key
                       blob.Database.content_type
                   else failwith "attachment is not previewable"
                 in
                 let path =
                   ensure_message_variant process_mgr blob.Database.key source
                     variation
                 in
                 let body = read_file path in
                 let extension =
                   if variation.Rails_crypto.format = "jpg" then "jpeg"
                   else variation.Rails_crypto.format
                 in
                 let content_type = "image/" ^ extension in
                 response
                   ~headers:
                     (Cohttp.Header.init_with "content-type" content_type
                     |> fun headers ->
                     Cohttp.Header.add headers "content-disposition" "inline")
                   `OK body
               with _ -> response `Not_found "Not found"))
      | _ -> response `Not_found "Not found")

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
      | Some _blob when not (Database.authorized_stored_blob database ~blob_id
                             ~user_id:identity.Database.user.Database.id) ->
          response `Forbidden "Forbidden"
      | Some blob ->
          (match read_storage_file blob.Database.key with
          | None -> response `Not_found "Not found"
          | Some body ->
              let content_type, inline =
                active_storage_serving_attributes blob.Database.content_type
                  attachment
              in
              let filename =
                blob.Database.filename
                |> String.map (function '\r' | '\n' | '"' | '\\' -> '_' | c -> c)
              in
              let headers =
                Cohttp.Header.init_with "content-type" content_type
                |> fun headers -> Cohttp.Header.add headers "accept-ranges" "bytes"
                |> fun headers ->
                Cohttp.Header.add headers "content-disposition"
                  ((if inline then "inline" else "attachment")
                  ^ "; filename=\"" ^ filename ^ "\"")
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

let show_account_logo process_mgr database request =
  let size =
    match List.assoc_opt "size" (query_parameters request) with
    | Some "small" -> `Small
    | _ -> `Large
  in
  let headers =
    Cohttp.Header.init_with "content-type" "image/png"
    |> fun headers ->
    Cohttp.Header.add headers "cache-control"
      "public, max-age=300, stale-while-revalidate=604800"
  in
  match Database.find_account_logo_blob database with
  | Some blob
    when List.mem blob.Database.content_type
           [ "image/png"; "image/jpeg"; "image/gif"; "image/bmp"; "image/webp" ] ->
      (try
         let variant = ensure_account_logo_variant process_mgr blob.Database.key size in
         response ~headers `OK (read_file variant)
       with _ -> response `Internal_server_error "Account logo processing failed")
  | _ ->
      (match read_public_asset
               (if size = `Small then "account-logo-192.png" else "account-logo-512.png") with
      | Some body -> response ~headers `OK body
      | None -> response `Not_found "Logo not found")

let qr_code_response process_mgr encoded_url =
  match Rails_crypto.base64_decode encoded_url with
  | None -> response `Bad_request "Invalid QR code URL"
  | Some url when url = "" || String.length url > 2_953 || String.contains url '\000' ->
      response `Unprocessable_entity "QR code URL is empty or too long"
  | Some url ->
      let output = Buffer.create 2_048 in
      let sink = Eio.Flow.buffer_sink output in
      let error_sink = Eio.Flow.buffer_sink (Buffer.create 128) in
      (try
         Eio.Process.run process_mgr ~stdout:sink ~stderr:error_sink
           [ "qrencode"; "-t"; "SVG"; "-o"; "-"; "--"; url ];
         let headers =
           Cohttp.Header.init_with "content-type" "image/svg+xml; charset=utf-8"
           |> fun headers -> Cohttp.Header.add headers "cache-control"
                "public, max-age=31536000"
         in
         response ~headers `OK (Buffer.contents output)
       with _ -> response `Unprocessable_entity "QR code URL could not be encoded")

let unfurl_response_headers path =
  try
    let size = (Unix.stat path).Unix.st_size in
    if size > 65_536 then []
    else
      read_file path |> String.split_on_char '\n'
      |> List.filter_map (fun line ->
             match String.index_opt line ':' with
             | None -> None
             | Some colon ->
                 let name = String.sub line 0 colon |> String.trim |> String.lowercase_ascii in
                 if name = "location" then
                   Some (String.sub line (colon + 1) (String.length line - colon - 1) |> String.trim)
                 else None)
  with _ -> []

let fetch_unfurl_resource process_mgr ~deadline ~head initial_url =
  let rec follow redirects value =
    if Unix.gettimeofday () >= deadline || redirects > 10 || String.length value > 8_192
       || String.exists (fun character -> Char.code character < 0x20 || character = '\127') value
    then None
    else
      match Opengraph.resolve_public_uri ~resolve:Opengraph.system_resolve value with
      | None -> None
      | Some (uri, addresses) ->
          let remaining = deadline -. Unix.gettimeofday () in
          if remaining <= 0. then None
          else
          let host = Uri.host uri |> Option.value ~default:"" |> Opengraph.strip_brackets in
          let port =
            Uri.port uri
            |> Option.value ~default:(if Uri.scheme uri = Some "https" then 443 else 80)
          in
          let address = List.hd addresses in
          let address = if String.contains address ':' then "[" ^ address ^ "]" else address in
          let host_for_resolve =
            if String.contains host ':' then "[" ^ host ^ "]" else host
          in
          let headers_path = Filename.temp_file "campfire-unfurl-headers-" ".tmp" in
          let body_path = Filename.temp_file "campfire-unfurl-body-" ".tmp" in
          Fun.protect
            ~finally:(fun () ->
              (try Sys.remove headers_path with _ -> ());
              (try Sys.remove body_path with _ -> ()))
            (fun () ->
              let stdout = Buffer.create 256 in
              let stdout_sink = Eio.Flow.buffer_sink stdout in
              let stderr_sink = Eio.Flow.buffer_sink (Buffer.create 256) in
              let resolve_argument =
                Printf.sprintf "%s:%d:%s" host_for_resolve port address
              in
              let command =
                [ "curl"; "--disable"; "--silent"; "--show-error";
                  "--noproxy"; "*"; "--connect-timeout";
                  Printf.sprintf "%.3f" (min 3. remaining);
                  "--max-time"; Printf.sprintf "%.3f" (min 7. remaining);
                  "--max-filesize"; "5242880";
                  "--limit-rate"; "1M"; "--proto"; "=http,https";
                  "--resolve"; resolve_argument ]
                @ (if head then [ "--head" ] else [])
                @ [ "--dump-header"; headers_path; "--output"; body_path;
                    "--write-out"; "%{http_code}\\n%{content_type}";
                    Uri.to_string uri ]
              in
              try
                Eio.Process.run process_mgr ~stdout:stdout_sink ~stderr:stderr_sink command;
                let result = Buffer.contents stdout |> String.split_on_char '\n' in
                let status = match result with code :: _ -> int_of_string_opt code | _ -> None in
                let content_type =
                  match result with
                  | _ :: content_type :: _ ->
                      content_type |> String.split_on_char ';' |> List.hd
                      |> String.trim |> String.lowercase_ascii
                  | _ -> ""
                in
                let locations = unfurl_response_headers headers_path in
                match status with
                | Some (301 | 302 | 303 | 307 | 308) when redirects < 9 ->
                    (match locations with
                    | [] -> None
                    | location :: _ ->
                        let target = Uri.resolve "http" uri (Uri.of_string location) in
                        follow (redirects + 1) (Uri.to_string target))
                | Some 200 when head -> Some (uri, content_type, "")
                | Some 200 when content_type = "text/html" ->
                    let size = (Unix.stat body_path).Unix.st_size in
                    if size > 5_242_880 then None
                    else Some (uri, content_type, read_file body_path)
                | _ -> None
              with _ -> None)
  in
  follow 0 initial_url

let fetch_unfurl_metadata process_mgr url =
  try
    let deadline = Unix.gettimeofday () +. 15. in
    let original_uri = Uri.of_string url in
    let fetch_uri = Opengraph.twitter_proxy_uri original_uri in
    if Opengraph.media_path (Uri.path fetch_uri) then None
    else
      match fetch_unfurl_resource process_mgr ~deadline ~head:false (Uri.to_string fetch_uri) with
      | None -> None
      | Some (_, _, html) ->
          let fields = Opengraph.extract_open_graph html in
          let image_content_type image =
            fetch_unfurl_resource process_mgr ~deadline ~head:true image
            |> Option.map (fun (_, content_type, _) -> content_type)
          in
          Opengraph.metadata ~resolve:Opengraph.system_resolve ~image_content_type url fields
  with _ -> None

let unfurl_link_request process_mgr database secret headers body =
  let session = load_session secret headers in
  let token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None ->
        let form, _ = parse_request_form headers body in
        form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path:"/unfurl_link" ~method_:"POST" session token)
  then response `Unprocessable_entity "Invalid request"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some _ ->
        let url =
          if String.starts_with ~prefix:"application/json"
               (String.lowercase_ascii (header headers "content-type"))
          then
            (try
               match Yojson.Basic.from_string body with
               | `Assoc fields ->
                   (match List.assoc_opt "url" fields with Some (`String value) -> value | _ -> "")
               | _ -> ""
             with _ -> "")
          else
            let form, _ = parse_request_form headers body in
            form_value form "url"
        in
        if String.trim url = "" then response `Bad_request "URL is required"
        else (match fetch_unfurl_metadata process_mgr (String.trim url) with
        | None -> response `No_content ""
        | Some (title, canonical_url, image, description) ->
            json_response `OK
              (`Assoc
                [ ("title", `String title); ("url", `String canonical_url);
                  ("image", Option.fold ~none:`Null ~some:(fun value -> `String value) image);
                  ("description", `String description) ]))

let show_avatar process_mgr database secret path =
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
                when List.mem blob.Database.content_type
                       [ "image/png"; "image/jpeg"; "image/gif"; "image/bmp";
                         "image/webp" ] ->
                  (try
                     let variant = ensure_avatar_variant process_mgr blob.Database.key in
                     response ~headers:(headers "image/webp") `OK
                       (read_file variant)
                   with _ -> response `Internal_server_error "Avatar processing failed")
              | _ ->
                  response ~headers:(headers "image/svg+xml; charset=utf-8") `OK
                    (avatar_svg user.Database.name))))

let profile_page ?(gzip = false) database secret headers ?(error = "") () =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      (match Database.find_profile database identity.Database.user.id with
      | None -> response `Not_found "User not found"
      | Some profile ->
          let session = load_session secret headers in
          let transfer_id =
            Rails_crypto.sign_user_transfer_id ~secret profile.Database.id
          in
          let transfer_url =
            request_origin headers ^ "/session/transfers/" ^ transfer_id
          in
          let transfer_qr_path = qr_code_path_for_url transfer_url in
          let transfer_section =
            "<section><h2>Transfer this account</h2><p>Use this one-time link to sign in on another device. It expires in four hours.</p><input readonly aria-label=\"Account transfer link\" value=\""
            ^ html_escape transfer_url ^ "\"><a href=\""
            ^ html_escape transfer_qr_path
            ^ "\" aria-label=\"Show auto-login QR code\">Show QR code</a></section>"
          in
          let avatar_remove_form =
            match Database.find_avatar_blob database profile.Database.id with
            | None -> ""
            | Some _ ->
                "<form action=\"/users/" ^ string_of_int profile.Database.id
                ^ "/avatar\" method=\"post\"><input type=\"hidden\" name=\"_method\" value=\"delete\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
                ^ html_escape session.Session.csrf_form_token
                ^ "\"><button type=\"submit\">Remove avatar</button></form>"
          in
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
          html_with_session ~gzip ~secure:(request_scheme headers = "https") ~secret session `OK
            ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
            ^ html_escape session.Session.csrf_form_token
            ^ "\"><title>" ^ html_escape profile.Database.name
            ^ " · Campfire</title></head><body><nav><a href=\"/\">Campfire</a></nav><main><a href=\"/\">Back to Campfire</a><h1>Profile</h1>"
            ^ error_html ^ "<form action=\"/users/me/profile\" method=\"post\" enctype=\"multipart/form-data\"><input type=\"hidden\" name=\"_method\" value=\"patch\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
            ^ html_escape session.Session.csrf_form_token
            ^ "\"><label>Name<input name=\"user[name]\" autocomplete=\"name\" required value=\""
            ^ html_escape profile.Database.name
            ^ "\"></label><label>Email address<input type=\"email\" name=\"user[email_address]\" autocomplete=\"username\" value=\""
            ^ html_escape profile.Database.email_address
            ^ "\"></label><label>Change password<input type=\"password\" name=\"user[password]\" autocomplete=\"new-password\" maxlength=\"72\"></label><label>Bio<textarea name=\"user[bio]\" maxlength=\"200\" rows=\"3\">"
            ^ html_escape profile.Database.bio
            ^ "</textarea></label><label>Avatar<input type=\"file\" name=\"user[avatar]\" accept=\"image/*\"></label><button type=\"submit\">Save profile</button></form>"
            ^ avatar_remove_form ^ transfer_section ^ "<section><h2>Your rooms</h2><ul>"
            ^ memberships ^ "</ul></section></main></body></html>"))

let push_subscriptions_page database secret headers =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      let session = load_session secret headers in
      let subscriptions = Database.push_subscriptions database ~user_id:identity.Database.user.id in
      let rows =
        subscriptions
        |> List.map (fun (subscription : Database.push_subscription) ->
               "<li><strong>" ^ html_escape subscription.Database.user_agent
               ^ "</strong><p>" ^ html_escape subscription.Database.endpoint
               ^ "</p><form method=\"post\" action=\"/users/me/push_subscriptions/"
               ^ string_of_int subscription.Database.id
               ^ "/test_notifications\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
               ^ html_escape session.Session.csrf_form_token
               ^ "\"><button type=\"submit\">Send test notification</button></form><form method=\"post\" action=\"/users/me/push_subscriptions/"
               ^ string_of_int subscription.Database.id
               ^ "\"><input type=\"hidden\" name=\"_method\" value=\"delete\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
               ^ html_escape session.Session.csrf_form_token
               ^ "\"><button type=\"submit\">Delete subscription</button></form></li>")
        |> String.concat ""
      in
      html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
        ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
        ^ html_escape session.Session.csrf_form_token
        ^ "\"><title>Push notification subscriptions</title></head><body><main><h1>Push Notification Subscriptions</h1><ul id=\"push_subscriptions\">"
        ^ rows ^ "</ul></main></body></html>")

let push_subscription_values body =
  let from_json () =
    try
      match Yojson.Basic.from_string body with
      | `Assoc fields ->
          (match List.assoc_opt "push_subscription" fields with
          | Some (`Assoc values) ->
              let value name =
                match List.assoc_opt name values with
                | Some (`String value) -> value
                | _ -> ""
              in
              Some (value "endpoint", value "p256dh_key", value "auth_key")
          | _ -> None)
      | _ -> None
    with Yojson.Json_error _ -> None
  in
  match from_json () with
  | Some values -> values
  | None ->
      let form = parse_form body in
      ( namespaced_form_value form "push_subscription" "endpoint",
        namespaced_form_value form "push_subscription" "p256dh_key",
        namespaced_form_value form "push_subscription" "auth_key" )

let mutate_push_subscription database secret headers request path subscription_id body =
  let session = load_session secret headers in
  let form = parse_form body in
  let override = String.uppercase_ascii (form_value form "_method") in
  let method_ =
    match Cohttp.Request.meth request with
    | `DELETE -> "DELETE"
    | `POST when override = "DELETE" -> "DELETE"
    | `POST -> "POST"
    | _ -> ""
  in
  let csrf_method = method_ in
  let csrf_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if method_ = "" then response `Method_not_allowed "Method not allowed"
  else if not (valid_origin headers)
          || not (Session.valid_csrf ~path ~method_:csrf_method session csrf_token)
  then
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        (match (method_, subscription_id) with
        | "POST", None ->
            let endpoint, p256dh_key, auth_key = push_subscription_values body in
            if not (Push_subscription.valid_endpoint
                      ~resolve:Push_subscription.system_resolve endpoint)
            then response `Unprocessable_entity "Invalid push subscription endpoint"
            else
              (match Database.register_push_subscription database
                       ~user_id:identity.Database.user.id ~endpoint ~p256dh_key ~auth_key
                       ~user_agent:(header headers "user-agent") ~timestamp:(timestamp_now ()) with
              | Some _ -> response `OK ""
              | None -> response `Service_unavailable "Campfire database is unavailable")
        | "DELETE", Some subscription_id ->
            if Database.delete_push_subscription database
                 ~user_id:identity.Database.user.id ~subscription_id
            then redirect "/users/me/push_subscriptions"
            else redirect "/users/me/push_subscriptions"
        | _ -> response `Method_not_allowed "Method not allowed")

let push_uuid () =
  let bytes = Bytes.of_string (Rails_crypto.random_bytes 16) in
  Bytes.set bytes 6 (Char.chr ((Char.code (Bytes.get bytes 6) land 0x0f) lor 0x40));
  Bytes.set bytes 8 (Char.chr ((Char.code (Bytes.get bytes 8) land 0x3f) lor 0x80));
  let hex = Rails_crypto.hex (Bytes.unsafe_to_string bytes) in
  Printf.sprintf "%s-%s-%s-%s-%s"
    (String.sub hex 0 8) (String.sub hex 8 4) (String.sub hex 12 4)
    (String.sub hex 16 4) (String.sub hex 20 12)

let deliver_test_push ?(title = "Campfire Test") ?body:notification_body
    ?(path = "/users/me/push_subscriptions") ?badge process_mgr database user_id subscription =
  match
    ( Sys.getenv_opt "VAPID_PRIVATE_KEY",
      Sys.getenv_opt "VAPID_PUBLIC_KEY",
      Sys.getenv_opt "VAPID_SUBJECT" )
  with
  | Some private_key, Some public_key, Some subject
    when private_key <> "" && public_key <> "" && subject <> "" ->
      (try
         let endpoint_uri = Uri.of_string subscription.Database.endpoint in
         let host =
           Uri.host endpoint_uri |> Option.value ~default:"" |> Opengraph.strip_brackets
         in
         let allowed_uri =
           Uri.scheme endpoint_uri = Some "https"
           && (Uri.port endpoint_uri = None || Uri.port endpoint_uri = Some 443)
           && Uri.userinfo endpoint_uri = None
           && Uri.fragment endpoint_uri = None
           && host <> ""
         in
         match
           if allowed_uri then
             Push_subscription.resolved_public_addresses
               ~resolve:Push_subscription.system_resolve subscription.Database.endpoint
           else None
         with
         | None -> `Invalid_endpoint
         | Some (resolved_host, addresses) when resolved_host = String.lowercase_ascii host ->
             let address = List.hd addresses in
             let address = if String.contains address ':' then "[" ^ address ^ "]" else address in
             let resolve_argument = Printf.sprintf "%s:443:%s" host address in
             let now = Unix.gettimeofday () |> floor |> Int64.of_float in
             let authorization =
               Web_push.authorization_header ~private_key ~public_key ~subject
                 ~audience:("https://" ^ host) ~now
             in
             let user_public = Web_push.decode_key subscription.Database.p256dh_key in
             let auth_secret = Web_push.decode_key subscription.Database.auth_key in
             let badge =
               match badge with
               | Some value -> value
               | None -> Database.unread_membership_count database ~user_id
             in
             let payload =
               Yojson.Basic.to_string
                 (`Assoc
                   [ ("title", `String title);
                     ("options",
                      `Assoc
                        [ ("body", `String (Option.value notification_body ~default:(push_uuid ())));
                          ("data",
                           `Assoc
                             [ ("badge", `Int badge);
                               ("path", `String path) ]) ]) ])
             in
             let encrypted =
               Web_push.encode_record ~user_public ~auth_secret payload
             in
             let body_path = Filename.temp_file "campfire-web-push-" ".bin" in
             Fun.protect
               ~finally:(fun () -> try Sys.remove body_path with _ -> ())
               (fun () ->
                 let output = open_out_bin body_path in
                 Fun.protect ~finally:(fun () -> close_out_noerr output)
                   (fun () -> output_string output encrypted);
                 let stdout = Buffer.create 16 in
                 let stdout_sink = Eio.Flow.buffer_sink stdout in
                 let stderr_sink = Eio.Flow.buffer_sink (Buffer.create 128) in
                 let command =
                   [ "curl"; "--disable"; "--silent"; "--show-error";
                     "--noproxy"; "*"; "--connect-timeout"; "3";
                     "--max-time"; "8"; "--proto"; "=https";
                     "--resolve"; resolve_argument; "--request"; "POST";
                     "--header"; "Content-Type: application/octet-stream";
                     "--header"; "Content-Encoding: aes128gcm";
                     "--header"; "TTL: 86400";
                     "--header"; "Urgency: normal";
                     "--header"; "Authorization: " ^ authorization;
                     "--data-binary"; "@" ^ body_path; "--output"; "/dev/null";
                     "--write-out"; "%{http_code}"; subscription.Database.endpoint ]
                 in
                 try
                   Eio.Process.run process_mgr ~stdout:stdout_sink ~stderr:stderr_sink command;
                   match int_of_string_opt (String.trim (Buffer.contents stdout)) with
                   | Some ((200 | 201 | 202) as status) -> `Delivered status
                   | Some ((404 | 410) as status) -> `Expired status
                   | Some status -> `Rejected status
                   | None -> `Failed
                 with _ -> `Failed)
         | Some _ -> `Invalid_endpoint
       with _ -> `Failed)
  | _ -> `Not_configured

let () =
  dispatch_message_push :=
    (fun ~sw ~process_mgr ~database ~database_lock ~secret ~room ~creator_id ~body
         ~message ~timestamp ->
      let mentioned_user_ids = Action_text.mentioned_user_ids ~secret body in
      let targets =
        Database.message_push_targets database ~room_id:room.Database.id ~creator_id
          ~mentioned_user_ids ~timestamp
      in
      let creator_name =
        Database.find_user_detail database creator_id
        |> Option.map (fun (user : Database.user_detail) -> user.Database.name)
        |> Option.value ~default:"Campfire"
      in
      let title, notification_body =
        let text = String.trim (html_body_to_text ~secret ~database body) in
        let attachment_filename =
          Database.attachments_for_messages database [ message ]
          |> List.assoc_opt message.Database.id
          |> Option.map (fun (attachment : Database.message_attachment) -> attachment.Database.filename)
          |> Option.value ~default:""
        in
        let text = if text = "" then attachment_filename else text in
        if room.Database.kind = "Rooms::Direct" then creator_name, text
        else room.Database.name, creator_name ^ ": " ^ text
      in
      List.iter
        (fun (target : Database.message_push_target) ->
          let badge =
            Database.unread_membership_count database ~user_id:target.Database.user_id
          in
          Eio.Fiber.fork ~sw (fun () ->
              match
                deliver_test_push ~title ~body:notification_body
                  ~path:("/rooms/" ^ string_of_int room.Database.id) ~badge
                  process_mgr database target.Database.user_id target.Database.subscription
              with
              | `Expired _ ->
                  Eio.Mutex.use_rw ~protect:true database_lock (fun () ->
                      ignore
                        (Database.delete_push_subscription database
                           ~user_id:target.Database.user_id
                           ~subscription_id:target.Database.subscription.Database.id))
              | _ -> ()))
        targets)

let test_push_notification_request process_mgr database secret headers request
    subscription_id body =
  let path =
    "/users/me/push_subscriptions/" ^ string_of_int subscription_id
    ^ "/test_notifications"
  in
  let session = load_session secret headers in
  let form = parse_form body in
  let token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some value -> value
    | None -> form_value form "authenticity_token"
  in
  if Cohttp.Request.meth request <> `POST then
    response `Method_not_allowed "Method not allowed"
  else if not (valid_origin headers)
          || not (Session.valid_csrf ~path ~method_:"POST" session token)
  then
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        (match
           Database.find_push_subscription database
             ~user_id:identity.Database.user.id ~subscription_id
         with
        | None -> response `Not_found "Push subscription not found"
        | Some subscription ->
            (match
               deliver_test_push process_mgr database identity.Database.user.id subscription
             with
            | `Delivered _ -> redirect "/users/me/push_subscriptions"
            | `Expired _ ->
                ignore
                  (Database.delete_push_subscription database
                     ~user_id:identity.Database.user.id ~subscription_id);
                redirect "/users/me/push_subscriptions"
            | `Not_configured ->
                response `Service_unavailable "Web Push VAPID configuration is required"
            | `Invalid_endpoint ->
                response `Unprocessable_entity "Invalid push subscription endpoint"
            | `Rejected status ->
                response `Bad_gateway
                  (Printf.sprintf "Push service rejected notification (%d)" status)
            | `Failed -> response `Bad_gateway "Push notification delivery failed"))

let user_page ?(gzip = false) database secret headers user_id =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      (match Database.find_user_detail database user_id with
      | None -> response `Not_found "User not found"
      | Some detail ->
          let session = load_session secret headers in
          let administrator = identity.Database.user.role = 1 in
          let class_name = if detail.Database.status = 2 then " class=\"banned\"" else "" in
          let avatar =
            "<img class=\"avatar\" alt=\"Profile avatar\" src=\"/users/"
            ^ Rails_crypto.sign_user_avatar_id ~secret detail.Database.id
            ^ "/avatar\">"
          in
          let identity_html =
            if detail.Database.status = 1 then
              "<h1>" ^ html_escape detail.Database.name ^ "</h1><p>"
              ^ html_escape detail.Database.name ^ " is no longer on this account</p>"
            else if detail.Database.role = 2 then
              "<h1>" ^ html_escape detail.Database.name ^ "</h1><p>Bot</p>"
            else
              "<h1>" ^ html_escape detail.Database.name ^ "</h1>"
              ^ (if administrator then "<p><a href=\"mailto:" ^ html_escape detail.Database.email_address
                 ^ "\">" ^ html_escape detail.Database.email_address ^ "</a></p>" else "")
              ^ "<p>" ^ html_escape detail.Database.bio ^ "</p>"
          in
          let transfer =
            if administrator && detail.Database.status = 0 && detail.Database.role <> 2 then
              let transfer_id = Rails_crypto.sign_user_transfer_id ~secret detail.Database.id in
              "<section><h2>Transfer account</h2><input readonly aria-label=\"Account transfer link\" value=\"/session/transfers/"
              ^ transfer_id ^ "\"></section>"
            else ""
          in
          let ban_control =
            if not administrator || detail.Database.id = identity.Database.user.id
               || detail.Database.role = 2 || detail.Database.status = 1 then ""
            else
              let method_ = if detail.Database.status = 2 then "delete" else "post" in
              let label = if detail.Database.status = 2 then "Remove ban" else "Ban " ^ detail.Database.name in
              "<form action=\"/users/" ^ string_of_int detail.Database.id
              ^ "/ban\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
              ^ html_escape session.Session.csrf_form_token ^ "\">"
              ^ (if method_ = "delete" then "<input type=\"hidden\" name=\"_method\" value=\"delete\">" else "")
              ^ "<button type=\"submit\">" ^ html_escape label ^ "</button></form>"
          in
          html_with_session ~gzip ~secure:(request_scheme headers = "https") ~secret session `OK
            ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
             ^ html_escape session.Session.csrf_form_token ^ "\"><title>" ^ html_escape detail.Database.name
             ^ "</title></head><body><main><section" ^ class_name ^ ">" ^ avatar ^ identity_html
             ^ transfer ^ ban_control ^ "</section></main></body></html>"))

let user_ban_request message_bus database secret headers user_id method_ body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token = match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token | None -> form_value form "authenticity_token" in
  let path = "/users/" ^ string_of_int user_id ^ "/ban" in
  if not (valid_origin headers
          && Session.valid_csrf ~path ~method_ session authenticity_token)
  then html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity when identity.Database.user.role <> 1 -> response `Forbidden "Forbidden"
    | Some _ ->
        let changed =
          match method_ with
          | "POST" ->
              if not (Database.ban_user database ~user_id ~timestamp:(timestamp_now ())) then false
              else (
                let messages, files = Database.delete_banned_user_messages database ~user_id in
                List.iter remove_storage_file files;
                List.iter (fun (message : Database.banned_message) ->
                  Cable_bus.publish_remove message_bus ~room_id:message.Database.room_id
                    ~message_dom_id:message.Database.client_message_id) messages;
                true)
          | "DELETE" -> Database.unban_user database ~user_id ~timestamp:(timestamp_now ())
          | _ -> false
        in
        if changed then redirect ("/users/" ^ string_of_int user_id)
        else response `Not_found "User not found"

let account_edit_page ?(gzip = false) database secret headers =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      let session = load_session secret headers in
      let name = Database.account_name database |> Option.value ~default:"Campfire" in
      let restricted = Database.room_creation_restricted database in
      let invite_section =
        match Database.account_join_code database with
        | None -> ""
        | Some join_code ->
            let url = request_origin headers ^ "/join/" ^ join_code in
            let qr_path = qr_code_path_for_url url in
            "<section><h2>Share to invite more people</h2><input type=\"text\" id=\"invite_url\" aria-label=\"Invite link\" readonly value=\""
            ^ html_escape url ^ "\"><a href=\"" ^ html_escape qr_path
            ^ "\" aria-label=\"Show join link QR code\">Show QR code</a></section>"
      in
      let logo_controls =
        match Database.find_account_logo_blob database with
        | None -> ""
        | Some _ ->
            "<img alt=\"Account logo\" src=\"/account/logo?size=small\"><form action=\"/account/logo\" method=\"post\"><input type=\"hidden\" name=\"_method\" value=\"delete\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
            ^ html_escape session.Session.csrf_form_token
            ^ "\"><button type=\"submit\">Remove account logo</button></form>"
      in
      let members =
        if identity.Database.user.role <> 1 then ""
        else
          Database.account_members database
          |> List.map (fun (member : Database.account_member) ->
                 let path = "/account/users/" ^ string_of_int member.Database.id in
                 let role = if member.Database.role = 1 then "administrator" else "member" in
                 let controls =
                   if member.Database.id = identity.Database.user.id then ""
                   else
                     let next_role = if member.Database.role = 1 then "member" else "administrator" in
                     "<form action=\"" ^ path ^ "\" method=\"post\"><input type=\"hidden\" name=\"_method\" value=\"patch\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
                     ^ html_escape session.Session.csrf_form_token ^ "\"><input type=\"hidden\" name=\"user[role]\" value=\"" ^ next_role ^ "\"><button>Make " ^ next_role ^ "</button></form><form action=\"" ^ path ^ "\" method=\"post\"><input type=\"hidden\" name=\"_method\" value=\"delete\"><input type=\"hidden\" name=\"authenticity_token\" value=\"" ^ html_escape session.Session.csrf_form_token ^ "\"><button>Deactivate</button></form>"
                 in
                 "<li><a href=\"" ^ path ^ "\">" ^ html_escape member.Database.name
                 ^ "</a> &lt;" ^ html_escape member.Database.email_address ^ "&gt; — " ^ role ^ controls ^ "</li>")
          |> String.concat ""
          |> fun rows -> "<section><h2>Members</h2><ul>" ^ rows ^ "</ul></section>"
      in
      let editor =
        if identity.Database.user.role = 1 then
          "<form action=\"/account\" method=\"post\" enctype=\"multipart/form-data\"><input type=\"hidden\" name=\"_method\" value=\"patch\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
          ^ html_escape session.Session.csrf_form_token
          ^ "\"><label>Account name<input name=\"account[name]\" required value=\""
          ^ html_escape name
          ^ "\"></label><label>Must be admin to create new rooms<select name=\"account[settings][restrict_room_creation_to_administrators]\"><option value=\"false\""
          ^ (if restricted then "" else " selected")
          ^ ">No</option><option value=\"true\""
          ^ (if restricted then " selected" else "")
          ^ ">Yes</option></select></label><label>Account logo<input type=\"file\" name=\"account[logo]\" accept=\"image/png,image/jpeg,image/gif,image/webp\"></label><button type=\"submit\">Save account</button></form>"
          ^ logo_controls ^ "<form action=\"/account/join_code\" method=\"post\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
          ^ html_escape session.Session.csrf_form_token ^ "\"><button>Regenerate join code</button></form><a href=\"/account/bots\">Chat bots</a> <a href=\"/account/custom_styles/edit\">Custom styles</a>"
        else "<p>Account: " ^ html_escape name ^ "</p>"
      in
      html_with_session ~gzip ~secure:(request_scheme headers = "https") ~secret session `OK
        ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
        ^ html_escape session.Session.csrf_form_token
        ^ "\"><title>Account settings</title></head><body><main><h1>Account settings</h1>"
        ^ invite_section ^ editor ^ members ^ "</main></body></html>")

let account_bots_page database secret headers mode =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity when identity.Database.user.role <> 1 -> response `Forbidden "Forbidden"
  | Some _ ->
      let session = load_session secret headers in
      let page body title =
        html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
          ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
          ^ html_escape session.Session.csrf_form_token
          ^ "\"><title>" ^ html_escape title
          ^ "</title></head><body><nav><a href=\"/account/edit\">Account settings</a></nav><main>"
          ^ body ^ "</main></body></html>")
      in
      let render_editor (bot : Database.account_bot) create =
        let action = if create then "/account/bots" else "/account/bots/" ^ string_of_int bot.Database.id in
        let form =
          "<h1>" ^ (if create then "New chat bot" else "Edit bot")
          ^ "</h1><form method=\"post\" action=\"" ^ action
          ^ "\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
          ^ html_escape session.Session.csrf_form_token ^ "\">"
          ^ (if create then "" else "<input type=\"hidden\" name=\"_method\" value=\"patch\">")
          ^ "<label>Name<input required name=\"user[name]\" value=\""
          ^ html_escape bot.Database.name ^ "\"></label><label>Webhook URL<input type=\"url\" name=\"user[webhook_url]\" value=\""
          ^ html_escape bot.Database.webhook_url ^ "\"></label><button type=\"submit\">Save bot</button></form>"
        in
        let controls =
          if create then ""
          else
            "<form method=\"post\" action=\"" ^ action
            ^ "\"><input type=\"hidden\" name=\"_method\" value=\"delete\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
            ^ html_escape session.Session.csrf_form_token
            ^ "\"><button type=\"submit\">Deactivate bot</button></form><form method=\"post\" action=\""
            ^ action ^ "/key\"><input type=\"hidden\" name=\"_method\" value=\"put\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
            ^ html_escape session.Session.csrf_form_token
            ^ "\"><button type=\"submit\">Generate a new key</button></form>"
        in
        page (form ^ controls) (if create then "New chat bot" else "Edit bot")
      in
      (match mode with
      | `Index ->
          let bots = Database.account_bots database in
          let rows =
            bots
            |> List.map (fun (bot : Database.account_bot) ->
                   let key = string_of_int bot.Database.id ^ "-" ^ bot.Database.token in
                   let rooms =
                     Database.rooms_for_user database bot.Database.id
                     |> List.filter (fun (room : Database.room) ->
                            room.Database.kind <> "Rooms::Direct")
                     |> List.map (fun (room : Database.room) ->
                            let command =
                              "curl -d 'Hello!' /rooms/" ^ string_of_int room.Database.id
                              ^ "/" ^ key ^ "/messages"
                            in
                            "<li>" ^ html_escape room.Database.name ^ " <code>"
                            ^ html_escape command ^ "</code></li>")
                     |> String.concat ""
                   in
                   "<li><h2>" ^ html_escape bot.Database.name ^ "</h2><p>Webhook: "
                   ^ html_escape bot.Database.webhook_url ^ "</p><p>Bot key: <code>"
                   ^ html_escape key ^ "</code></p><ul>" ^ rooms ^ "</ul><a href=\"/account/bots/"
                   ^ string_of_int bot.Database.id ^ "/edit\">Edit "
                   ^ html_escape bot.Database.name ^ "</a></li>")
            |> String.concat ""
          in
          page ("<h1>Chat bots</h1><p>With chat bots, other sites and services can post updates directly to Campfire.</p><a href=\"/account/bots/new\">Add a chat bot</a><ul>"
                ^ rows ^ "</ul>") "Chat bots"
      | `New ->
          render_editor
            { Database.id = 0; name = ""; token = ""; webhook_url = "" }
            true
      | `Edit id ->
          (match Database.account_bots database
                   |> List.find_opt (fun (bot : Database.account_bot) -> bot.Database.id = id) with
          | None -> response `Not_found "Bot not found"
          | Some bot -> render_editor bot false)
      | `Bot _ | `Key _ -> response `Not_found "Not found")

let account_bots_request database secret headers request path route body =
  let session = load_session secret headers in
  let form, _files = parse_request_form headers body in
  let submitted_override = String.uppercase_ascii (form_value form "_method") in
  let method_ =
    match Cohttp.Request.meth request with
    | `GET -> "GET"
    | `POST when submitted_override <> "" -> submitted_override
    | `POST -> "POST"
    | `PUT -> "PUT"
    | `PATCH -> "PATCH"
    | `DELETE -> "DELETE"
    | _ -> ""
  in
  let csrf_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if method_ = "GET" then
    (match route with
    | `Index | `New | `Edit _ -> account_bots_page database secret headers route
    | _ -> response `Not_found "Not found")
  else if not (valid_origin headers
               && Session.valid_csrf ~path ~method_ session csrf_token)
  then
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity when identity.Database.user.role <> 1 -> response `Forbidden "Forbidden"
    | Some _ ->
        let name = namespaced_form_value form "user" "name" |> String.trim in
        let webhook_url = namespaced_form_value form "user" "webhook_url" |> String.trim in
        let webhook_url = if webhook_url = "" then None else Some webhook_url in
        let changed =
          match (route, method_) with
          | `Index, "POST" when name <> "" ->
              Database.create_bot database ~name ~webhook_url
                ~timestamp:(timestamp_now ()) <> None
          | `Bot id, ("PATCH" | "PUT") when name <> "" ->
              Database.update_bot database ~user_id:id ~name ~webhook_url
                ~timestamp:(timestamp_now ())
          | `Bot id, "DELETE" ->
              Database.deactivate_bot database ~user_id:id
                ~timestamp:(timestamp_now ())
          | `Key id, "PUT" ->
              Database.rotate_bot_key database ~user_id:id
                ~timestamp:(timestamp_now ()) <> None
          | _ -> false
        in
        if changed then redirect "/account/bots"
        else response `Unprocessable_entity "Bot could not be saved"

let account_custom_styles_request database secret headers request path route body =
  let session = load_session secret headers in
  let form, _files = parse_request_form headers body in
  let submitted_override = String.uppercase_ascii (form_value form "_method") in
  let method_ =
    match Cohttp.Request.meth request with
    | `GET -> "GET"
    | `POST when submitted_override <> "" -> submitted_override
    | `POST -> "POST"
    | `PUT -> "PUT"
    | `PATCH -> "PATCH"
    | _ -> ""
  in
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity when identity.Database.user.role <> 1 -> response `Forbidden "Forbidden"
  | Some _ ->
      (match (route, method_) with
      | `Edit, "GET" ->
          let styles = Database.account_custom_styles database |> Option.value ~default:"" in
          html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
            ("<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"csrf-param\" content=\"authenticity_token\"><meta name=\"csrf-token\" content=\""
            ^ html_escape session.Session.csrf_form_token
            ^ "\"><title>Custom styles</title></head><body><main><h1>Custom styles</h1><form method=\"post\" action=\"/account/custom_styles\"><input type=\"hidden\" name=\"_method\" value=\"patch\"><input type=\"hidden\" name=\"authenticity_token\" value=\""
            ^ html_escape session.Session.csrf_form_token
            ^ "\"><label>CSS<textarea name=\"account[custom_styles]\" rows=\"24\">"
            ^ html_escape styles
            ^ "</textarea></label><button type=\"submit\">Save custom styles</button></form></main></body></html>")
      | `Update, ("PATCH" | "PUT") ->
          let csrf_token =
            match Cohttp.Header.get headers "x-csrf-token" with
            | Some token -> token
            | None -> form_value form "authenticity_token"
          in
          if not (valid_origin headers
                  && Session.valid_csrf ~path ~method_ session csrf_token)
          then html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
              "<!doctype html><html><body>Unprocessable request</body></html>"
          else
            let styles = namespaced_form_value form "account" "custom_styles" in
            if Database.update_account_custom_styles database ~custom_styles:styles
                 ~timestamp:(timestamp_now ())
            then (
              current_custom_styles := Some styles;
              redirect "/account/custom_styles/edit")
            else response `Unprocessable_entity "Custom styles could not be saved"
      | _ -> response `Not_found "Not found")

let account_admin_request database secret headers body path user_id method_ =
  let session = load_session secret headers in
  let form, _files = parse_request_form headers body in
  let authenticity_token = match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token | None -> form_value form "authenticity_token" in
  if not (valid_origin headers
          && Session.valid_csrf ~path ~method_ session authenticity_token)
  then html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity when identity.Database.user.role <> 1 -> response `Forbidden "Forbidden"
    | Some _ ->
        let changed = match method_ with
          | "PATCH" ->
              let role = match form_value form "user[role]" with
                | "administrator" | "1" -> Some 1
                | "member" | "0" -> Some 0
                | _ -> None in
              Option.fold ~none:false ~some:(fun role ->
                Database.update_member_role database ~user_id ~role
                  ~timestamp:(Rails_crypto.utc_now ())) role
          | "DELETE" -> Database.deactivate_member database ~user_id
              ~timestamp:(Rails_crypto.utc_now ())
          | _ -> false in
        if changed then redirect "/account/edit" else response `Not_found "Member not found"

let regenerate_join_code_request database secret headers body =
  let session = load_session secret headers in
  let form = parse_form body in
  let token = match Cohttp.Header.get headers "x-csrf-token" with
    | Some value -> value | None -> form_value form "authenticity_token" in
  if not (valid_origin headers
          && Session.valid_csrf ~path:"/account/join_code" ~method_:"POST" session token)
  then html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity when identity.Database.user.role <> 1 -> response `Forbidden "Forbidden"
    | Some _ -> (match Database.rotate_join_code database with
        | Some _ -> redirect "/account/edit"
        | None -> response `Not_found "Account not found")

let update_account_request database secret headers body method_ =
  let session = load_session secret headers in
  let form, files = parse_request_form headers body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path:"/account" ~method_ session authenticity_token)
  then
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity when identity.Database.user.role <> 1 -> response `Forbidden "Forbidden"
    | Some identity ->
        let logo_file =
          List.find_opt
            (fun (file : Multipart.file_part) ->
              file.Multipart.name = "account[logo]" && file.Multipart.filename <> "")
            files
        in
        let invalid_logo =
          match logo_file with
          | None -> false
          | Some file ->
              let content_type = upload_content_type file.Multipart.content_type file.Multipart.data in
              String.length file.Multipart.data = 0
              || String.length file.Multipart.data > 52_428_800
              || not (List.mem content_type [ "image/png"; "image/jpeg"; "image/gif"; "image/bmp"; "image/webp" ])
        in
        let name =
          List.assoc_opt "account[name]" form |> Option.map trim
        in
        let restrict =
          match form_value form "account[settings][restrict_room_creation_to_administrators]" with
          | "true" | "1" | "on" -> Some true
          | "false" | "0" | "off" -> Some false
          | _ -> None
        in
        if invalid_logo then response `Unprocessable_entity "Account logo must be a supported raster image"
        else if name = Some "" then response `Unprocessable_entity "Account name is required"
        else
          (try
             let updated =
               Database.update_account database ~name
                 ~restrict_room_creation:restrict
                 ~timestamp:(Rails_crypto.utc_now ())
             in
             if updated then (
               (match logo_file with
               | None -> ()
               | Some file ->
                   let staged = store_upload database
                       ~user_id:identity.Database.user.Database.id file in
                   (try
                      let previous = Database.attach_account_logo database
                          ~blob_id:(fst staged) ~timestamp:(timestamp_now ()) in
                      List.iter remove_storage_file previous
                    with error ->
                      cleanup_uploaded_blob database staged;
                      raise error));
               redirect "/account/edit")
             else response `Not_found "Account not found"
           with _ -> response `Unprocessable_entity "Account could not be updated")

let destroy_account_logo_request database secret headers body =
  let session = load_session secret headers in
  let form, _files = parse_request_form headers body in
  let token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some value -> value
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers
          && Session.valid_csrf ~path:"/account/logo" ~method_:"DELETE" session token)
  then html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity when identity.Database.user.role <> 1 -> response `Forbidden "Forbidden"
    | Some _ ->
        Database.destroy_account_logo database ~timestamp:(timestamp_now ())
        |> List.iter remove_storage_file;
        redirect "/account/edit"

let update_profile_request database secret headers body =
  let session = load_session secret headers in
  let form, files = parse_request_form headers body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  let path = "/users/me/profile" in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_:"PATCH" session authenticity_token)
  then
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        let name = namespaced_form_value form "user" "name" in
        let email = namespaced_form_value form "user" "email_address" in
        let password = namespaced_form_value form "user" "password" in
        let bio = namespaced_form_value form "user" "bio" in
        let avatar_uploads =
          List.filter (fun (file : Multipart.file_part) -> file.Multipart.name = "user[avatar]") files
        in
        if List.length avatar_uploads > 1 then
          profile_page database secret headers ~error:"Only one avatar may be uploaded." ()
        else if avatar_uploads <> []
                && not (String.starts_with ~prefix:"image/"
                          (match avatar_uploads with
                           | [ file ] -> upload_content_type file.Multipart.content_type file.Multipart.data
                           | _ -> "")) then
          profile_page database secret headers ~error:"Avatar must be a supported image." ()
        else if String.length password > 72 then
          profile_page database secret headers
            ~error:"Password must be no longer than 72 bytes." ()
        else
          let password_digest =
            if password = "" then None else Some (Bcrypt.hash password)
          in
          let uploaded =
            match avatar_uploads with
            | [] -> None
            | [ file ] -> Some (store_upload database ~user_id:identity.Database.user.id file)
            | _ -> assert false
          in
          (try
             if
               Database.update_profile database ~user_id:identity.Database.user.id
                 ~name ~email_address:email ~password_digest ~bio
                 ~timestamp:(timestamp_now ())
             then (
               (match uploaded with
               | Some (blob_id, _) ->
                   Database.attach_avatar database ~user_id:identity.Database.user.id
                     ~blob_id ~timestamp:(timestamp_now ())
                   |> List.iter (fun key ->
                          remove_storage_file key)
               | None -> ());
               redirect path)
             else (
               Option.iter (cleanup_uploaded_blob database) uploaded;
               profile_page database secret headers
                 ~error:"Profile could not be saved. The email address may already be in use." ())
           with error ->
             Option.iter (cleanup_uploaded_blob database) uploaded;
             prerr_endline ("benchmark profile update failed: " ^ Printexc.to_string error);
             profile_page database secret headers ~error:"Profile could not be saved." ())

let destroy_avatar_request database secret headers user_id body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  let path = "/users/" ^ string_of_int user_id ^ "/avatar" in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_:"DELETE" session authenticity_token)
  then
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity when identity.Database.user.id <> user_id -> response `Forbidden "Forbidden"
    | Some _ ->
        (try
           Database.destroy_avatar database user_id
           |> List.iter remove_storage_file;
           redirect "/users/me/profile"
         with _ -> response `Unprocessable_entity "Avatar could not be removed")

let delete_room_request message_bus database secret headers path room_id is_direct body =
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
      "<!doctype html><html><body>Unprocessable request</body></html>"
  else
    match current_identity database secret headers with
    | None -> redirect "/session/new"
    | Some identity ->
        (match Database.find_room_for_user database identity.Database.user.id room_id with
        | None -> response `Not_found "Room not found or inaccessible"
        | Some _room
          when (_room.Database.kind = "Rooms::Direct") <> is_direct ->
            response `Not_found "Room not found or inaccessible"
        | Some _room
          when not is_direct
               && not
                    (Database.can_administer_room database ~room_id
                       ~user_id:identity.Database.user.id ~role:identity.Database.user.role) ->
            response `Forbidden "Room administration is not allowed"
        | Some room ->
            (try
               let member_ids = Database.room_member_ids database room_id in
               (match
                  Database.delete_room database ~room_id
                    ~user_id:identity.Database.user.id ~role:identity.Database.user.role
                with
                | None -> response `Service_unavailable "Campfire database is unavailable"
                | Some keys ->
                    List.iter
                      remove_storage_file
                      keys;
                    if is_direct then
                      List.iter (fun user_id ->
                          publish_user_room_sidebar message_bus database ~user_id ~room_id
                            ~direct:true "remove") member_ids
                    else if room.Database.kind = "Rooms::Open" then
                      publish_global_room_sidebar message_bus "remove" room
                    else
                      List.iter (fun user_id ->
                          publish_user_room_sidebar message_bus database ~user_id ~room_id
                            ~direct:false "remove") member_ids;
                    redirect "/")
             with
            | Database.Room_not_found -> response `Not_found "Room not found"
            | Database.Room_not_authorized ->
                response `Forbidden "Room deletion is not allowed"
            ))

let edit_room_page database secret headers room_id =
  match current_identity database secret headers with
  | None -> redirect "/session/new"
  | Some identity ->
      (match Database.find_room_for_user database identity.Database.user.id room_id with
      | None -> response `Not_found "Room not found or inaccessible"
      | Some room when room.Database.kind = "Rooms::Direct" ->
          response `Not_found "Room not found or inaccessible"
      | Some _room
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
          html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
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

let update_room_request ?(csrf_method = "PATCH") message_bus database secret
    headers path room_id body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token =
    match Cohttp.Header.get headers "x-csrf-token" with
    | Some token -> token
    | None -> form_value form "authenticity_token"
  in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_:csrf_method session authenticity_token)
  then
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
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
        | Some room ->
            let name = namespaced_form_value form "room" "name" in
            let submitted_kind = namespaced_form_value form "room" "type" in
            let kind = if submitted_kind = "" then room.Database.kind else submitted_kind in
            let previous_kind = room.Database.kind in
            let previous_member_ids = Database.room_member_ids database room_id in
            let member_ids =
              parse_form_pairs body
              |> List.filter_map (fun (key, value) ->
                     if key = "user_ids[]" then int_of_string_opt value else None)
            in
            if
              Database.update_shared_room database ~room_id
                ~user_id:identity.Database.user.id ~role:identity.Database.user.role
                ~name ~kind ~member_ids ~timestamp:(timestamp_now ())
            then (
              let current_member_ids = Database.room_member_ids database room_id in
              (match (previous_kind, kind) with
              | "Rooms::Open", "Rooms::Open" ->
                  publish_global_room_sidebar_named message_bus "replace" ~room_id ~name
              | "Rooms::Open", "Rooms::Closed" ->
                  publish_global_room_sidebar message_bus "remove" room;
                  List.iter (fun user_id ->
                      publish_user_room_sidebar message_bus database ~user_id ~room_id
                        ~direct:false "prepend") current_member_ids
              | "Rooms::Closed", "Rooms::Open" ->
                  publish_global_room_sidebar_named message_bus "prepend" ~room_id ~name
              | "Rooms::Closed", "Rooms::Closed" ->
                  List.iter (fun user_id ->
                      if List.mem user_id current_member_ids then
                        publish_user_room_sidebar message_bus database ~user_id ~room_id
                          ~direct:false "replace"
                      else
                        publish_user_room_sidebar message_bus database ~user_id ~room_id
                          ~direct:false "remove") previous_member_ids;
                  List.iter (fun user_id ->
                      if not (List.mem user_id previous_member_ids) then
                        publish_user_room_sidebar message_bus database ~user_id ~room_id
                          ~direct:false "prepend") current_member_ids
              | _ -> ());
              redirect ("/rooms/" ^ string_of_int room_id))
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
              html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
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
              html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
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
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
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
              html_with_session ~secure:(request_scheme headers = "https") ~headers:response_headers ~secret session `OK html)

let create_boost_request database secret headers message_id body =
  let session = load_session secret headers in
  let form = parse_form body in
  let authenticity_token = form_value form "authenticity_token" in
  let path = "/messages/" ^ string_of_int message_id ^ "/boosts" in
  if not (valid_origin headers)
     || not (Session.valid_csrf ~path ~method_:"POST" session authenticity_token)
  then
    html_with_session ~secure:(request_scheme headers = "https") ~secret session `Unprocessable_entity
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
                      html_with_session ~secure:(request_scheme headers = "https") ~headers:response_headers ~secret session `OK html
                    else redirect path
              with _ -> response `Unprocessable_entity "Boost could not be saved")

let serve_request ~sw ~database_lock ~process_mgr ~database ~jobs_database ~message_bus ~remote_ip request body =
  let path = path_of_request request in
  let headers = Cohttp.Request.headers request in
  match (Cohttp.Request.meth request, path) with
  | `GET, "/up" ->
      response `OK
        "<!doctype html><html><body style=\"background-color: green\">OK</body></html>"
  | ((`GET | `HEAD), "/account/logo") ->
      show_account_logo process_mgr database request
  | ((`GET | `HEAD), qr_path) when qr_code_route_of_path qr_path <> None ->
      qr_code_response process_mgr (Option.get (qr_code_route_of_path qr_path))
  | `GET, "/webmanifest" ->
      let name = Database.account_name database |> Option.value ~default:"Campfire" in
      json_response ~gzip:(gzip_requested headers) `OK
        (`Assoc
          [ ("name", `String name);
            ("icons",
             `List
               [ `Assoc
                   [ ("src", `String "/account/logo?size=small");
                     ("type", `String "image/png");
                     ("sizes", `String "192x192") ];
                 `Assoc
                   [ ("src", `String "/account/logo");
                     ("type", `String "image/png");
                     ("sizes", `String "512x512");
                     ("purpose", `String "maskable") ] ]);
            ("start_url", `String "/"); ("display", `String "standalone");
            ("scope", `String "/");
            ("description", `String "A chat app from the makers of Basecamp and HEY.");
            ("categories", `List (List.map (fun value -> `String value)
              [ "social"; "business"; "productivity" ]));
            ("theme_color", `String "#ffffff");
            ("background_color", `String "#ffffff");
            ("shortcuts",
             `List
               [ `Assoc
                   [ ("name", `String "New chat room");
                     ("description", `String "Open Campfire and start a new chat room");
                     ("url", `String "/rooms/opens/new") ];
                 `Assoc
                   [ ("name", `String "My profile");
                     ("description", `String "Open Campfire and view your profile");
                     ("url", `String "/users/me/profile") ] ]) ])
  | `GET, transfer_path when session_transfer_id_of_path transfer_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          session_transfer_page secret headers
            (Option.get (session_transfer_id_of_path transfer_path)))
  | `GET, "/service-worker" ->
      response ~gzip:(gzip_requested headers)
        ~headers:(Cohttp.Header.init_with "content-type" "text/javascript; charset=utf-8")
        `OK
        "self.addEventListener(\"push\",async(event)=>{const data=await event.data.json();event.waitUntil(Promise.all([self.registration.showNotification(data.title,data.options),self.navigator.setAppBadge?.(data.options?.data?.badge||0)]))});self.addEventListener(\"notificationclick\",(event)=>{event.notification.close();const url=new URL(event.notification.data.path,self.location.origin).href;event.waitUntil((async()=>{const clients=await self.clients.matchAll({type:\"window\"});const focused=clients.find((client)=>client.focused);if(focused)await focused.navigate(url);else await self.clients.openWindow(url)})())})"
  | `GET, "/assets/campfire.css" ->
      (match read_public_asset "campfire.css" with
      | Some body ->
          response ~gzip:(gzip_requested headers)
            ~headers:(Cohttp.Header.init_with "content-type" "text/css; charset=utf-8")
            `OK body
      | None -> response `Not_found "Asset not found")
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
  | `GET, asset_path
    when List.mem asset_path
           [ "/assets/menu-dots-horizontal.svg"; "/assets/boost.svg";
             "/assets/reply.svg"; "/assets/link.svg"; "/assets/pencil.svg" ] ->
      (match read_public_asset (Filename.basename asset_path) with
      | Some body ->
          response ~gzip:(gzip_requested headers)
            ~headers:(Cohttp.Header.init_with "content-type" "image/svg+xml") `OK body
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
                html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
                  (join_form code session.Session.csrf_form_token)))
  | `GET, "/first_run" ->
      if Database.account_exists database then redirect "/"
      else
        (match secret_key_base () with
        | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
        | Some secret ->
            let session = load_session secret headers in
            html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
              (first_run_form session.Session.csrf_form_token))
  | `GET, "/searches" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          let query = query_parameters request |> fun params -> form_value params "q" in
          search_index ~gzip:(gzip_requested headers) database secret headers query)
  | `GET, "/users/me/sidebar" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret -> sidebar_page ~gzip:(gzip_requested headers) database secret headers)
  | `GET, "/users/me/push_subscriptions" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret -> push_subscriptions_page database secret headers)
  | `POST, push_test_path
    when push_test_notification_route_of_path push_test_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             let body = request_body body in
             test_push_notification_request process_mgr database secret headers request
               (Option.get (push_test_notification_route_of_path push_test_path)) body
           with Request_body_too_large ->
             response `Request_entity_too_large "Request body too large"))
  | ((`POST | `DELETE), push_path)
    when push_subscriptions_route_of_path push_path <> None ->
      (match (secret_key_base (), push_subscriptions_route_of_path push_path) with
      | Some secret, Some subscription_id ->
          (try
             let body = request_body body in
             mutate_push_subscription database secret headers request push_path
               subscription_id body
           with Request_body_too_large ->
             response `Request_entity_too_large "Request body too large")
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `GET, user_path when user_id_of_path user_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret -> user_page ~gzip:(gzip_requested headers) database secret headers
          (Option.get (user_id_of_path user_path)))
  | `GET, "/account/edit" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret -> account_edit_page ~gzip:(gzip_requested headers) database secret headers)
  | ((`GET | `POST | `PUT | `PATCH | `DELETE), bot_admin_path)
    when account_bot_route_of_path bot_admin_path <> None ->
      (match (secret_key_base (), account_bot_route_of_path bot_admin_path) with
      | Some secret, Some route ->
          (try
             let body =
               match Cohttp.Request.meth request with
               | `GET -> ""
               | _ -> request_body body
             in
             account_bots_request database secret headers request bot_admin_path
               route body
           with Request_body_too_large ->
             response `Request_entity_too_large "Request body too large")
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | ((`GET | `POST | `PUT | `PATCH), custom_styles_path)
    when account_custom_styles_route_of_path custom_styles_path <> None ->
      (match (secret_key_base (), account_custom_styles_route_of_path custom_styles_path) with
      | Some secret, Some route ->
          (try
             let body =
               match Cohttp.Request.meth request with
               | `GET -> ""
               | _ -> request_body body
             in
             account_custom_styles_request database secret headers request
               custom_styles_path route body
           with Request_body_too_large ->
             response `Request_entity_too_large "Request body too large")
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `GET, "/autocompletable/users" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret -> autocomplete_users_page ~gzip:(gzip_requested headers) database secret headers request)
  | `GET, avatar_path when avatar_token_of_path avatar_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
          | Some secret -> show_avatar process_mgr database secret avatar_path)
  | `POST, "/rails/active_storage/direct_uploads" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             direct_upload_metadata_request database secret headers
               (request_body body)
           with Request_body_too_large ->
             response `Request_entity_too_large "Request body too large"
              | _ -> response `Unprocessable_entity "Invalid blob metadata"))
  | `PUT, disk_path when active_storage_disk_token_of_path disk_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             direct_upload_bytes_request secret
               (Option.get (active_storage_disk_token_of_path disk_path))
               (request_body ~max_size:52_428_800 body)
           with Request_body_too_large ->
             response `Request_entity_too_large "Request body too large"
              | _ -> response `Unprocessable_entity "Upload could not be stored"))
  | `GET, representation_path
    when active_storage_representation_of_path representation_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          let signed_id, variation =
            Option.get (active_storage_representation_of_path representation_path)
          in
          show_active_storage_representation process_mgr database secret headers
            signed_id variation)
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
      | Some secret -> profile_page ~gzip:(gzip_requested headers) database secret headers ())
  | `POST, "/account/join_code" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try regenerate_join_code_request database secret headers (request_body body)
           with Request_body_too_large -> response `Request_entity_too_large "Request body too large"
              | _ -> response `Bad_request "Invalid account request"))
  | `POST, member_path when account_user_id_of_path member_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             let body = request_body body in
             let form, _ = parse_request_form headers body in
             let method_ = String.uppercase_ascii (form_value form "_method") in
             if method_ <> "PATCH" && method_ <> "DELETE" then
               response `Method_not_allowed "Method not allowed"
             else account_admin_request database secret headers body member_path
                    (Option.get (account_user_id_of_path member_path)) method_
           with Request_body_too_large -> response `Request_entity_too_large "Request body too large"
              | _ -> response `Bad_request "Invalid account request"))
  | (`POST | `DELETE), ban_path when user_ban_id_of_path ban_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             let body = request_body body in
             let method_ = match Cohttp.Request.meth request with
               | `DELETE -> "DELETE"
               | `POST ->
                   let override = parse_form body |> fun form -> form_value form "_method"
                       |> String.uppercase_ascii in
                   if override = "DELETE" then override else "POST"
               | _ -> "" in
             user_ban_request message_bus database secret headers
               (Option.get (user_ban_id_of_path ban_path)) method_ body
           with Request_body_too_large -> response `Request_entity_too_large "Request body too large"
              | _ -> response `Bad_request "Invalid ban request"))
  | (`POST | `PATCH | `PUT), "/account" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             let body = request_body ~max_size:54_525_952 body in
             let method_ =
               match Cohttp.Request.meth request with
               | `PATCH -> "PATCH"
               | `PUT -> "PUT"
               | `POST ->
                   parse_request_form headers body |> fst |> fun form -> form_value form "_method"
                   |> String.uppercase_ascii
               | _ -> ""
             in
             if method_ <> "PATCH" && method_ <> "PUT" then
               response `Method_not_allowed "Method not allowed"
             else update_account_request database secret headers body method_
          with Request_body_too_large ->
             response `Request_entity_too_large "Request body too large"
              | _ -> response `Bad_request "Invalid account request"))
  | (`POST | `DELETE), "/account/logo" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             let body = request_body body in
             let method_ =
               match Cohttp.Request.meth request with
               | `DELETE -> "DELETE"
               | `POST ->
                   let override = parse_request_form headers body |> fst |> fun form -> form_value form "_method"
                       |> String.uppercase_ascii in
                   if override = "DELETE" then override else "POST"
               | _ -> ""
             in
             if method_ <> "DELETE" then response `Method_not_allowed "Method not allowed"
             else destroy_account_logo_request database secret headers body
           with Request_body_too_large -> response `Request_entity_too_large "Request body too large"
              | _ -> response `Bad_request "Invalid account logo request"))
  | (`POST | `PATCH), "/users/me/profile" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             let body = request_body ~max_size:54_525_952 body in
             let method_ =
               match Cohttp.Request.meth request with
               | `PATCH -> "PATCH"
               | `POST ->
                   parse_request_form headers body |> fst |> fun form -> form_value form "_method"
                   |> String.uppercase_ascii
               | _ -> ""
             in
             if method_ <> "PATCH" then response `Method_not_allowed "Method not allowed"
             else update_profile_request database secret headers body
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid profile update request"))
  | `POST, avatar_path when avatar_user_id_of_path avatar_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             let request_body = request_body body in
             let form = parse_form request_body in
             if String.lowercase_ascii (form_value form "_method") <> "delete" then
               response `Method_not_allowed "Method not allowed"
             else
               destroy_avatar_request database secret headers
                 (Option.get (avatar_user_id_of_path avatar_path)) request_body
           with Request_body_too_large ->
             response `Request_entity_too_large "Request body too large"
              | _ -> response `Bad_request "Invalid form request"))
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
             let compat_post =
               match Cohttp.Request.meth request with
               | `POST -> form_value (parse_form body) "_method" = ""
               | _ -> false
             in
             let method_ =
               match Cohttp.Request.meth request with
               | `PATCH -> "PATCH"
               | `POST ->
                   let override =
                     parse_form body |> fun form -> form_value form "_method"
                     |> String.uppercase_ascii
                   in
                   if override = "" then "PATCH" else override
               | _ -> ""
             in
             if method_ = "PATCH" then
               update_room_request
                 ~csrf_method:(if compat_post then "POST" else "PATCH")
                 message_bus database secret headers
                 ("/rooms/" ^ kind
                 ^ (if compat_post then "" else "/" ^ string_of_int room_id))
                 room_id body
             else if method_ = "DELETE" then
               delete_room_request message_bus database secret headers
                 room_update_path room_id false body
             else response `Method_not_allowed "Method not allowed"
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
  | `POST, ("/rooms/opens" | "/rooms/opens/new") ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try create_open_room_request message_bus database secret headers (request_body body)
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
  | `POST, ("/rooms/closeds" | "/rooms/closeds/new") ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try create_closed_room_request message_bus database secret headers (request_body body)
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid room request"))
  | `POST, ("/rooms/directs" | "/rooms/directs/new") ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             create_direct_room_request message_bus database secret headers (request_body body)
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
  | `POST, "/unfurl_link" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             unfurl_link_request process_mgr database secret headers (request_body body)
           with
           | Request_body_too_large ->
               response `Request_entity_too_large "Request body too large"
           | _ -> response `No_content ""))
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
                ~headers:(attach_session_cookie ~secure:(request_scheme headers = "https") (Cohttp.Header.init ()) ~secret session)
                "/first_run"
            else
              (match current_identity database secret headers with
              | Some identity ->
                  (match Database.rooms_for_user database identity.Database.user.id with
                  | room :: _ -> redirect ("/rooms/" ^ string_of_int room.Database.id)
                  | [] ->
                      html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
                        ("<!doctype html><html><body><h1>No rooms yet</h1><p>"
                        ^ html_escape identity.Database.user.name
                        ^ "</p></body></html>"))
              | None ->
                  redirect
                    ~headers:(attach_session_cookie ~secure:(request_scheme headers = "https") (Cohttp.Header.init ()) ~secret session)
                    "/session/new")
          else if Database.user_exists database then
            html_with_session ~secure:(request_scheme headers = "https") ~secret session `OK
              (login_form
                 ~email:(query_parameters request
                         |> fun params -> form_value params "email_address")
                 session.Session.csrf_form_token)
          else
            redirect
              ~headers:(attach_session_cookie ~secure:(request_scheme headers = "https") (Cohttp.Header.init ()) ~secret session)
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
  | (`POST | `DELETE), bot_boost_path
    when bot_boost_route_of_path bot_boost_path <> None ->
      (match (secret_key_base (), bot_boost_route_of_path bot_boost_path) with
      | Some secret, Some (room_id, bot_key, message_id, boost_id) ->
          bot_boost_request message_bus database secret request body room_id bot_key message_id
            boost_id
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | (`GET | `POST | `PATCH | `PUT | `DELETE), bot_path
    when bot_messages_route_of_path bot_path <> None ->
      (match (secret_key_base (), bot_messages_route_of_path bot_path) with
      | Some secret, Some (room_id, bot_key, message_id) ->
          bot_messages_request ~sw ~process_mgr
            ~database_lock
            ~store_upload:(store_upload ~process_mgr database)
            ~cleanup_upload:(cleanup_uploaded_blob database)
            ~remove_file:remove_storage_file message_bus database secret request body room_id
            bot_key message_id
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
              | Some room ->
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
                      room_messages_index_response ~gzip:(gzip_requested headers) database secret headers
                        identity.Database.user room messages
                  | `Absent, (`Value _ as after) ->
                      let after = match after with `Value id -> Some id | _ -> None in
                      let messages = Database.messages_for_room ?after database room_id in
                      room_messages_index_response ~gzip:(gzip_requested headers) database secret headers
                        identity.Database.user room messages
                  | `Absent, `Absent ->
                      let messages = Database.messages_for_room database room_id in
                      room_messages_index_response ~gzip:(gzip_requested headers) database secret headers
                        identity.Database.user room messages
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
          room_response ~gzip:(gzip_requested headers) database secret headers room_id messages
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
             let request_body = request_body ~max_size:54_525_952 body in
             submit_message ~sw ~process_mgr ~database_lock message_bus database secret
               headers room_id request_body
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
               delete_room_request message_bus database secret headers room_path room_id false body
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
               delete_room_request message_bus database secret headers direct_room_path room_id true body
           with
          | Request_body_too_large ->
              response `Request_entity_too_large "Request body too large"
          | _ -> response `Bad_request "Invalid direct-room deletion request")
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `GET, refresh_path when room_refresh_id_of_path refresh_path <> None ->
      (match (secret_key_base (), room_refresh_id_of_path refresh_path) with
      | Some secret, Some room_id ->
          refresh_room_request database secret headers room_id request
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | `GET, room_path ->
      (match (secret_key_base (), room_id_of_path room_path) with
      | Some secret, Some room_id ->
          let messages = Database.messages_for_room database room_id in
          room_response ~gzip:(gzip_requested headers) database secret headers room_id messages
            ~older_messages:true ~newer_messages:true
      | None, _ -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | _, None -> response `Not_found "Not found")
  | (`POST | `PUT | `PATCH), transfer_path
    when session_transfer_id_of_path transfer_path <> None ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             let request_body = request_body body in
             let method_ = match Cohttp.Request.meth request with
               | `PUT -> "PUT"
               | `PATCH -> "PATCH"
               | `POST ->
                   parse_form request_body |> fun form -> form_value form "_method"
                   |> String.uppercase_ascii
               | _ -> "" in
             if method_ <> "PUT" && method_ <> "PATCH" then
               response `Method_not_allowed "Method not allowed"
             else update_session_transfer_request database secret headers remote_ip
                    (Option.get (session_transfer_id_of_path transfer_path))
                    method_ request_body
           with Request_body_too_large -> response `Request_entity_too_large "Request body too large"
              | _ -> response `Bad_request "Invalid transfer request"))
  | `POST, "/session" ->
      (match secret_key_base () with
      | None -> response `Internal_server_error "SECRET_KEY_BASE is required"
      | Some secret ->
          (try
             let request_body = request_body body in
           if String.length request_body > 1_048_576 then
              response `Request_entity_too_large "Request body too large"
           else
             let form = parse_form request_body in
             if String.lowercase_ascii (form_value form "_method") = "delete" then
               logout database secret headers request_body
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
  | Some "rooms" when channel = "Turbo::StreamsChannel" -> Some `Global_sidebar
  | Some stream_name
    when channel = "Turbo::StreamsChannel" && String.ends_with ~suffix:":rooms" stream_name ->
      let gid = String.sub stream_name 0 (String.length stream_name - 6) in
      (match Rails_crypto.base64_decode gid |> Option.map (String.split_on_char '/') with
      | Some [ "gid:"; ""; "campfire"; "User"; raw_id ]
        when int_of_string_opt raw_id = Some user_id -> Some (`User_sidebar user_id)
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
        | Some (`String "TypingNotificationsChannel") ->
            Option.bind (List.assoc_opt "room_id" fields)
              (function `Int room_id -> Some ("TypingNotificationsChannel", `Room_id room_id) | _ -> None)
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
  | "TypingNotificationsChannel", `Room_id room_id ->
      Option.map (fun _ -> `Typing room_id)
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
            | `Global_sidebar ->
                Some
                  (Cable_bus.subscribe_sidebar_global message_bus ~identifier
                     ~queue:events)
            | `User_sidebar user_id ->
                Some
                  (Cable_bus.subscribe_sidebar_user message_bus ~user_id
                     ~identifier ~queue:events)
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
            | `Typing room_id ->
                Some
                  (Cable_bus.subscribe_typing message_bus ~room_id ~identifier
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
      | Cable_bus.Message
          (identifier, room_id, room_name, (message : Database.message), boosts, attachments)
        when List.exists (fun (known, _, _) -> known = identifier) !subscriptions ->
          let rendered_message =
            database_read (fun () ->
              render_live_message ~secret ~database ~room_name ~boosts ~attachments
                identity.Database.user room_id csrf message)
          in
          let html =
            "<turbo-stream action=\"append\" target=\"room_"
            ^ string_of_int room_id ^ "_messages\"><template>"
            ^ rendered_message
            ^ "</template></turbo-stream>"
          in
          send_stream identifier html
      | Cable_bus.Replace
          (identifier, room_id, room_name, (message : Database.message), boosts, attachments)
        when List.exists (fun (known, _, _) -> known = identifier) !subscriptions ->
          let rendered_message =
            database_read (fun () ->
              render_live_message ~secret ~database ~room_name ~boosts ~attachments
                identity.Database.user room_id csrf message)
          in
          let html =
            "<turbo-stream action=\"replace\" target=\"message_"
            ^ html_escape message.Database.client_message_id ^ "\"><template>"
            ^ rendered_message
            ^ "</template></turbo-stream>"
          in
          send_stream identifier html
      | Cable_bus.Remove (identifier, message_dom_id)
        when List.exists (fun (known, _, _) -> known = identifier) !subscriptions ->
          send_stream identifier
            ("<turbo-stream action=\"remove\" target=\""
            ^ html_escape message_dom_id ^ "\"></turbo-stream>")
      | Cable_bus.Boost_append (identifier, target, html)
        when List.exists (fun (known, _, _) -> known = identifier) !subscriptions ->
          send_stream identifier
            ("<turbo-stream action=\"append\" target=\""
            ^ html_escape target ^ "\"><template>" ^ html
            ^ "</template></turbo-stream>")
      | Cable_bus.Boost_remove (identifier, boost_dom_id)
        when List.exists (fun (known, _, _) -> known = identifier) !subscriptions ->
          send_stream identifier
            ("<turbo-stream action=\"remove\" target=\""
            ^ html_escape boost_dom_id ^ "\"></turbo-stream>")
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
      | Cable_bus.Sidebar (identifier, html)
        when List.exists (fun (known, _, _) -> known = identifier) !subscriptions ->
          send_stream identifier html
      | Cable_bus.Typing (identifier, action, user_id, user_name)
        when List.exists (fun (known, _, _) -> known = identifier) !subscriptions ->
          write_json
            (`Assoc
              [ ("identifier", `String identifier);
                ( "message",
                  `Assoc
                    [ ("action", `String action);
                      ( "user",
                        `Assoc
                          [ ("id", `Int user_id); ("name", `String user_name) ] ) ] ) ])
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
  let channel_action identifier action =
    match List.find_opt (fun (known, _, _) -> known = identifier) !subscriptions with
    | Some (_, `Typing room_id, _) when action = "start" || action = "stop" ->
        Cable_bus.publish_typing message_bus ~room_id ~action
          ~user_id:identity.Database.user.Database.id
          ~user_name:identity.Database.user.Database.name
    | _ -> presence_action identifier action
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
                                  | Some (`String action) -> channel_action identifier action
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

let serve ~database ~jobs_database ~port ~domains ~bind_address =
  Eio_main.run (fun env ->
      let message_bus = Cable_bus.create () in
      let database_lock = Eio.Mutex.create () in
      Eio.Switch.run (fun sw ->
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
                      (serve_request ~sw ~database_lock
                         ~process_mgr:(Eio.Stdenv.process_mgr env)
                         ~database ~jobs_database ~message_bus
                         ~remote_ip:(remote_ip connection) request body)
                  in
                  match Cohttp.Request.meth request with
                  | `GET | `HEAD -> Eio.Mutex.use_ro database_lock respond
                  | _ -> Eio.Mutex.use_rw ~protect:true database_lock respond)
              ()
          in
          let socket =
            Eio.Net.listen ~reuse_addr:true ~backlog:128 ~sw env#net
              (`Tcp (bind_address, port))
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
  let bind_address =
    match Sys.getenv_opt "HTTP_BIND_ADDRESS" with
    | None | Some "0.0.0.0" -> Eio.Net.Ipaddr.V4.any
    | Some "127.0.0.1" -> Eio.Net.Ipaddr.V4.loopback
    | Some "::1" -> Eio.Net.Ipaddr.V6.loopback
    | Some value -> failwith ("unsupported HTTP_BIND_ADDRESS: " ^ value)
  in
  let domains =
    match Sys.getenv_opt "WEB_WORKERS" with
    | None -> 1
    | Some value -> (try max 1 (min 64 (int_of_string value)) with Failure _ -> 4)
  in
  let storage_root =
    match Sys.getenv_opt "CAMPFIRE_STORAGE_PATH" with
    | Some value when value <> "" -> value
    | _ -> Sys.getenv_opt "STORAGE_PATH" |> Option.value ~default:"storage"
  in
  let database = Database.open_existing storage_root in
  current_custom_styles := Database.account_custom_styles database;
  let jobs_database = Database.open_jobs storage_root in
  Fun.protect
    ~finally:(fun () ->
      Option.iter (fun db -> ignore (Sqlite3.db_close db)) database;
      ignore (Sqlite3.db_close jobs_database))
    (fun () -> serve ~database ~jobs_database ~port ~domains ~bind_address)
