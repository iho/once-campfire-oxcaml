type t = {
  values : (string * Yojson.Basic.t) list;
  csrf_secret : string;
  csrf_form_token : string;
  dirty : bool;
}

let assoc = function `Assoc values -> values | _ -> []

let get session key = List.assoc_opt key session.values

let get_string session key =
  match get session key with Some (`String value) -> Some value | _ -> None

let set session key value =
  let rec replace = function
    | [] -> [ (key, value) ]
    | (existing, _) :: tail when existing = key -> (key, value) :: tail
    | head :: tail -> head :: replace tail
  in
  { session with values = replace session.values; dirty = true }

let remove session key =
  let values = List.remove_assoc key session.values in
  if List.length values = List.length session.values then session
  else { session with values; dirty = true }

let values session = `Assoc session.values

let xor left right =
  String.mapi
    (fun index character ->
      Char.chr (Char.code character lxor Char.code right.[index]))
    left

let make_form_token csrf_secret =
  let pad = Rails_crypto.random_bytes 32 in
  Rails_crypto.base64url_encode (pad ^ xor pad csrf_secret)

let clear _session =
  let csrf_secret = Rails_crypto.random_bytes 32 in
  { values = [];
    csrf_secret;
    csrf_form_token = make_form_token csrf_secret;
    dirty = true }

let load ~secret cookie =
  let decoded =
    Option.bind cookie (Rails_crypto.decrypt_cookie ~secret ~name:"_campfire_session")
  in
  let original_values = Option.fold ~none:[] ~some:assoc decoded in
  let values = ref original_values in
  let dirty = ref (decoded = None) in
  let find key = List.assoc_opt key !values in
  let put key value =
    let rec replace = function
      | [] -> [ (key, value) ]
      | (existing, _) :: tail when existing = key -> (key, value) :: tail
      | head :: tail -> head :: replace tail
    in
    values := replace !values;
    dirty := true
  in
  (match find "session_id" with
  | Some (`String value) when value <> "" -> ()
  | _ -> put "session_id" (`String (Rails_crypto.random_bytes 16 |> Rails_crypto.hex)));
  let csrf_secret =
    match find "_csrf_token" with
    | Some (`String encoded) ->
        (match Rails_crypto.base64_decode encoded with
        | Some raw when String.length raw = 32 -> raw
        | _ ->
            let raw = Rails_crypto.random_bytes 32 in
            put "_csrf_token" (`String (Rails_crypto.base64_encode raw));
            raw)
    | _ ->
        let raw = Rails_crypto.random_bytes 32 in
        put "_csrf_token" (`String (Rails_crypto.base64_encode raw));
        raw
  in
  { values = !values;
    csrf_secret;
    csrf_form_token = make_form_token csrf_secret;
    dirty = !dirty }

let valid_csrf ?path ?method_ session token =
  match Rails_crypto.base64_decode token with
  | Some decoded ->
      let unmasked =
        match String.length decoded with
        | 32 -> Some decoded
        | 64 ->
            let pad = String.sub decoded 0 32 in
            let masked = String.sub decoded 32 32 in
            Some (xor pad masked)
        | _ -> None
      in
      (match unmasked with
      | None -> false
      | Some value ->
          let candidates =
            [ session.csrf_secret;
              Rails_crypto.hmac 2 session.csrf_secret "!real_csrf_token" ]
          in
          let candidates =
            match (path, method_) with
            | Some path, Some method_ ->
                let path =
                  if String.ends_with ~suffix:"/" path then
                    String.sub path 0 (String.length path - 1)
                  else path
                in
                candidates
                @ [ Rails_crypto.hmac 2 session.csrf_secret
                      (path ^ "#" ^ String.lowercase_ascii method_) ]
            | _ -> candidates
          in
          List.exists
            (Rails_crypto.constant_time_equal value)
            candidates)
  | _ -> false

let set_cookie ~secret session =
  if not session.dirty then None
  else
    let expires_at, expires_header = Rails_crypto.cookie_expiration () in
    let value =
      Rails_crypto.encrypt_cookie ~secret ~name:"_campfire_session"
        ~expires_at (values session)
    in
    Some
      (Printf.sprintf
         "_campfire_session=%s; Path=/; Max-Age=631152000; Expires=%s; HttpOnly; SameSite=Lax"
         value expires_header)
