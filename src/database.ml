type t = Sqlite3.db

type user = {
  id : int;
  name : string;
  password_digest : string option;
  role : int;
}

type profile = { id : int; name : string; email_address : string; bio : string }
type avatar_user = { id : int; name : string; role : int }
type avatar_blob = { key : string; content_type : string }

type identity = { session_id : int; user : user }
type room = { id : int; name : string; kind : string; creator_id : int }
type sidebar_room = { room : room; unread : bool }
type user_option = { id : int; name : string }
type message = {
  id : int;
  creator_id : int;
  client_message_id : string;
  creator_name : string;
  body_html : string;
  created_at : string;
}
type boost = {
  id : int;
  message_id : int;
  booster_id : int;
  booster_name : string;
  content : string;
  created_at : string;
}
type message_attachment = {
  message_id : int;
  blob_id : int;
  key : string;
  filename : string;
  content_type : string;
  byte_size : int;
}
type stored_blob = {
  id : int;
  key : string;
  filename : string;
  content_type : string;
  byte_size : int;
}
type search_result = { message : message; room_id : int; room_name : string }

exception Message_not_found
exception Message_not_authorized
exception Message_has_attachments
exception Room_not_found
exception Room_not_authorized
exception Room_has_attachments
exception Invalid_join_code
exception Duplicate_email
exception Duplicate_profile_email

let path storage_root =
  Filename.concat (Filename.concat storage_root "db") "production.sqlite3"

let exists db sql =
  let statement = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize statement))
    (fun () ->
      match Sqlite3.step statement with
      | Sqlite3.Rc.ROW -> true
      | Sqlite3.Rc.DONE -> false
      | error ->
          failwith
            (Printf.sprintf "SQLite query failed (%s): %s"
               (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)))

let account_exists = function
  | None -> false
  | Some db -> exists db "SELECT 1 FROM accounts LIMIT 1"

let user_exists = function
  | None -> false
  | Some db -> exists db "SELECT 1 FROM users LIMIT 1"

let check_rc db operation = function
  | Sqlite3.Rc.OK -> ()
  | error ->
      failwith
        (Printf.sprintf "%s failed (%s): %s" operation
           (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))

let with_statement db sql f =
  let statement = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize statement))
    (fun () -> f statement)

let generate_join_code () =
  let alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" in
  let output = Buffer.create 12 in
  while Buffer.length output < 12 do
    Rails_crypto.random_bytes 16
    |> String.iter (fun character ->
           let value = Char.code character in
           if value < 248 && Buffer.length output < 12 then
             Buffer.add_char output alphabet.[value mod String.length alphabet])
  done;
  let code = Buffer.contents output in
  String.sub code 0 4 ^ "-" ^ String.sub code 4 4 ^ "-" ^ String.sub code 8 4

let create_first_run database ~name ~email_address ~password_digest ~timestamp =
  Option.bind database (fun db ->
      let run sql =
        match Sqlite3.exec db sql with
        | Sqlite3.Rc.OK -> ()
        | error ->
            failwith
              (Printf.sprintf "first-run setup failed (%s): %s"
                 (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
      in
      let bind ?(start = 1) statement values =
        List.iteri
          (fun index value ->
            check_rc db "bind first-run value"
              (Sqlite3.bind_text statement (index + start) value))
          values;
        match Sqlite3.step statement with
        | Sqlite3.Rc.DONE -> ()
        | error ->
            failwith
              (Printf.sprintf "first-run insert failed (%s): %s"
                 (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
      in
      run "BEGIN IMMEDIATE";
      try
        if exists db "SELECT 1 FROM accounts LIMIT 1" then
          failwith "Campfire has already been set up";
        with_statement db
          "INSERT INTO accounts(name,join_code,created_at,updated_at) VALUES(?,?,?,?)"
          (fun statement ->
            bind statement
              [ "Campfire"; generate_join_code ();
                timestamp; timestamp ]);
        with_statement db
          "INSERT INTO users(name,email_address,password_digest,role,status,created_at,updated_at) VALUES(?,?,?,1,0,?,?)"
          (fun statement ->
            bind statement [ name; email_address; password_digest; timestamp; timestamp ]);
        let user_id =
          with_statement db "SELECT last_insert_rowid()" (fun statement ->
              match Sqlite3.step statement with
              | Sqlite3.Rc.ROW -> Sqlite3.column_int statement 0
              | _ -> failwith "first-run user insert returned no row")
        in
        with_statement db
          "INSERT INTO rooms(name,type,creator_id,created_at,updated_at) VALUES('All Talk','Rooms::Open',?,?,?)"
          (fun statement ->
            check_rc db "bind initial room creator"
              (Sqlite3.bind_int statement 1 user_id);
            bind ~start:2 statement [ timestamp; timestamp ]);
        let room_id =
          with_statement db "SELECT last_insert_rowid()" (fun statement ->
              match Sqlite3.step statement with
              | Sqlite3.Rc.ROW -> Sqlite3.column_int statement 0
              | _ -> failwith "first-run room insert returned no row")
        in
        with_statement db
          "INSERT INTO memberships(room_id,user_id,created_at,updated_at) VALUES(?,?,?,?)"
          (fun statement ->
            check_rc db "bind initial membership room"
              (Sqlite3.bind_int statement 1 room_id);
            check_rc db "bind initial membership user"
              (Sqlite3.bind_int statement 2 user_id);
            bind ~start:3 statement [ timestamp; timestamp ]);
        run "COMMIT";
        Some user_id
      with error ->
        (try run "ROLLBACK" with _ -> ());
        raise error)

let create_join_user database ~join_code ~name ~email_address ~password_digest
    ~timestamp =
  Option.bind database (fun db ->
      let run sql =
        match Sqlite3.exec db sql with
        | Sqlite3.Rc.OK -> ()
        | error ->
            failwith
              (Printf.sprintf "join transaction failed (%s): %s"
                 (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
      in
      run "BEGIN IMMEDIATE";
      try
        let current_join_code =
          with_statement db "SELECT join_code FROM accounts ORDER BY id LIMIT 1"
            (fun statement ->
              match Sqlite3.step statement with
              | Sqlite3.Rc.ROW -> Sqlite3.column_text statement 0
              | Sqlite3.Rc.DONE -> ""
              | error ->
                  failwith
                    (Printf.sprintf "join-code lookup failed (%s): %s"
                       (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)))
        in
        if join_code = "" || current_join_code <> join_code then
          raise Invalid_join_code;
        with_statement db
          "INSERT INTO users(name,email_address,password_digest,role,status,created_at,updated_at) VALUES(?,?,?,0,0,?,?)"
          (fun statement ->
            List.iteri
              (fun index value ->
                check_rc db "bind joined user"
                  (Sqlite3.bind_text statement (index + 1) value))
              [ name; email_address; password_digest; timestamp; timestamp ];
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | Sqlite3.Rc.CONSTRAINT -> raise Duplicate_email
            | error ->
                failwith
                  (Printf.sprintf "joined user insert failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        let user_id =
          with_statement db "SELECT last_insert_rowid()" (fun statement ->
              match Sqlite3.step statement with
              | Sqlite3.Rc.ROW -> Sqlite3.column_int statement 0
              | _ -> failwith "joined user insert returned no row")
        in
        with_statement db
          "INSERT INTO memberships(room_id,user_id,created_at,updated_at) SELECT id,?,?,? FROM rooms WHERE type='Rooms::Open'"
          (fun statement ->
            check_rc db "bind joined-user membership id"
              (Sqlite3.bind_int statement 1 user_id);
            check_rc db "bind joined-user membership created time"
              (Sqlite3.bind_text statement 2 timestamp);
            check_rc db "bind joined-user membership updated time"
              (Sqlite3.bind_text statement 3 timestamp);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "joined-user memberships insert failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        run "COMMIT";
        Some user_id
      with error ->
        (try run "ROLLBACK" with _ -> ());
        raise error)

let valid_join_code database join_code =
  Option.fold ~none:false
    ~some:(fun db ->
      with_statement db "SELECT 1 FROM accounts WHERE join_code=? LIMIT 1"
        (fun statement ->
          check_rc db "bind invitation join code"
            (Sqlite3.bind_text statement 1 join_code);
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW -> true
          | Sqlite3.Rc.DONE -> false
          | error ->
              failwith
                (Printf.sprintf "invitation lookup failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
    database

let find_active_user database email_address =
  Option.bind database (fun db ->
      with_statement db
        "SELECT id,name,password_digest,role FROM users WHERE email_address=? AND status=0 LIMIT 1"
        (fun statement ->
          check_rc db "bind user email"
            (Sqlite3.bind_text statement 1 email_address);
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE -> None
          | Sqlite3.Rc.ROW ->
              Some
                { id = Sqlite3.column_int statement 0;
                  name = Sqlite3.column_text statement 1;
                  password_digest =
                    (match Sqlite3.column statement 2 with
                    | Sqlite3.Data.NULL -> None
                    | _ -> Some (Sqlite3.column_text statement 2));
                  role = Sqlite3.column_int statement 3 }
          | error ->
              failwith
                (Printf.sprintf "user lookup failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))

let find_profile database user_id =
  Option.bind database (fun db ->
      with_statement db
        "SELECT id,name,COALESCE(email_address,''),COALESCE(bio,'') FROM users WHERE id=? AND status=0 LIMIT 1"
        (fun statement ->
          check_rc db "bind profile user" (Sqlite3.bind_int statement 1 user_id);
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW ->
              Some
                { id = Sqlite3.column_int statement 0;
                  name = Sqlite3.column_text statement 1;
                  email_address = Sqlite3.column_text statement 2;
                  bio = Sqlite3.column_text statement 3 }
          | Sqlite3.Rc.DONE -> None
          | error ->
              failwith
                (Printf.sprintf "profile lookup failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))

let find_avatar_user database user_id =
  Option.bind database (fun db ->
      with_statement db "SELECT id,name,role FROM users WHERE id=? LIMIT 1"
        (fun statement ->
          check_rc db "bind avatar user" (Sqlite3.bind_int statement 1 user_id);
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW ->
              Some
                { id = Sqlite3.column_int statement 0;
                  name = Sqlite3.column_text statement 1;
                  role = Sqlite3.column_int statement 2 }
          | Sqlite3.Rc.DONE -> None
          | error ->
              failwith
                (Printf.sprintf "avatar user lookup failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))

let find_avatar_blob database user_id =
  Option.bind database (fun db ->
      with_statement db
        "SELECT b.key,COALESCE(b.content_type,'application/octet-stream') FROM active_storage_attachments a JOIN active_storage_blobs b ON b.id=a.blob_id WHERE a.record_type='User' AND a.record_id=? AND a.name='avatar' LIMIT 1"
        (fun statement ->
          check_rc db "bind avatar attachment user"
            (Sqlite3.bind_int statement 1 user_id);
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW ->
              Some
                { key = Sqlite3.column_text statement 0;
                  content_type = Sqlite3.column_text statement 1 }
          | Sqlite3.Rc.DONE -> None
          | error ->
              failwith
                (Printf.sprintf "avatar attachment lookup failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))

let update_profile database ~user_id ~name ~email_address ~password_digest ~bio
    ~timestamp =
  Option.fold ~none:false
      ~some:(fun db ->
        let run sql =
          match Sqlite3.exec db sql with
          | Sqlite3.Rc.OK -> ()
          | error ->
              failwith
                (Printf.sprintf "profile transaction failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
        in
        run "BEGIN IMMEDIATE";
        try
          with_statement db
            "UPDATE users SET name=?,email_address=?,password_digest=COALESCE(?,password_digest),bio=?,updated_at=? WHERE id=? AND status=0"
            (fun statement ->
              List.iteri
                (fun index value ->
                  check_rc db "bind profile field"
                    (Sqlite3.bind_text statement (index + 1) value))
                [ name; email_address ];
              (match password_digest with
              | None ->
                  check_rc db "bind profile password"
                    (Sqlite3.bind statement 3 Sqlite3.Data.NULL)
              | Some digest ->
                  check_rc db "bind profile password"
                    (Sqlite3.bind_text statement 3 digest));
              check_rc db "bind profile bio" (Sqlite3.bind_text statement 4 bio);
              check_rc db "bind profile timestamp" (Sqlite3.bind_text statement 5 timestamp);
              check_rc db "bind profile user id" (Sqlite3.bind_int statement 6 user_id);
              match Sqlite3.step statement with
              | Sqlite3.Rc.DONE when Sqlite3.changes db = 1 -> ()
              | Sqlite3.Rc.DONE -> failwith "active profile not found"
              | Sqlite3.Rc.CONSTRAINT -> raise Duplicate_profile_email
              | error ->
                  failwith
                    (Printf.sprintf "profile update failed (%s): %s"
                       (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
          run "COMMIT";
          true
        with
        | Duplicate_profile_email ->
            (try run "ROLLBACK" with _ -> ());
            false
        | error ->
            (try run "ROLLBACK" with _ -> ());
            raise error)
      database

let create_session database ~user_id ~token ~user_agent ~ip_address ~timestamp =
  Option.iter
    (fun db ->
      with_statement db
        "INSERT INTO sessions(user_id,token,user_agent,ip_address,last_active_at,created_at,updated_at) VALUES(?,?,?,?,?,?,?)"
        (fun statement ->
          List.iteri
            (fun index value ->
              check_rc db "bind session"
                (Sqlite3.bind_text statement (index + 1) value))
            [ string_of_int user_id; token; user_agent; ip_address; timestamp;
              timestamp; timestamp ];
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE -> ()
          | error ->
              failwith
                (Printf.sprintf "session insert failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
    database

let find_session_identity database token =
  Option.bind database (fun db ->
      with_statement db
        "SELECT s.id,u.id,u.name,u.password_digest,u.role FROM sessions s JOIN users u ON u.id=s.user_id WHERE s.token=? AND u.status=0 AND u.role<>2 LIMIT 1"
        (fun statement ->
          check_rc db "bind session token" (Sqlite3.bind_text statement 1 token);
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE -> None
          | Sqlite3.Rc.ROW ->
              Some
                { session_id = Sqlite3.column_int statement 0;
                  user =
                    { id = Sqlite3.column_int statement 1;
                      name = Sqlite3.column_text statement 2;
                      password_digest =
                        (match Sqlite3.column statement 3 with
                        | Sqlite3.Data.NULL -> None
                        | _ -> Some (Sqlite3.column_text statement 3));
                   role = Sqlite3.column_int statement 4 } }
          | error ->
              failwith
                (Printf.sprintf "session lookup failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))

let rooms_for_user database user_id =
  Option.fold ~none:[]
    ~some:(fun db ->
      with_statement db
        "SELECT r.id,CASE WHEN r.type='Rooms::Direct' THEN COALESCE((SELECT group_concat(name, ', ') FROM (SELECT u.name AS name FROM memberships dm JOIN users u ON u.id=dm.user_id WHERE dm.room_id=r.id AND dm.user_id<>? ORDER BY lower(u.name))), 'Direct conversation') ELSE COALESCE(r.name,'') END,r.type,r.creator_id FROM rooms r JOIN memberships m ON m.room_id=r.id WHERE m.user_id=? ORDER BY r.created_at,r.id"
        (fun statement ->
          check_rc db "bind direct-room display user"
            (Sqlite3.bind_int statement 1 user_id);
          check_rc db "bind room membership user"
            (Sqlite3.bind_int statement 2 user_id);
          let rec collect rooms =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW ->
                collect
                  ({ id = Sqlite3.column_int statement 0;
                     name = Sqlite3.column_text statement 1;
                     kind = Sqlite3.column_text statement 2;
                     creator_id = Sqlite3.column_int statement 3 }
                  :: rooms)
            | Sqlite3.Rc.DONE -> List.rev rooms
            | error ->
                failwith
                  (Printf.sprintf "room lookup failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
          in
          collect []))
    database

let sidebar_rooms database user_id =
  Option.fold ~none:[]
    ~some:(fun db ->
      with_statement db
        "SELECT r.id,CASE WHEN r.type='Rooms::Direct' THEN COALESCE((SELECT group_concat(name, ', ') FROM (SELECT u.name AS name FROM memberships dm JOIN users u ON u.id=dm.user_id WHERE dm.room_id=r.id AND dm.user_id<>? ORDER BY lower(u.name))), 'Direct conversation') ELSE COALESCE(r.name,'') END,r.type,r.creator_id,(m.unread_at IS NOT NULL) FROM rooms r JOIN memberships m ON m.room_id=r.id WHERE m.user_id=? AND COALESCE(m.involvement,'')<>'invisible' ORDER BY CASE WHEN r.type='Rooms::Direct' THEN 0 ELSE 1 END,CASE WHEN r.type='Rooms::Direct' THEN r.updated_at END DESC,LOWER(COALESCE(r.name,''))"
        (fun statement ->
          check_rc db "bind sidebar direct-room display user"
            (Sqlite3.bind_int statement 1 user_id);
          check_rc db "bind sidebar membership user"
            (Sqlite3.bind_int statement 2 user_id);
          let rec collect rooms =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW ->
                let room =
                  { id = Sqlite3.column_int statement 0;
                    name = Sqlite3.column_text statement 1;
                    kind = Sqlite3.column_text statement 2;
                    creator_id = Sqlite3.column_int statement 3 }
                in
                collect
                  ({ room; unread = Sqlite3.column_int statement 4 <> 0 }
                  :: rooms)
            | Sqlite3.Rc.DONE -> List.rev rooms
            | error ->
                failwith
                  (Printf.sprintf "sidebar room lookup failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
          in
          collect []))
    database

let find_room_for_user database user_id room_id =
  rooms_for_user database user_id
  |> List.find_opt (fun (room : room) -> room.id = room_id)

let room_member_ids database room_id =
  Option.fold ~none:[] ~some:(fun db ->
      with_statement db
        "SELECT user_id FROM memberships WHERE room_id=? ORDER BY user_id"
        (fun statement ->
          check_rc db "bind room member list" (Sqlite3.bind_int statement 1 room_id);
          let rec collect ids =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW -> collect (Sqlite3.column_int statement 0 :: ids)
            | Sqlite3.Rc.DONE -> List.rev ids
            | error ->
                failwith
                  (Printf.sprintf "room members lookup failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
          in
          collect []))
    database

let membership_involvement database ~room_id ~user_id =
  Option.bind database (fun db ->
      with_statement db
        "SELECT involvement FROM memberships WHERE room_id=? AND user_id=? LIMIT 1"
        (fun statement ->
          check_rc db "bind involvement room" (Sqlite3.bind_int statement 1 room_id);
          check_rc db "bind involvement user" (Sqlite3.bind_int statement 2 user_id);
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW -> Some (Sqlite3.column_text statement 0)
          | Sqlite3.Rc.DONE -> None
          | error ->
              failwith
                (Printf.sprintf "membership involvement lookup failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))

let update_membership_involvement database ~room_id ~user_id ~involvement
    ~timestamp =
  let valid = [ "invisible"; "nothing"; "mentions"; "everything" ] in
  if not (List.mem involvement valid) then false
  else
    Option.fold ~none:false
      ~some:(fun db ->
        with_statement db
          "UPDATE memberships SET involvement=?,updated_at=? WHERE room_id=? AND user_id=?"
          (fun statement ->
            List.iteri
              (fun index value ->
                check_rc db "bind membership involvement"
                  (Sqlite3.bind_text statement (index + 1) value))
              [ involvement; timestamp ];
            check_rc db "bind involvement room"
              (Sqlite3.bind_int statement 3 room_id);
            check_rc db "bind involvement user"
              (Sqlite3.bind_int statement 4 user_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> Sqlite3.changes db > 0
            | error ->
                failwith
                  (Printf.sprintf "membership involvement update failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
      database

let active_users database =
  Option.fold ~none:[]
    ~some:(fun db ->
      with_statement db "SELECT id,name FROM users WHERE status=0 ORDER BY lower(name),id"
        (fun statement ->
          let rec collect users =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW ->
                collect
                  ({ id = Sqlite3.column_int statement 0;
                     name = Sqlite3.column_text statement 1 }
                  :: users)
            | Sqlite3.Rc.DONE -> List.rev users
            | error ->
                failwith
                  (Printf.sprintf "active-user lookup failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
          in
          collect []))
    database

let room_creation_restricted database =
  Option.fold ~none:false
    ~some:(fun db ->
      with_statement db "SELECT settings FROM accounts ORDER BY id LIMIT 1" (fun statement ->
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW ->
              (match Sqlite3.column statement 0 with
              | Sqlite3.Data.NULL -> false
              | Sqlite3.Data.TEXT settings ->
                  (try
                     settings |> Yojson.Safe.from_string
                     |> Yojson.Safe.Util.member "restrict_room_creation_to_administrators"
                     |> Yojson.Safe.Util.to_bool
                   with _ -> false)
              | _ -> false)
          | Sqlite3.Rc.DONE -> false
          | error ->
              failwith
                (Printf.sprintf "account settings lookup failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
    database

let create_open_room database ~name ~creator_id ~timestamp =
  Option.bind database (fun db ->
      let run sql =
        match Sqlite3.exec db sql with
        | Sqlite3.Rc.OK -> ()
        | error ->
            failwith
              (Printf.sprintf "room transaction failed (%s): %s"
                 (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
      in
      run "BEGIN IMMEDIATE";
      try
        if not (exists db
          (Printf.sprintf "SELECT 1 FROM users WHERE id=%d AND status=0 LIMIT 1" creator_id))
        then failwith "active room creator required";
        with_statement db
          "INSERT INTO rooms(name,type,creator_id,created_at,updated_at) VALUES(?, 'Rooms::Open', ?, ?, ?)"
          (fun statement ->
            check_rc db "bind room name" (Sqlite3.bind_text statement 1 name);
            check_rc db "bind room creator" (Sqlite3.bind_int statement 2 creator_id);
            check_rc db "bind room creation time" (Sqlite3.bind_text statement 3 timestamp);
            check_rc db "bind room update time" (Sqlite3.bind_text statement 4 timestamp);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "room insert failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        let room_id =
          with_statement db "SELECT last_insert_rowid()" (fun statement ->
              match Sqlite3.step statement with
              | Sqlite3.Rc.ROW -> Sqlite3.column_int statement 0
              | _ -> failwith "room insert returned no row")
        in
        with_statement db
          "INSERT INTO memberships(room_id,user_id,created_at,updated_at) SELECT ?,id,?,? FROM users WHERE status=0"
          (fun statement ->
            check_rc db "bind new-room membership room" (Sqlite3.bind_int statement 1 room_id);
            check_rc db "bind new-room membership created time" (Sqlite3.bind_text statement 2 timestamp);
            check_rc db "bind new-room membership updated time" (Sqlite3.bind_text statement 3 timestamp);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "room memberships insert failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        run "COMMIT";
        Some room_id
      with error ->
        (try run "ROLLBACK" with _ -> ());
        raise error)

let create_closed_room database ~name ~creator_id ~member_ids ~timestamp =
  Option.bind database (fun db ->
      let run sql =
        match Sqlite3.exec db sql with
        | Sqlite3.Rc.OK -> ()
        | error ->
            failwith
              (Printf.sprintf "room transaction failed (%s): %s"
                 (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
      in
      run "BEGIN IMMEDIATE";
      try
        if not (exists db
          (Printf.sprintf "SELECT 1 FROM users WHERE id=%d AND status=0 LIMIT 1" creator_id))
        then failwith "active room creator required";
        with_statement db
          "INSERT INTO rooms(name,type,creator_id,created_at,updated_at) VALUES(?, 'Rooms::Closed', ?, ?, ?)"
          (fun statement ->
            check_rc db "bind room name" (Sqlite3.bind_text statement 1 name);
            check_rc db "bind room creator" (Sqlite3.bind_int statement 2 creator_id);
            check_rc db "bind room creation time" (Sqlite3.bind_text statement 3 timestamp);
            check_rc db "bind room update time" (Sqlite3.bind_text statement 4 timestamp);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "room insert failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        let room_id =
          with_statement db "SELECT last_insert_rowid()" (fun statement ->
              match Sqlite3.step statement with
              | Sqlite3.Rc.ROW -> Sqlite3.column_int statement 0
              | _ -> failwith "room insert returned no row")
        in
        List.sort_uniq compare (creator_id :: member_ids)
        |> List.iter (fun user_id ->
               if exists db
                    (Printf.sprintf "SELECT 1 FROM users WHERE id=%d LIMIT 1" user_id)
               then
                 with_statement db
                   "INSERT INTO memberships(room_id,user_id,created_at,updated_at) VALUES(?,?,?,?)"
                   (fun statement ->
                     check_rc db "bind closed-room membership room"
                       (Sqlite3.bind_int statement 1 room_id);
                     check_rc db "bind closed-room membership user"
                       (Sqlite3.bind_int statement 2 user_id);
                     check_rc db "bind closed-room membership created time"
                       (Sqlite3.bind_text statement 3 timestamp);
                     check_rc db "bind closed-room membership updated time"
                       (Sqlite3.bind_text statement 4 timestamp);
                     match Sqlite3.step statement with
                     | Sqlite3.Rc.DONE -> ()
                     | error ->
                         failwith
                           (Printf.sprintf "room membership insert failed (%s): %s"
                              (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))));
        run "COMMIT";
        Some room_id
      with error ->
        (try run "ROLLBACK" with _ -> ());
        raise error)

let find_or_create_direct_room database ~creator_id ~member_ids ~timestamp =
  Option.bind database (fun db ->
      let run sql =
        match Sqlite3.exec db sql with
        | Sqlite3.Rc.OK -> ()
        | error ->
            failwith
              (Printf.sprintf "direct-room transaction failed (%s): %s"
                 (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
      in
      run "BEGIN IMMEDIATE";
      try
        let active id =
          exists db
            (Printf.sprintf
               "SELECT 1 FROM users WHERE id=%d AND status=0 LIMIT 1" id)
        in
        if not (active creator_id) then failwith "active direct-room creator required";
        let user_exists id =
          exists db
            (Printf.sprintf "SELECT 1 FROM users WHERE id=%d LIMIT 1" id)
        in
        let member_ids =
          creator_id :: member_ids
          |> List.sort_uniq compare
          |> List.filter user_exists
        in
        let direct_room_ids =
          with_statement db
            "SELECT id FROM rooms WHERE type='Rooms::Direct' ORDER BY id"
            (fun statement ->
              let rec collect ids =
                match Sqlite3.step statement with
                | Sqlite3.Rc.ROW -> collect (Sqlite3.column_int statement 0 :: ids)
                | Sqlite3.Rc.DONE -> List.rev ids
                | error ->
                    failwith
                      (Printf.sprintf "direct-room lookup failed (%s): %s"
                         (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
              in
              collect [])
        in
        let existing =
          List.find_opt
            (fun room_id ->
              room_member_ids database room_id |> List.sort_uniq compare = member_ids)
            direct_room_ids
        in
        let room_id =
          match existing with
          | Some room_id -> room_id
          | None ->
              with_statement db
                "INSERT INTO rooms(name,type,creator_id,created_at,updated_at) VALUES(NULL,'Rooms::Direct',?,?,?)"
                (fun statement ->
                  check_rc db "bind direct-room creator"
                    (Sqlite3.bind_int statement 1 creator_id);
                  List.iteri
                    (fun index value ->
                      check_rc db "bind direct-room timestamp"
                        (Sqlite3.bind_text statement (index + 2) value))
                    [ timestamp; timestamp ];
                  match Sqlite3.step statement with
                  | Sqlite3.Rc.DONE -> ()
                  | error ->
                      failwith
                        (Printf.sprintf "direct-room insert failed (%s): %s"
                           (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
              let id =
                with_statement db "SELECT last_insert_rowid()" (fun statement ->
                    match Sqlite3.step statement with
                    | Sqlite3.Rc.ROW -> Sqlite3.column_int statement 0
                    | _ -> failwith "direct-room insert returned no row")
              in
              List.iter
                (fun user_id ->
                  with_statement db
                    "INSERT INTO memberships(room_id,user_id,involvement,created_at,updated_at) VALUES(?,?,'everything',?,?)"
                    (fun statement ->
                      check_rc db "bind direct membership room"
                        (Sqlite3.bind_int statement 1 id);
                      check_rc db "bind direct membership user"
                        (Sqlite3.bind_int statement 2 user_id);
                      check_rc db "bind direct membership created_at"
                        (Sqlite3.bind_text statement 3 timestamp);
                      check_rc db "bind direct membership updated_at"
                        (Sqlite3.bind_text statement 4 timestamp);
                      match Sqlite3.step statement with
                      | Sqlite3.Rc.DONE -> ()
                      | error ->
                          failwith
                            (Printf.sprintf "direct membership insert failed (%s): %s"
                               (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
                member_ids;
              id
        in
        run "COMMIT";
        Some room_id
      with error ->
        (try run "ROLLBACK" with _ -> ());
        raise error)

let can_administer_room database ~room_id ~user_id ~role =
  role = 1
  || Option.fold ~none:false
       ~some:(fun db ->
         with_statement db
           "SELECT 1 FROM rooms r JOIN memberships m ON m.room_id=r.id AND m.user_id=? WHERE r.id=? AND r.creator_id=? AND r.type<>'Rooms::Direct' LIMIT 1"
           (fun statement ->
             check_rc db "bind room admin user" (Sqlite3.bind_int statement 1 user_id);
             check_rc db "bind room admin id" (Sqlite3.bind_int statement 2 room_id);
             check_rc db "bind room creator" (Sqlite3.bind_int statement 3 user_id);
             match Sqlite3.step statement with
             | Sqlite3.Rc.ROW -> true
             | Sqlite3.Rc.DONE -> false
             | error ->
                 failwith
                   (Printf.sprintf "room authorization lookup failed (%s): %s"
                      (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
       database

let update_shared_room database ~room_id ~user_id ~role ~name ~kind ~member_ids
    ~timestamp =
  let kind =
    match kind with
    | "Rooms::Open" | "Rooms::Closed" -> Some kind
    | _ -> None
  in
  match (kind, database) with
  | None, _ | _, None -> false
  | Some kind, Some db ->
      if String.trim name = ""
         || not (exists db
                   (Printf.sprintf "SELECT 1 FROM memberships WHERE room_id=%d AND user_id=%d LIMIT 1"
                      room_id user_id))
         || not (can_administer_room database ~room_id ~user_id ~role)
      then false
      else
        let run sql =
          match Sqlite3.exec db sql with
          | Sqlite3.Rc.OK -> ()
          | error ->
              failwith
                (Printf.sprintf "room update transaction failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
        in
        run "BEGIN IMMEDIATE";
        try
          let exists_shared =
            exists db
              (Printf.sprintf
                 "SELECT 1 FROM rooms WHERE id=%d AND type<>'Rooms::Direct' LIMIT 1"
                 room_id)
          in
          if not exists_shared then (run "ROLLBACK"; false)
          else (
            with_statement db
              "UPDATE rooms SET name=?,type=?,updated_at=? WHERE id=? AND type<>'Rooms::Direct'"
              (fun statement ->
                List.iteri
                  (fun index value ->
                    check_rc db "bind room update value"
                      (Sqlite3.bind_text statement (index + 1) value))
                  [ String.trim name; kind; timestamp ];
                check_rc db "bind room update id" (Sqlite3.bind_int statement 4 room_id);
                match Sqlite3.step statement with
                | Sqlite3.Rc.DONE -> ()
                | error ->
                    failwith
                      (Printf.sprintf "room update failed (%s): %s"
                         (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
            if kind = "Rooms::Open" then
              with_statement db
                "INSERT INTO memberships(room_id,user_id,created_at,updated_at) SELECT ?,id,?,? FROM users WHERE status=0 ON CONFLICT(room_id,user_id) DO NOTHING"
                (fun statement ->
                  check_rc db "bind public-room conversion id"
                    (Sqlite3.bind_int statement 1 room_id);
                  check_rc db "bind public-room conversion created_at"
                    (Sqlite3.bind_text statement 2 timestamp);
                  check_rc db "bind public-room conversion updated_at"
                    (Sqlite3.bind_text statement 3 timestamp);
                  match Sqlite3.step statement with
                  | Sqlite3.Rc.DONE -> ()
                  | error ->
                      failwith
                        (Printf.sprintf "public-room grants failed (%s): %s"
                           (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)))
            else (
              List.sort_uniq compare member_ids
              |> List.iter (fun selected_user_id ->
                     with_statement db
                       "INSERT INTO memberships(room_id,user_id,created_at,updated_at) SELECT ?,id,?,? FROM users WHERE id=? ON CONFLICT(room_id,user_id) DO NOTHING"
                       (fun statement ->
                         check_rc db "bind selected-room id"
                           (Sqlite3.bind_int statement 1 room_id);
                         check_rc db "bind selected-room created_at"
                           (Sqlite3.bind_text statement 2 timestamp);
                         check_rc db "bind selected-room updated_at"
                           (Sqlite3.bind_text statement 3 timestamp);
                         check_rc db "bind selected user"
                           (Sqlite3.bind_int statement 4 selected_user_id);
                         match Sqlite3.step statement with
                         | Sqlite3.Rc.DONE -> ()
                         | error ->
                             failwith
                               (Printf.sprintf "selected-room grant failed (%s): %s"
                                  (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))));
              let removed =
                with_statement db
                  "SELECT user_id FROM memberships WHERE room_id=?"
                  (fun statement ->
                    check_rc db "bind room membership list"
                      (Sqlite3.bind_int statement 1 room_id);
                    let rec collect users =
                      match Sqlite3.step statement with
                      | Sqlite3.Rc.ROW ->
                          collect (Sqlite3.column_int statement 0 :: users)
                      | Sqlite3.Rc.DONE ->
                          List.filter
                            (fun existing -> not (List.mem existing member_ids))
                            users
                      | error ->
                          failwith
                            (Printf.sprintf "room membership list failed (%s): %s"
                               (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
                    in
                    collect [])
              in
              List.iter (fun removed_user_id ->
                  with_statement db
                    "DELETE FROM memberships WHERE room_id=? AND user_id=?"
                    (fun statement ->
                      check_rc db "bind room membership revoke"
                        (Sqlite3.bind_int statement 1 room_id);
                      check_rc db "bind membership revoke user"
                        (Sqlite3.bind_int statement 2 removed_user_id);
                      match Sqlite3.step statement with
                      | Sqlite3.Rc.DONE -> ()
                      | error ->
                          failwith
                            (Printf.sprintf "room membership revoke failed (%s): %s"
                               (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
                removed);
            run "COMMIT";
            true)
        with error ->
          (try run "ROLLBACK" with _ -> ());
          raise error

let messages_for_room ?before ?after ?around database room_id =
  Option.fold ~none:[]
    ~some:(fun db ->
      let select =
        "SELECT m.id,u.name,COALESCE(rt.body,''),m.created_at,m.creator_id,m.client_message_id FROM messages m JOIN users u ON u.id=m.creator_id LEFT JOIN action_text_rich_texts rt ON rt.record_type='Message' AND rt.record_id=m.id AND rt.name='body' WHERE m.room_id=?"
      in
      let pivot message_id =
        with_statement db
          "SELECT created_at FROM messages WHERE room_id=? AND id=? LIMIT 1"
          (fun statement ->
            check_rc db "bind message pivot room"
              (Sqlite3.bind_int statement 1 room_id);
            check_rc db "bind message pivot id"
              (Sqlite3.bind_int statement 2 message_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW -> Some (Sqlite3.column_text statement 0)
            | Sqlite3.Rc.DONE -> None
            | error ->
                failwith
                  (Printf.sprintf "message pivot lookup failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)))
      in
      let pivot_message message_id =
        with_statement db
          (select ^ " AND m.id=? LIMIT 1")
          (fun statement ->
            check_rc db "bind message pivot room"
              (Sqlite3.bind_int statement 1 room_id);
            check_rc db "bind message pivot id"
              (Sqlite3.bind_int statement 2 message_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW ->
                Some
                  { id = Sqlite3.column_int statement 0;
                    creator_id = Sqlite3.column_int statement 4;
                    client_message_id = Sqlite3.column_text statement 5;
                    creator_name = Sqlite3.column_text statement 1;
                    body_html = Sqlite3.column_text statement 2;
                    created_at = Sqlite3.column_text statement 3 }
            | Sqlite3.Rc.DONE -> None
            | error ->
                failwith
                  (Printf.sprintf "message pivot lookup failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)))
      in
      let query ?timestamp predicate order =
        with_statement db (select ^ predicate ^ " ORDER BY m.created_at " ^ order
                           ^ ",m.id " ^ order ^ " LIMIT 40")
          (fun statement ->
            check_rc db "bind message room" (Sqlite3.bind_int statement 1 room_id);
            Option.iter
              (fun timestamp ->
                check_rc db "bind message pivot timestamp"
                  (Sqlite3.bind_text statement 2 timestamp))
              timestamp;
            let rec collect messages =
              match Sqlite3.step statement with
              | Sqlite3.Rc.ROW ->
                  collect
                    ({ id = Sqlite3.column_int statement 0;
                       creator_id = Sqlite3.column_int statement 4;
                       client_message_id = Sqlite3.column_text statement 5;
                       creator_name = Sqlite3.column_text statement 1;
                       body_html = Sqlite3.column_text statement 2;
                       created_at = Sqlite3.column_text statement 3 }
                    :: messages)
              | Sqlite3.Rc.DONE -> List.rev messages
              | error ->
                  failwith
                    (Printf.sprintf "message lookup failed (%s): %s"
                       (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
            in
            let messages = collect [] in
            if order = "DESC" then List.rev messages else messages)
      in
      let latest () = query "" "DESC" in
      match around with
      | Some message_id ->
          (match (pivot message_id, pivot_message message_id) with
          | Some timestamp, Some message ->
              query ~timestamp " AND m.created_at<?" "DESC"
              @ [ message ]
              @ query ~timestamp " AND m.created_at>?" "ASC"
          | _ -> latest ())
      | None ->
          (match (before, after) with
          | Some message_id, _ ->
              (match pivot message_id with
              | Some timestamp -> query ~timestamp " AND m.created_at<?" "DESC"
              | None -> raise Message_not_found)
          | _, Some message_id ->
              (match pivot message_id with
              | Some timestamp -> query ~timestamp " AND m.created_at>?" "ASC"
              | None -> raise Message_not_found)
          | None, None -> latest ()))
    database

let boosts_for_messages database messages =
  match database with
  | None -> []
  | Some _ when messages = [] -> []
  | Some db ->
      let message_ids = List.map (fun (message : message) -> message.id) messages in
      let placeholders = List.map (fun _ -> "?") message_ids |> String.concat "," in
      with_statement db
        ("SELECT b.id,b.message_id,b.booster_id,u.name,b.content,b.created_at "
        ^ "FROM boosts b JOIN users u ON u.id=b.booster_id WHERE b.message_id IN ("
        ^ placeholders ^ ") ORDER BY b.created_at,b.id")
        (fun statement ->
          List.iteri
            (fun index id ->
              check_rc db "bind boosted message id" (Sqlite3.bind_int statement (index + 1) id))
            message_ids;
          let rec collect boosts =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW ->
                let boost =
                  { id = Sqlite3.column_int statement 0;
                    message_id = Sqlite3.column_int statement 1;
                    booster_id = Sqlite3.column_int statement 2;
                    booster_name = Sqlite3.column_text statement 3;
                    content = Sqlite3.column_text statement 4;
                    created_at = Sqlite3.column_text statement 5 }
                in
                collect (boost :: boosts)
            | Sqlite3.Rc.DONE -> List.rev boosts
            | error ->
                failwith
                  (Printf.sprintf "message boost lookup failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
          in
          collect []
          |> List.fold_left
               (fun grouped (boost : boost) ->
                 let previous = List.assoc_opt boost.message_id grouped |> Option.value ~default:[] in
                 let grouped = List.remove_assoc boost.message_id grouped in
                 grouped @ [ (boost.message_id, previous @ [ boost ]) ])
               [])

let room_id_for_message_user database ~message_id ~user_id =
  Option.bind database (fun db ->
      with_statement db
        "SELECT m.room_id FROM messages m JOIN memberships ms ON ms.room_id=m.room_id WHERE m.id=? AND ms.user_id=? LIMIT 1"
        (fun statement ->
          check_rc db "bind reachable message id" (Sqlite3.bind_int statement 1 message_id);
          check_rc db "bind reachable message user" (Sqlite3.bind_int statement 2 user_id);
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW -> Some (Sqlite3.column_int statement 0)
          | Sqlite3.Rc.DONE -> None
          | error ->
              failwith
                (Printf.sprintf "reachable message lookup failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))

let create_boost database ~message_id ~booster_id ~content ~timestamp =
  Option.bind database (fun db ->
      with_statement db
        "INSERT INTO boosts(message_id,booster_id,content,created_at,updated_at) VALUES(?,?,?,?,?)"
        (fun statement ->
          check_rc db "bind boost message" (Sqlite3.bind_int statement 1 message_id);
          check_rc db "bind boost author" (Sqlite3.bind_int statement 2 booster_id);
          check_rc db "bind boost content" (Sqlite3.bind_text statement 3 content);
          check_rc db "bind boost created time" (Sqlite3.bind_text statement 4 timestamp);
          check_rc db "bind boost updated time" (Sqlite3.bind_text statement 5 timestamp);
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE ->
              with_statement db "SELECT last_insert_rowid()" (fun id_statement ->
                  match Sqlite3.step id_statement with
                  | Sqlite3.Rc.ROW -> Some (Sqlite3.column_int id_statement 0)
                  | _ -> None)
          | error ->
              failwith
                (Printf.sprintf "boost insert failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))

let delete_boost database ~boost_id ~message_id ~booster_id =
  Option.fold ~none:false
    ~some:(fun db ->
      with_statement db
        "DELETE FROM boosts WHERE id=? AND message_id=? AND booster_id=?"
        (fun statement ->
          check_rc db "bind boost id" (Sqlite3.bind_int statement 1 boost_id);
          check_rc db "bind boost message" (Sqlite3.bind_int statement 2 message_id);
          check_rc db "bind boost author" (Sqlite3.bind_int statement 3 booster_id);
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE -> Sqlite3.changes db = 1
          | error ->
              failwith
                (Printf.sprintf "boost deletion failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
    database

let attachments_for_messages database messages =
  match database with
  | None -> []
  | Some _ when messages = [] -> []
  | Some db ->
      let message_ids = List.map (fun (message : message) -> message.id) messages in
      let placeholders = List.map (fun _ -> "?") message_ids |> String.concat "," in
      with_statement db
        ("SELECT a.record_id,b.id,b.key,b.filename,b.content_type,b.byte_size "
        ^ "FROM active_storage_attachments a JOIN active_storage_blobs b ON b.id=a.blob_id "
        ^ "WHERE a.record_type='Message' AND a.name='attachment' AND a.record_id IN ("
        ^ placeholders ^ ")")
        (fun statement ->
          List.iteri
            (fun index id ->
              check_rc db "bind attached message id"
                (Sqlite3.bind_int statement (index + 1) id))
            message_ids;
          let rec collect attachments =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW ->
                collect
                  ((Sqlite3.column_int statement 0,
                    { message_id = Sqlite3.column_int statement 0;
                      blob_id = Sqlite3.column_int statement 1;
                      key = Sqlite3.column_text statement 2;
                      filename = Sqlite3.column_text statement 3;
                      content_type = Sqlite3.column_text statement 4;
                      byte_size = Sqlite3.column_int statement 5 })
                   :: attachments)
            | Sqlite3.Rc.DONE -> List.rev attachments
            | error ->
                failwith
                  (Printf.sprintf "message attachment lookup failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
          in
          collect [])

let find_stored_blob database blob_id =
  Option.bind database (fun db ->
      with_statement db
        "SELECT id,key,filename,COALESCE(content_type,'application/octet-stream'),byte_size FROM active_storage_blobs WHERE id=? LIMIT 1"
        (fun statement ->
          check_rc db "bind stored blob id" (Sqlite3.bind_int statement 1 blob_id);
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW ->
              Some
                { id = Sqlite3.column_int statement 0;
                  key = Sqlite3.column_text statement 1;
                  filename = Sqlite3.column_text statement 2;
                  content_type = Sqlite3.column_text statement 3;
                  byte_size = Sqlite3.column_int statement 4 }
          | Sqlite3.Rc.DONE -> None
          | error ->
              failwith
                (Printf.sprintf "stored blob lookup failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))

let authorized_stored_blob database ~blob_id ~user_id =
  Option.fold ~none:false
    ~some:(fun db ->
      with_statement db
        "SELECT a.record_type,a.record_id,b.metadata FROM active_storage_blobs b LEFT JOIN active_storage_attachments a ON a.blob_id=b.id WHERE b.id=?"
        (fun statement ->
          check_rc db "bind authorized blob id" (Sqlite3.bind_int statement 1 blob_id);
          let rec collect has_attachment authorized metadata =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW ->
                let record_type = Sqlite3.column_text statement 0 in
                let record_id = Sqlite3.column_int statement 1 in
                let metadata = Sqlite3.column_text statement 2 in
                let allowed =
                  match record_type with
                  | "User" | "Account" -> true
                  | "Message" ->
                      exists db
                        (Printf.sprintf
                           "SELECT 1 FROM messages m JOIN memberships ms ON ms.room_id=m.room_id WHERE m.id=%d AND ms.user_id=%d LIMIT 1"
                           record_id user_id)
                  | "ActionText::RichText" ->
                      exists db
                        (Printf.sprintf
                           "SELECT 1 FROM action_text_rich_texts rt JOIN messages m ON m.id=rt.record_id JOIN memberships ms ON ms.room_id=m.room_id WHERE rt.id=%d AND rt.record_type='Message' AND ms.user_id=%d LIMIT 1"
                           record_id user_id)
                  | _ -> false
                in
                collect true (authorized || allowed) metadata
            | Sqlite3.Rc.DONE ->
                if has_attachment then authorized
                else
                  (try
                     match Yojson.Basic.from_string metadata with
                     | `Assoc fields ->
                         (match List.assoc_opt "campfire_upload_user_id" fields with
                         | None | Some `Null -> true
                         | Some (`Int owner) -> owner = user_id
                         | Some (`String owner) -> int_of_string_opt owner = Some user_id
                         | _ -> false)
                     | _ -> true
                   with _ -> true)
            | error ->
                failwith
                  (Printf.sprintf "stored blob authorization failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
          in
          collect false false "{}"))
    database

let messages_after_id database room_id message_id =
  Option.fold ~none:[]
    ~some:(fun db ->
      with_statement db
        "SELECT m.id,u.name,COALESCE(rt.body,''),m.created_at,m.creator_id,m.client_message_id FROM messages m JOIN users u ON u.id=m.creator_id LEFT JOIN action_text_rich_texts rt ON rt.record_type='Message' AND rt.record_id=m.id AND rt.name='body' WHERE m.room_id=? AND m.id>? ORDER BY m.id ASC LIMIT 100"
        (fun statement ->
          check_rc db "bind live-message room" (Sqlite3.bind_int statement 1 room_id);
          check_rc db "bind live-message cursor" (Sqlite3.bind_int statement 2 message_id);
          let rec collect messages =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW ->
                collect
                  ({ id = Sqlite3.column_int statement 0;
                     creator_name = Sqlite3.column_text statement 1;
                     body_html = Sqlite3.column_text statement 2;
                     created_at = Sqlite3.column_text statement 3;
                     creator_id = Sqlite3.column_int statement 4;
                     client_message_id = Sqlite3.column_text statement 5 }
                  :: messages)
            | Sqlite3.Rc.DONE -> List.rev messages
            | error ->
                failwith
                  (Printf.sprintf "live-message query failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
          in
          collect []))
    database

let has_messages_before database room_id message_id =
  Option.fold ~none:false
    ~some:(fun db ->
      with_statement db
        "SELECT EXISTS(SELECT 1 FROM messages p JOIN messages m ON m.room_id=p.room_id AND m.created_at<p.created_at WHERE p.room_id=? AND p.id=?)"
        (fun statement ->
          check_rc db "bind older-message room"
            (Sqlite3.bind_int statement 1 room_id);
          check_rc db "bind older-message pivot"
            (Sqlite3.bind_int statement 2 message_id);
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW -> Sqlite3.column_int statement 0 <> 0
          | _ -> false))
    database

let has_messages_after database room_id message_id =
  Option.fold ~none:false
    ~some:(fun db ->
      with_statement db
        "SELECT EXISTS(SELECT 1 FROM messages p JOIN messages m ON m.room_id=p.room_id AND m.created_at>p.created_at WHERE p.room_id=? AND p.id=?)"
        (fun statement ->
          check_rc db "bind newer-message room"
            (Sqlite3.bind_int statement 1 room_id);
          check_rc db "bind newer-message pivot"
            (Sqlite3.bind_int statement 2 message_id);
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW -> Sqlite3.column_int statement 0 <> 0
          | _ -> false))
    database

let create_message_with_id ?attachment_blob_id database ~room_id ~creator_id ~body
    ~client_message_id ~timestamp =
  Option.fold ~none:None ~some:(fun db ->
      let run sql =
        match Sqlite3.exec db sql with
        | Sqlite3.Rc.OK -> ()
        | error ->
            failwith
              (Printf.sprintf "message transaction failed (%s): %s"
                 (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
      in
      let escaped =
        let output = Buffer.create (String.length body) in
        String.iter
          (function
            | '&' -> Buffer.add_string output "&amp;"
            | '<' -> Buffer.add_string output "&lt;"
            | '>' -> Buffer.add_string output "&gt;"
            | '"' -> Buffer.add_string output "&quot;"
            | '\'' -> Buffer.add_string output "&#39;"
            | c -> Buffer.add_char output c)
          body;
        Buffer.contents output
      in
      let escaped = String.split_on_char '\n' escaped |> String.concat "</div><div>" in
      run "BEGIN IMMEDIATE";
      try
        if not (exists db
          (Printf.sprintf "SELECT 1 FROM memberships WHERE room_id=%d AND user_id=%d LIMIT 1"
             room_id creator_id))
        then failwith "room membership required";
        with_statement db
          "INSERT INTO messages(room_id,creator_id,client_message_id,created_at,updated_at) VALUES(?,?,?,?,?)"
          (fun statement ->
            List.iteri
              (fun index value ->
                check_rc db "bind new message"
                  (Sqlite3.bind_text statement (index + 1) value))
              [ string_of_int room_id; string_of_int creator_id; client_message_id;
                timestamp; timestamp ];
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "message insert failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        let message_id =
          with_statement db "SELECT last_insert_rowid()" (fun statement ->
              match Sqlite3.step statement with
              | Sqlite3.Rc.ROW -> Sqlite3.column_int statement 0
              | _ -> failwith "message insert returned no row")
        in
        with_statement db
          "INSERT INTO action_text_rich_texts(record_type,record_id,name,body,created_at,updated_at) VALUES('Message',?,'body',?,?,?)"
          (fun statement ->
            check_rc db "bind rich text message id"
              (Sqlite3.bind_int statement 1 message_id);
            List.iteri
              (fun index value ->
                check_rc db "bind rich text message"
                  (Sqlite3.bind_text statement (index + 2) value))
              [ "<div>" ^ escaped ^ "</div>"; timestamp; timestamp ];
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "rich text insert failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        Option.iter
          (fun blob_id ->
            with_statement db
              "INSERT INTO active_storage_attachments(name,record_type,record_id,blob_id,created_at) VALUES('attachment','Message',?,?,?)"
              (fun statement ->
                check_rc db "bind attached message id"
                  (Sqlite3.bind_int statement 1 message_id);
                check_rc db "bind attached blob id"
                  (Sqlite3.bind_int statement 2 blob_id);
                check_rc db "bind attachment timestamp"
                  (Sqlite3.bind_text statement 3 timestamp);
                match Sqlite3.step statement with
                | Sqlite3.Rc.DONE -> ()
                | error ->
                    failwith
                      (Printf.sprintf "message attachment insert failed (%s): %s"
                         (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
          attachment_blob_id;
        with_statement db
          "INSERT INTO message_search_index(rowid,body) VALUES(?,?)"
          (fun statement ->
            check_rc db "bind message search id"
              (Sqlite3.bind_int statement 1 message_id);
            check_rc db "bind message search body"
              (Sqlite3.bind_text statement 2 body);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "message search insert failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        (* Match Room#receive: only visible memberships which are not currently
           connected become unread, and a message never marks its author unread. *)
        with_statement db
          "UPDATE memberships SET unread_at=?,updated_at=? WHERE room_id=? AND user_id<>? AND COALESCE(involvement,'')<>'invisible' AND (connected_at IS NULL OR datetime(connected_at)<datetime(?,'-60 seconds'))"
          (fun statement ->
            List.iteri
              (fun index value ->
                check_rc db "bind unread membership update"
                  (Sqlite3.bind_text statement (index + 1) value))
              [ timestamp; timestamp; string_of_int room_id;
                string_of_int creator_id; timestamp ];
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "unread membership update failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        run "COMMIT";
        Some message_id
      with error ->
        (try run "ROLLBACK" with _ -> ());
        raise error)
    database

let membership_present database ~room_id ~user_id ~timestamp =
  Option.fold ~none:false ~some:(fun db ->
      with_statement db
        "UPDATE memberships SET connections=CASE WHEN connected_at IS NOT NULL AND datetime(connected_at)>=datetime(?,'-60 seconds') THEN connections+1 ELSE 1 END,connected_at=?,unread_at=NULL,updated_at=? WHERE room_id=? AND user_id=?"
        (fun statement ->
          List.iteri
            (fun index value ->
              check_rc db "bind membership presence"
                (Sqlite3.bind_text statement (index + 1) value))
            [ timestamp; timestamp; timestamp; string_of_int room_id;
              string_of_int user_id ];
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE -> Sqlite3.changes db = 1
          | error ->
              failwith
                (Printf.sprintf "membership presence failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
    database

let membership_absent database ~room_id ~user_id ~timestamp =
  Option.fold ~none:false ~some:(fun db ->
      with_statement db
        "UPDATE memberships SET connections=CASE WHEN connected_at IS NOT NULL AND datetime(connected_at)>=datetime(?,'-60 seconds') THEN MAX(connections-1,0) ELSE 0 END,connected_at=CASE WHEN connected_at IS NOT NULL AND datetime(connected_at)>=datetime(?,'-60 seconds') AND connections>1 THEN ? ELSE NULL END,updated_at=? WHERE room_id=? AND user_id=?"
        (fun statement ->
          List.iteri
            (fun index value ->
              check_rc db "bind membership absence"
                (Sqlite3.bind_text statement (index + 1) value))
            [ timestamp; timestamp; timestamp; timestamp;
              string_of_int room_id; string_of_int user_id ];
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE -> Sqlite3.changes db = 1
          | error ->
              failwith
                (Printf.sprintf "membership absence failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
    database

let membership_refresh database ~room_id ~user_id ~timestamp =
  Option.fold ~none:false ~some:(fun db ->
      with_statement db
        "UPDATE memberships SET connections=CASE WHEN connected_at IS NOT NULL AND datetime(connected_at)>=datetime(?,'-60 seconds') THEN connections ELSE 1 END,connected_at=?,updated_at=? WHERE room_id=? AND user_id=?"
        (fun statement ->
          List.iteri
            (fun index value ->
              check_rc db "bind membership refresh"
                (Sqlite3.bind_text statement (index + 1) value))
            [ timestamp; timestamp; timestamp; string_of_int room_id;
              string_of_int user_id ];
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE -> Sqlite3.changes db = 1
          | error ->
              failwith
                (Printf.sprintf "membership refresh failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
    database

let create_message database ~room_id ~creator_id ~body ~client_message_id ~timestamp =
  ignore
    (create_message_with_id database ~room_id ~creator_id ~body ~client_message_id
       ~timestamp)

let find_message database room_id message_id =
  Option.bind database (fun db ->
      with_statement db
        "SELECT m.id,m.creator_id,u.name,COALESCE(rt.body,''),m.created_at,m.client_message_id FROM messages m JOIN users u ON u.id=m.creator_id LEFT JOIN action_text_rich_texts rt ON rt.record_type='Message' AND rt.record_id=m.id AND rt.name='body' WHERE m.room_id=? AND m.id=? LIMIT 1"
        (fun statement ->
          check_rc db "bind message room" (Sqlite3.bind_int statement 1 room_id);
          check_rc db "bind message id" (Sqlite3.bind_int statement 2 message_id);
          match Sqlite3.step statement with
          | Sqlite3.Rc.ROW ->
              Some
                { id = Sqlite3.column_int statement 0;
                  creator_id = Sqlite3.column_int statement 1;
                  creator_name = Sqlite3.column_text statement 2;
                  body_html = Sqlite3.column_text statement 3;
                  created_at = Sqlite3.column_text statement 4;
                  client_message_id = Sqlite3.column_text statement 5 }
          | Sqlite3.Rc.DONE -> None
          | error ->
              failwith
                (Printf.sprintf "message lookup failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))

let authorize_message db ~room_id ~message_id ~user_id ~role =
  with_statement db
    "SELECT m.creator_id FROM messages m JOIN memberships ms ON ms.room_id=m.room_id AND ms.user_id=? WHERE m.room_id=? AND m.id=? LIMIT 1"
    (fun statement ->
      check_rc db "bind message authorization user"
        (Sqlite3.bind_int statement 1 user_id);
      check_rc db "bind message authorization room"
        (Sqlite3.bind_int statement 2 room_id);
      check_rc db "bind message authorization id"
        (Sqlite3.bind_int statement 3 message_id);
      match Sqlite3.step statement with
      | Sqlite3.Rc.DONE -> raise Message_not_found
      | Sqlite3.Rc.ROW ->
          let creator_id = Sqlite3.column_int statement 0 in
          if creator_id <> user_id && role <> 1 then
            raise Message_not_authorized
          else creator_id
      | error ->
          failwith
            (Printf.sprintf "message authorization failed (%s): %s"
               (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)))

let message_has_attachments db message_id =
  exists db
    "SELECT 1 FROM sqlite_master WHERE type='table' AND name='active_storage_attachments'"
  && exists db
       (Printf.sprintf
          "SELECT 1 FROM active_storage_attachments a LEFT JOIN action_text_rich_texts rt ON rt.id=a.record_id AND rt.record_type='Message' WHERE (a.record_type='Message' AND a.record_id=%d AND a.name='attachment') OR (a.record_type='ActionText::RichText' AND rt.record_id=%d AND rt.name='body') LIMIT 1"
          message_id message_id)

let action_text_body body =
  let escaped = Buffer.create (String.length body) in
  String.iter
    (function
      | '&' -> Buffer.add_string escaped "&amp;"
      | '<' -> Buffer.add_string escaped "&lt;"
      | '>' -> Buffer.add_string escaped "&gt;"
      | '"' -> Buffer.add_string escaped "&quot;"
      | '\'' -> Buffer.add_string escaped "&#39;"
      | character -> Buffer.add_char escaped character)
    body;
  "<div>"
  ^ (Buffer.contents escaped |> String.split_on_char '\n'
    |> String.concat "</div><div>")
  ^ "</div>"

let update_message database ~room_id ~message_id ~user_id ~role ~body ~timestamp =
  Option.iter
    (fun db ->
      let run sql =
        match Sqlite3.exec db sql with
        | Sqlite3.Rc.OK -> ()
        | error ->
            failwith
              (Printf.sprintf "message update failed (%s): %s"
                 (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
      in
      run "BEGIN IMMEDIATE";
      try
        ignore (authorize_message db ~room_id ~message_id ~user_id ~role);
        if message_has_attachments db message_id then raise Message_has_attachments;
        let rich_body = action_text_body body in
        with_statement db
          "UPDATE messages SET updated_at=? WHERE id=? AND room_id=?"
          (fun statement ->
            check_rc db "bind message update timestamp"
              (Sqlite3.bind_text statement 1 timestamp);
            check_rc db "bind message update id"
              (Sqlite3.bind_int statement 2 message_id);
            check_rc db "bind message update room"
              (Sqlite3.bind_int statement 3 room_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "message update failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        with_statement db
          "INSERT INTO action_text_rich_texts(record_type,record_id,name,body,created_at,updated_at) VALUES('Message',?,'body',?,?,?) ON CONFLICT(record_type,record_id,name) DO UPDATE SET body=excluded.body,updated_at=excluded.updated_at"
          (fun statement ->
            check_rc db "bind updated rich-text message id"
              (Sqlite3.bind_int statement 1 message_id);
            check_rc db "bind updated rich-text body"
              (Sqlite3.bind_text statement 2 rich_body);
            check_rc db "bind updated rich-text creation time"
              (Sqlite3.bind_text statement 3 timestamp);
            check_rc db "bind updated rich-text update time"
              (Sqlite3.bind_text statement 4 timestamp);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "rich-text update failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        with_statement db "DELETE FROM message_search_index WHERE rowid=?"
          (fun statement ->
            check_rc db "bind message FTS delete"
              (Sqlite3.bind_int statement 1 message_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "message FTS delete failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        with_statement db
          "INSERT INTO message_search_index(rowid,body) VALUES(?,?)"
          (fun statement ->
            check_rc db "bind updated message FTS id"
              (Sqlite3.bind_int statement 1 message_id);
            check_rc db "bind updated message FTS body"
              (Sqlite3.bind_text statement 2 body);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "message FTS insert failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        with_statement db "UPDATE rooms SET updated_at=? WHERE id=?"
          (fun statement ->
            check_rc db "bind touched room timestamp"
              (Sqlite3.bind_text statement 1 timestamp);
            check_rc db "bind touched room id" (Sqlite3.bind_int statement 2 room_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "room touch failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        run "COMMIT"
      with error ->
        (try run "ROLLBACK" with _ -> ());
        raise error)
    database

let delete_message database ~room_id ~message_id ~user_id ~role =
  Option.iter
    (fun db ->
      let run sql =
        match Sqlite3.exec db sql with
        | Sqlite3.Rc.OK -> ()
        | error ->
            failwith
              (Printf.sprintf "message deletion failed (%s): %s"
                 (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
      in
      run "BEGIN IMMEDIATE";
      try
        ignore (authorize_message db ~room_id ~message_id ~user_id ~role);
        if message_has_attachments db message_id then raise Message_has_attachments;
        if exists db
             "SELECT 1 FROM sqlite_master WHERE type='table' AND name='boosts'"
        then
          with_statement db "DELETE FROM boosts WHERE message_id=?"
            (fun statement ->
              check_rc db "bind deleted-message boosts"
                (Sqlite3.bind_int statement 1 message_id);
              match Sqlite3.step statement with
              | Sqlite3.Rc.DONE -> ()
              | error ->
                  failwith
                    (Printf.sprintf "boost deletion failed (%s): %s"
                       (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        with_statement db "DELETE FROM message_search_index WHERE rowid=?"
          (fun statement ->
            check_rc db "bind deleted-message FTS id"
              (Sqlite3.bind_int statement 1 message_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "message FTS delete failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        with_statement db
          "DELETE FROM action_text_rich_texts WHERE record_type='Message' AND record_id=?"
          (fun statement ->
            check_rc db "bind deleted-message rich text"
              (Sqlite3.bind_int statement 1 message_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "rich-text deletion failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        with_statement db "DELETE FROM messages WHERE id=? AND room_id=?"
          (fun statement ->
            check_rc db "bind deleted message id"
              (Sqlite3.bind_int statement 1 message_id);
            check_rc db "bind deleted message room"
              (Sqlite3.bind_int statement 2 room_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "message deletion failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        run "COMMIT"
      with error ->
        (try run "ROLLBACK" with _ -> ());
        raise error)
    database

let delete_room database ~room_id ~user_id ~role =
  Option.fold ~none:false
    ~some:(fun db ->
      let run sql =
        match Sqlite3.exec db sql with
        | Sqlite3.Rc.OK -> ()
        | error ->
            failwith
              (Printf.sprintf "room deletion failed (%s): %s"
                 (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
      in
      run "BEGIN IMMEDIATE";
      try
        let room_kind, creator_id =
          with_statement db
            "SELECT r.type,r.creator_id FROM rooms r JOIN memberships m ON m.room_id=r.id AND m.user_id=? WHERE r.id=? LIMIT 1"
            (fun statement ->
              check_rc db "bind room deletion user"
                (Sqlite3.bind_int statement 1 user_id);
              check_rc db "bind room deletion id"
                (Sqlite3.bind_int statement 2 room_id);
              match Sqlite3.step statement with
              | Sqlite3.Rc.ROW ->
                  (Sqlite3.column_text statement 0, Sqlite3.column_int statement 1)
              | Sqlite3.Rc.DONE -> raise Room_not_found
              | error ->
                  failwith
                    (Printf.sprintf "room deletion lookup failed (%s): %s"
                       (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)))
        in
        if room_kind <> "Rooms::Direct" && role <> 1 && creator_id <> user_id then
          raise Room_not_authorized;
        let attachments =
          with_statement db "SELECT id FROM messages WHERE room_id=?"
            (fun statement ->
              check_rc db "bind room message attachments"
                (Sqlite3.bind_int statement 1 room_id);
              let rec find () =
                match Sqlite3.step statement with
                | Sqlite3.Rc.ROW ->
                    if message_has_attachments db (Sqlite3.column_int statement 0)
                    then true else find ()
                | Sqlite3.Rc.DONE -> false
                | error ->
                    failwith
                      (Printf.sprintf "room messages lookup failed (%s): %s"
                         (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
              in
              find ())
        in
        if attachments then raise Room_has_attachments;
        if exists db
             "SELECT 1 FROM sqlite_master WHERE type='table' AND name='boosts'"
        then
          with_statement db
            "DELETE FROM boosts WHERE message_id IN (SELECT id FROM messages WHERE room_id=?)"
            (fun statement ->
              check_rc db "bind room boosts" (Sqlite3.bind_int statement 1 room_id);
              match Sqlite3.step statement with
              | Sqlite3.Rc.DONE -> ()
              | error ->
                  failwith
                    (Printf.sprintf "room boosts deletion failed (%s): %s"
                       (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        with_statement db
          "DELETE FROM message_search_index WHERE rowid IN (SELECT id FROM messages WHERE room_id=?)"
          (fun statement ->
            check_rc db "bind room search index" (Sqlite3.bind_int statement 1 room_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "room search-index deletion failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        with_statement db
          "DELETE FROM action_text_rich_texts WHERE record_type='Message' AND record_id IN (SELECT id FROM messages WHERE room_id=?)"
          (fun statement ->
            check_rc db "bind room rich text" (Sqlite3.bind_int statement 1 room_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "room rich-text deletion failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        with_statement db "DELETE FROM messages WHERE room_id=?"
          (fun statement ->
            check_rc db "bind room messages" (Sqlite3.bind_int statement 1 room_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "room messages deletion failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        with_statement db "DELETE FROM memberships WHERE room_id=?"
          (fun statement ->
            check_rc db "bind room memberships" (Sqlite3.bind_int statement 1 room_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "room membership deletion failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        with_statement db "DELETE FROM rooms WHERE id=?"
          (fun statement ->
            check_rc db "bind deleted room" (Sqlite3.bind_int statement 1 room_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error ->
                failwith
                  (Printf.sprintf "room row deletion failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
        run "COMMIT";
        true
      with error ->
        (try run "ROLLBACK" with _ -> ());
        raise error)
    database

let search_messages database user_id fts_query =
  Option.fold ~none:[]
    ~some:(fun db ->
      with_statement db
        "SELECT m.id,u.name,COALESCE(rt.body,''),m.created_at,r.name,m.room_id,m.creator_id,m.client_message_id FROM messages m JOIN rooms r ON r.id=m.room_id JOIN users u ON u.id=m.creator_id JOIN memberships ms ON ms.room_id=m.room_id AND ms.user_id=? JOIN message_search_index idx ON idx.rowid=m.id LEFT JOIN action_text_rich_texts rt ON rt.record_type='Message' AND rt.record_id=m.id AND rt.name='body' WHERE idx.body MATCH ? ORDER BY m.created_at ASC,m.id ASC LIMIT 100"
        (fun statement ->
          check_rc db "bind search membership user"
            (Sqlite3.bind_int statement 1 user_id);
          check_rc db "bind message search query"
            (Sqlite3.bind_text statement 2 fts_query);
          let rec collect results =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW ->
                let message =
                  { id = Sqlite3.column_int statement 0;
                    creator_id = Sqlite3.column_int statement 6;
                    client_message_id = Sqlite3.column_text statement 7;
                    creator_name = Sqlite3.column_text statement 1;
                    body_html = Sqlite3.column_text statement 2;
                    created_at = Sqlite3.column_text statement 3 }
                in
                collect
                  ({ message; room_name = Sqlite3.column_text statement 4;
                     room_id = Sqlite3.column_int statement 5 }
                  :: results)
            | Sqlite3.Rc.DONE -> List.rev results
            | error ->
                failwith
                  (Printf.sprintf "message search failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
          in
          collect []))
    database

let recent_searches database user_id =
  Option.fold ~none:[]
    ~some:(fun db ->
      with_statement db
        "SELECT query FROM searches WHERE user_id=? ORDER BY updated_at DESC,id DESC LIMIT 10"
        (fun statement ->
          check_rc db "bind recent-search user"
            (Sqlite3.bind_int statement 1 user_id);
          let rec collect queries =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW -> collect (Sqlite3.column_text statement 0 :: queries)
            | Sqlite3.Rc.DONE -> List.rev queries
            | error ->
                failwith
                  (Printf.sprintf "recent-search lookup failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
          in
          collect []))
    database

let record_search database user_id query timestamp =
  Option.iter
    (fun db ->
      with_statement db "BEGIN IMMEDIATE" (fun statement ->
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE -> ()
          | error -> check_rc db "begin search transaction" error);
      try
        let existing_id =
          with_statement db "SELECT id FROM searches WHERE user_id=? AND query=? LIMIT 1"
            (fun statement ->
              check_rc db "bind search owner" (Sqlite3.bind_int statement 1 user_id);
              check_rc db "bind search text" (Sqlite3.bind_text statement 2 query);
              match Sqlite3.step statement with
              | Sqlite3.Rc.ROW -> Some (Sqlite3.column_int statement 0)
              | Sqlite3.Rc.DONE -> None
              | error ->
                  failwith
                    (Printf.sprintf "search lookup failed (%s): %s"
                       (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)))
        in
        (match existing_id with
        | Some id ->
            with_statement db "UPDATE searches SET updated_at=? WHERE id=? AND user_id=?"
              (fun statement ->
                check_rc db "bind search timestamp" (Sqlite3.bind_text statement 1 timestamp);
                check_rc db "bind search id" (Sqlite3.bind_int statement 2 id);
                check_rc db "bind search owner" (Sqlite3.bind_int statement 3 user_id);
                match Sqlite3.step statement with
                | Sqlite3.Rc.DONE -> ()
                | error -> check_rc db "update search" error)
        | None ->
            with_statement db
              "INSERT INTO searches(user_id,query,created_at,updated_at) VALUES(?,?,?,?)"
              (fun statement ->
                check_rc db "bind search owner" (Sqlite3.bind_int statement 1 user_id);
                List.iteri
                  (fun index value ->
                    check_rc db "bind new search"
                      (Sqlite3.bind_text statement (index + 2) value))
                  [ query; timestamp; timestamp ];
                match Sqlite3.step statement with
                | Sqlite3.Rc.DONE -> ()
                | error -> check_rc db "insert search" error));
        with_statement db
          "DELETE FROM searches WHERE user_id=? AND id NOT IN (SELECT id FROM searches WHERE user_id=? ORDER BY updated_at DESC,id DESC LIMIT 10)"
          (fun statement ->
            check_rc db "bind search cleanup owner" (Sqlite3.bind_int statement 1 user_id);
            check_rc db "bind search cleanup query" (Sqlite3.bind_int statement 2 user_id);
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error -> check_rc db "cleanup searches" error);
        with_statement db "COMMIT" (fun statement ->
            match Sqlite3.step statement with
            | Sqlite3.Rc.DONE -> ()
            | error -> check_rc db "commit search transaction" error)
      with error ->
        (try ignore (Sqlite3.exec db "ROLLBACK") with _ -> ());
        raise error)
    database

let clear_searches database user_id =
  Option.iter
    (fun db ->
      with_statement db "DELETE FROM searches WHERE user_id=?" (fun statement ->
          check_rc db "bind clear-search owner" (Sqlite3.bind_int statement 1 user_id);
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE -> ()
          | error -> check_rc db "clear searches" error))
    database

let delete_session database session_id =
  Option.iter
    (fun db ->
      with_statement db "DELETE FROM sessions WHERE id=?" (fun statement ->
          check_rc db "bind session id" (Sqlite3.bind_int statement 1 session_id);
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE -> ()
          | error ->
              failwith
                (Printf.sprintf "session delete failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))))
    database

let open_existing storage_root =
  let filename = path storage_root in
  if not (Sys.file_exists filename) then None
  else
    let db = Sqlite3.db_open ~mode:`NO_CREATE ~mutex:`FULL filename in
    match Sqlite3.exec db "PRAGMA busy_timeout = 10000; PRAGMA foreign_keys = ON" with
    | Sqlite3.Rc.OK -> Some db
    | error ->
        let message = Sqlite3.errmsg db in
        ignore (Sqlite3.db_close db);
        failwith
          (Printf.sprintf "SQLite setup failed (%s): %s" (Sqlite3.Rc.to_string error) message)

let rec ensure_directory directory =
  if Sys.file_exists directory then (
    if not (Sys.is_directory directory) then
      failwith (directory ^ " exists but is not a directory"))
  else (
    let parent = Filename.dirname directory in
    if parent <> directory then ensure_directory parent;
    try Unix.mkdir directory 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let jobs_path storage_root =
  match Sys.getenv_opt "JOBS_DATABASE_PATH" with
  | Some filename when filename <> "" -> filename
  | None -> Filename.concat (Filename.concat storage_root "db") "jobs.sqlite3"
  | Some _ -> Filename.concat (Filename.concat storage_root "db") "jobs.sqlite3"

let open_jobs storage_root =
  let filename = jobs_path storage_root in
  ensure_directory (Filename.dirname filename);
  let db = Sqlite3.db_open ~mutex:`FULL filename in
  let schema =
    "PRAGMA busy_timeout=10000; PRAGMA journal_mode=WAL; "
    ^ "CREATE TABLE IF NOT EXISTS jobs(id INTEGER PRIMARY KEY,payload TEXT NOT NULL,attempts INTEGER NOT NULL DEFAULT 0,available_at REAL NOT NULL,lease_until REAL,lease_token TEXT,status TEXT NOT NULL DEFAULT 'ready',last_error TEXT); "
    ^ "CREATE TABLE IF NOT EXISTS login_limits(ip TEXT PRIMARY KEY,attempts INTEGER NOT NULL,expires_at INTEGER NOT NULL)"
  in
  match Sqlite3.exec db schema with
  | Sqlite3.Rc.OK -> db
  | error ->
      let message = Sqlite3.errmsg db in
      ignore (Sqlite3.db_close db);
      failwith
        (Printf.sprintf "Auxiliary SQLite setup failed (%s): %s"
           (Sqlite3.Rc.to_string error) message)

let allow_login db ip ~at_ms =
  with_statement db
    "INSERT INTO login_limits(ip,attempts,expires_at) VALUES(?,1,?) ON CONFLICT(ip) DO UPDATE SET attempts=CASE WHEN expires_at<=? THEN 1 ELSE attempts+1 END,expires_at=CASE WHEN expires_at<=? THEN excluded.expires_at ELSE expires_at END RETURNING attempts"
    (fun statement ->
      let expires = Int64.add at_ms 180_000L in
      check_rc db "bind login limit ip" (Sqlite3.bind_text statement 1 ip);
      check_rc db "bind login limit expiry" (Sqlite3.bind_int64 statement 2 expires);
      check_rc db "bind login limit current time"
        (Sqlite3.bind_int64 statement 3 at_ms);
      check_rc db "bind login limit reset time"
        (Sqlite3.bind_int64 statement 4 at_ms);
      let attempts =
        match Sqlite3.step statement with
        | Sqlite3.Rc.ROW -> Sqlite3.column_int statement 0
        | error ->
            failwith
              (Printf.sprintf "login limit failed (%s): %s"
                 (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
      in
      (match Sqlite3.step statement with
      | Sqlite3.Rc.DONE -> ()
      | error ->
          failwith
            (Printf.sprintf "login limit completion failed (%s): %s"
               (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
      with_statement db "DELETE FROM login_limits WHERE expires_at<=?"
        (fun cleanup ->
          check_rc db "bind expired login limit"
            (Sqlite3.bind_int64 cleanup 1 at_ms);
          match Sqlite3.step cleanup with
          | Sqlite3.Rc.DONE -> ()
          | error ->
              failwith
                (Printf.sprintf "login limit cleanup failed (%s): %s"
                   (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db)));
      attempts <= 10)
