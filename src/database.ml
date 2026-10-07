type t = Sqlite3.db

type user = {
  id : int;
  name : string;
  password_digest : string option;
  role : int;
}

type identity = { session_id : int; user : user }
type room = { id : int; name : string; kind : string }
type user_option = { id : int; name : string }
type message = { id : int; creator_name : string; body_html : string; created_at : string }
type search_result = { message : message; room_id : int; room_name : string }

exception Message_not_found
exception Invalid_join_code
exception Duplicate_email

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
        "SELECT r.id,COALESCE(r.name,''),r.type FROM rooms r JOIN memberships m ON m.room_id=r.id WHERE m.user_id=? ORDER BY r.created_at,r.id"
        (fun statement ->
          check_rc db "bind room membership user"
            (Sqlite3.bind_int statement 1 user_id);
          let rec collect rooms =
            match Sqlite3.step statement with
            | Sqlite3.Rc.ROW ->
                collect
                  ({ id = Sqlite3.column_int statement 0;
                     name = Sqlite3.column_text statement 1;
                     kind = Sqlite3.column_text statement 2 }
                  :: rooms)
            | Sqlite3.Rc.DONE -> List.rev rooms
            | error ->
                failwith
                  (Printf.sprintf "room lookup failed (%s): %s"
                     (Sqlite3.Rc.to_string error) (Sqlite3.errmsg db))
          in
          collect []))
    database

let find_room_for_user database user_id room_id =
  rooms_for_user database user_id
  |> List.find_opt (fun (room : room) -> room.id = room_id)

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

let messages_for_room ?before ?after ?around database room_id =
  Option.fold ~none:[]
    ~some:(fun db ->
      let select =
        "SELECT m.id,u.name,COALESCE(rt.body,''),m.created_at FROM messages m JOIN users u ON u.id=m.creator_id LEFT JOIN action_text_rich_texts rt ON rt.record_type='Message' AND rt.record_id=m.id AND rt.name='body' WHERE m.room_id=?"
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

let create_message database ~room_id ~creator_id ~body ~client_message_id ~timestamp =
  Option.iter
    (fun db ->
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
        run "COMMIT"
      with error ->
        (try run "ROLLBACK" with _ -> ());
        raise error)
    database

let search_messages database user_id fts_query =
  Option.fold ~none:[]
    ~some:(fun db ->
      with_statement db
        "SELECT m.id,u.name,COALESCE(rt.body,''),m.created_at,r.name,m.room_id FROM messages m JOIN rooms r ON r.id=m.room_id JOIN users u ON u.id=m.creator_id JOIN memberships ms ON ms.room_id=m.room_id AND ms.user_id=? JOIN message_search_index idx ON idx.rowid=m.id LEFT JOIN action_text_rich_texts rt ON rt.record_type='Message' AND rt.record_id=m.id AND rt.name='body' WHERE idx.body MATCH ? ORDER BY m.created_at DESC,m.id DESC LIMIT 100"
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
