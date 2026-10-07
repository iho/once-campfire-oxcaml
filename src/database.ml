type t = Sqlite3.db

type user = {
  id : int;
  name : string;
  password_digest : string option;
  role : int;
}

type identity = { session_id : int; user : user }

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
