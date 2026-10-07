type t = Sqlite3.db

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
