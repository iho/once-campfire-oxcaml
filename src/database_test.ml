let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

let exec db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | error -> failwith (Sqlite3.Rc.to_string error ^ ": " ^ Sqlite3.errmsg db)

let () =
  check "missing database" false (Database.account_exists None);
  check "missing database user" false (Database.user_exists None);
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      exec db "CREATE TABLE accounts (id INTEGER PRIMARY KEY)";
      exec db "CREATE TABLE users (id INTEGER PRIMARY KEY)";
      check "empty accounts" false (Database.account_exists (Some db));
      check "empty users" false (Database.user_exists (Some db));
      exec db "INSERT INTO accounts DEFAULT VALUES";
      check "existing account" true (Database.account_exists (Some db));
      check "no users" false (Database.user_exists (Some db));
      exec db "INSERT INTO users DEFAULT VALUES";
      check "existing user" true (Database.user_exists (Some db)))
