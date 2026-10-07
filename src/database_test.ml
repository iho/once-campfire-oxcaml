let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

let exec db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | error -> failwith (Sqlite3.Rc.to_string error ^ ": " ^ Sqlite3.errmsg db)

let () =
  let bcrypt_hash =
    "$2a$04$abcdefghijklmnopqrstuu0//9Hm2kfER85NNo0cY8vC7Rsrlx0Yi"
  in
  check "bcrypt independent bcryptjs vector" true
    (Bcrypt.verify ~hash:bcrypt_hash "oxcaml-password");
  check "bcrypt 2b variant" true
    (Bcrypt.verify
       ~hash:"$2b$04$abcdefghijklmnopqrstuu0//9Hm2kfER85NNo0cY8vC7Rsrlx0Yi"
       "oxcaml-password");
  check "bcrypt 2y variant" true
    (Bcrypt.verify
       ~hash:"$2y$04$abcdefghijklmnopqrstuuyQbK3uYMNeDhpWD3ALRiFXKhkHYEr32"
       "variant-y-password");
  check "bcrypt rejects wrong password" false
    (Bcrypt.verify ~hash:bcrypt_hash "not the password");
  check "bcrypt rejects malformed hash" false (Bcrypt.verify ~hash:"bad" "password");
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
