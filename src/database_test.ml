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
      exec db
        "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT NOT NULL, email_address TEXT, password_digest TEXT, role INTEGER DEFAULT 0, status INTEGER DEFAULT 0)";
      exec db
        "CREATE TABLE sessions (id INTEGER PRIMARY KEY, created_at TEXT NOT NULL, ip_address TEXT, last_active_at TEXT NOT NULL, token TEXT NOT NULL, updated_at TEXT NOT NULL, user_agent TEXT, user_id INTEGER NOT NULL)";
      exec db
        "CREATE TABLE login_limits (ip TEXT PRIMARY KEY, attempts INTEGER NOT NULL, expires_at INTEGER NOT NULL)";
      check "empty accounts" false (Database.account_exists (Some db));
      check "empty users" false (Database.user_exists (Some db));
      exec db "INSERT INTO accounts DEFAULT VALUES";
      check "existing account" true (Database.account_exists (Some db));
      check "no users" false (Database.user_exists (Some db));
      exec db
        "INSERT INTO users(id,name,email_address,password_digest,role,status) VALUES(1,'David','david@example.com','$2a$04$abcdefghijklmnopqrstuu0//9Hm2kfER85NNo0cY8vC7Rsrlx0Yi',0,0)";
      check "existing user" true (Database.user_exists (Some db));
      check "active user lookup" (Some "David")
        (Database.find_active_user (Some db) "david@example.com"
        |> Option.map (fun user -> user.Database.name));
      check "email lookup is exact" None
        (Database.find_active_user (Some db) "DAVID@example.com");
      Database.create_session (Some db) ~user_id:1 ~token:"session-token"
        ~user_agent:"test-agent" ~ip_address:"127.0.0.1"
        ~timestamp:"2026-10-07 12:00:00.000";
      check "session lookup" (Some "David")
        (Database.find_session_identity (Some db) "session-token"
        |> Option.map (fun identity -> identity.Database.user.name));
      check "revoked user cannot authenticate" None
        (let _ = exec db "UPDATE users SET status=1 WHERE id=1" in
         Database.find_session_identity (Some db) "session-token");
      Database.delete_session (Some db) 1;
      check "deleted session cannot authenticate" None
        (Database.find_session_identity (Some db) "session-token");
      for _attempt = 1 to 10 do
        check "first ten login attempts are allowed" true
          (Database.allow_login db "127.0.0.1" ~at_ms:1_000_000L)
      done;
      check "eleventh login attempt is blocked" false
        (Database.allow_login db "127.0.0.1" ~at_ms:1_000_000L);
      check "login limits are per address" true
        (Database.allow_login db "127.0.0.2" ~at_ms:1_000_000L);
      check "login limit resets after three minutes" true
        (Database.allow_login db "127.0.0.1" ~at_ms:1_180_000L))
