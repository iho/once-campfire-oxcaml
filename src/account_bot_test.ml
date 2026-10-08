let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

let exec db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | error -> failwith (Sqlite3.Rc.to_string error ^ ": " ^ Sqlite3.errmsg db)

let () =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () ->
      exec db "CREATE TABLE users(id INTEGER PRIMARY KEY,name TEXT NOT NULL,email_address TEXT,password_digest TEXT,bio TEXT,role INTEGER NOT NULL DEFAULT 0,status INTEGER NOT NULL DEFAULT 0,created_at TEXT NOT NULL,updated_at TEXT NOT NULL,bot_token TEXT)";
      exec db "CREATE TABLE rooms(id INTEGER PRIMARY KEY,name TEXT,type TEXT NOT NULL,creator_id INTEGER NOT NULL,created_at TEXT NOT NULL,updated_at TEXT NOT NULL)";
      exec db "CREATE TABLE memberships(id INTEGER PRIMARY KEY,room_id INTEGER NOT NULL,user_id INTEGER NOT NULL,created_at TEXT NOT NULL,updated_at TEXT NOT NULL,UNIQUE(room_id,user_id))";
      exec db "CREATE TABLE webhooks(id INTEGER PRIMARY KEY,user_id INTEGER NOT NULL,url TEXT,created_at TEXT NOT NULL,updated_at TEXT NOT NULL)";
      exec db "CREATE TABLE sessions(id INTEGER PRIMARY KEY,user_id INTEGER NOT NULL)";
      exec db "CREATE TABLE searches(id INTEGER PRIMARY KEY,user_id INTEGER NOT NULL)";
      exec db "CREATE TABLE push_subscriptions(id INTEGER PRIMARY KEY,user_id INTEGER NOT NULL)";
      exec db "INSERT INTO rooms(id,name,type,creator_id,created_at,updated_at) VALUES(1,'Lobby','Rooms::Open',1,'now','now'),(2,'Pair','Rooms::Direct',1,'now','now')";
      let bot_id = Database.create_bot (Some db) ~name:"Notifier"
          ~webhook_url:(Some "https://hooks.example.test/campfire") ~timestamp:"now" |> Option.get in
      let bot = Database.account_bots (Some db) |> List.find (fun (b : Database.account_bot) -> b.id = bot_id) in
      check "bot token format" true (String.length bot.token = 12 && String.for_all (function '0'..'9' | 'A'..'Z' | 'a'..'z' -> true | _ -> false) bot.token);
      check "bot key authenticates" (Some bot_id) (Database.authenticate_bot (Some db) (string_of_int bot_id ^ "-" ^ bot.token) |> Option.map (fun (u : Database.user) -> u.id));
      check "new bot joins all open rooms" true (Database.find_room_for_user (Some db) bot_id 1 <> None);
      check "webhook persists" "https://hooks.example.test/campfire" bot.webhook_url;
      check "bot update succeeds" true (Database.update_bot (Some db) ~user_id:bot_id ~name:"Notifier v2" ~webhook_url:None ~timestamp:"later");
      check "bot update removes blank webhook" [ ("Notifier v2", "") ] (Database.account_bots (Some db) |> List.map (fun (b : Database.account_bot) -> b.name,b.webhook_url));
      exec db (Printf.sprintf "INSERT INTO memberships(room_id,user_id,created_at,updated_at) VALUES(2,%d,'now','now'); INSERT INTO sessions(user_id) VALUES(%d); INSERT INTO searches(user_id) VALUES(%d); INSERT INTO push_subscriptions(user_id) VALUES(%d)" bot_id bot_id bot_id bot_id);
      let rotated = Database.rotate_bot_key (Some db) ~user_id:bot_id ~timestamp:"later" |> Option.get in
      check "rotation rejects old key" None (Database.authenticate_bot (Some db) (string_of_int bot_id ^ "-" ^ bot.token));
      check "rotation accepts new key" (Some bot_id) (Database.authenticate_bot (Some db) (string_of_int bot_id ^ "-" ^ rotated) |> Option.map (fun (u : Database.user) -> u.id));
      check "bot deactivation succeeds" true (Database.deactivate_bot (Some db) ~user_id:bot_id ~timestamp:"later");
      check "deactivated bot no longer authenticates" None (Database.authenticate_bot (Some db) (string_of_int bot_id ^ "-" ^ rotated));
      check "deactivated bot is hidden" [] (Database.account_bots (Some db));
      check "deactivation removes shared membership and retains direct membership" [2] (Database.with_statement db "SELECT room_id FROM memberships WHERE user_id=? ORDER BY room_id" (fun s -> ignore (Sqlite3.bind_int s 1 bot_id); let rec loop acc = match Sqlite3.step s with Sqlite3.Rc.ROW -> loop (Sqlite3.column_int s 0 :: acc) | Sqlite3.Rc.DONE -> List.rev acc | e -> failwith (Sqlite3.Rc.to_string e) in loop []));
      check "deactivation clears sessions, searches, push subscriptions" [0;0;0] (List.map (fun table -> Database.with_statement db ("SELECT COUNT(*) FROM " ^ table ^ " WHERE user_id=?") (fun s -> ignore (Sqlite3.bind_int s 1 bot_id); ignore (Sqlite3.step s); Sqlite3.column_int s 0)) ["sessions";"searches";"push_subscriptions"]))
