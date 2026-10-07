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
        |> Option.map (fun (user : Database.user) -> user.Database.name));
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
        (Database.allow_login db "127.0.0.1" ~at_ms:1_180_000L));
  let setup_db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close setup_db))
    (fun () ->
      exec setup_db
        "CREATE TABLE accounts(id INTEGER PRIMARY KEY,name TEXT NOT NULL,join_code TEXT NOT NULL,settings TEXT,created_at TEXT NOT NULL,updated_at TEXT NOT NULL,singleton_guard INTEGER NOT NULL DEFAULT 0 UNIQUE)";
      exec setup_db
        "CREATE TABLE users(id INTEGER PRIMARY KEY,name TEXT NOT NULL,email_address TEXT UNIQUE,password_digest TEXT,bio TEXT,role INTEGER NOT NULL DEFAULT 0,status INTEGER NOT NULL DEFAULT 0,created_at TEXT NOT NULL,updated_at TEXT NOT NULL)";
      exec setup_db
        "CREATE TABLE rooms(id INTEGER PRIMARY KEY,name TEXT,type TEXT NOT NULL,creator_id INTEGER NOT NULL,created_at TEXT NOT NULL,updated_at TEXT NOT NULL)";
      exec setup_db
        "CREATE TABLE memberships(id INTEGER PRIMARY KEY,room_id INTEGER NOT NULL,user_id INTEGER NOT NULL,created_at TEXT NOT NULL,updated_at TEXT NOT NULL,involvement TEXT DEFAULT 'mentions',connections INTEGER NOT NULL DEFAULT 0,unread_at TEXT,UNIQUE(room_id,user_id))";
      exec setup_db
        "CREATE TABLE messages(id INTEGER PRIMARY KEY,room_id INTEGER NOT NULL,creator_id INTEGER NOT NULL,client_message_id TEXT NOT NULL,created_at TEXT NOT NULL,updated_at TEXT NOT NULL)";
      exec setup_db
        "CREATE TABLE action_text_rich_texts(id INTEGER PRIMARY KEY,record_type TEXT NOT NULL,record_id INTEGER NOT NULL,name TEXT NOT NULL,body TEXT,created_at TEXT NOT NULL,updated_at TEXT NOT NULL,UNIQUE(record_type,record_id,name))";
      exec setup_db
        "CREATE TABLE boosts(id INTEGER PRIMARY KEY,message_id INTEGER NOT NULL,booster_id INTEGER NOT NULL,content TEXT NOT NULL,created_at TEXT NOT NULL,updated_at TEXT NOT NULL)";
      exec setup_db
        "CREATE TABLE active_storage_blobs(id INTEGER PRIMARY KEY,key TEXT NOT NULL,content_type TEXT)";
      exec setup_db
        "CREATE TABLE active_storage_attachments(id INTEGER PRIMARY KEY,record_type TEXT NOT NULL,record_id INTEGER NOT NULL,name TEXT NOT NULL,blob_id INTEGER NOT NULL)";
      exec setup_db "CREATE VIRTUAL TABLE message_search_index USING fts5(body)";
      exec setup_db
        "CREATE TABLE searches(id INTEGER PRIMARY KEY,user_id INTEGER NOT NULL,query TEXT NOT NULL,created_at TEXT NOT NULL,updated_at TEXT NOT NULL)";
      let password_digest = Bcrypt.hash "setup password" in
      check "generated bcrypt digest verifies" true
        (Bcrypt.verify ~hash:password_digest "setup password");
      let user_id =
        Database.create_first_run (Some setup_db) ~name:"Ada Lovelace"
          ~email_address:"ada@example.com" ~password_digest
          ~timestamp:"2026-10-07 12:00:00.000"
        |> Option.get
      in
      exec setup_db
        "INSERT INTO active_storage_blobs(id,key,content_type) VALUES(77,'abcdefgxyz','image/png')";
      exec setup_db
        (Printf.sprintf "INSERT INTO active_storage_attachments(record_type,record_id,name,blob_id) VALUES('User',%d,'avatar',77)" user_id);
      check "avatar lookup resolves the original Rails blob key and MIME type"
        (Some { Database.key = "abcdefgxyz"; content_type = "image/png" })
        (Database.find_avatar_blob (Some setup_db) user_id);
      exec setup_db
        "INSERT INTO users(id,name,email_address,password_digest,bio,role,status,created_at,updated_at) VALUES(99,'Inactive','duplicate@example.com',NULL,NULL,0,1,'2026-10-07','2026-10-07')";
      check "profile reads Rails profile fields" (Some ("Ada Lovelace", "ada@example.com", ""))
        (Database.find_profile (Some setup_db) user_id
        |> Option.map (fun (profile : Database.profile) ->
               (profile.Database.name, profile.Database.email_address, profile.Database.bio)));
      let replacement_digest = Bcrypt.hash "updated profile password" in
      check "profile update changes identity and bcrypt password" true
        (Database.update_profile (Some setup_db) ~user_id ~name:"Ada Byron"
           ~email_address:"ada.byron@example.com"
           ~password_digest:(Some replacement_digest) ~bio:"Mathematician"
           ~timestamp:"2026-10-07 12:00:15.000");
      check "updated profile fields are persisted"
        (Some ("Ada Byron", "ada.byron@example.com", "Mathematician"))
        (Database.find_profile (Some setup_db) user_id
        |> Option.map (fun (profile : Database.profile) ->
               (profile.Database.name, profile.Database.email_address, profile.Database.bio)));
      check "profile password update is bcrypt-compatible" true
        (Database.with_statement setup_db "SELECT password_digest FROM users WHERE id=?"
           (fun statement ->
             ignore (Sqlite3.bind_int statement 1 user_id);
             ignore (Sqlite3.step statement);
             Bcrypt.verify ~hash:(Sqlite3.column_text statement 0)
               "updated profile password"));
      check "profile update without password preserves password digest" true
        (Database.update_profile (Some setup_db) ~user_id ~name:"Ada Lovelace"
           ~email_address:"ada.byron@example.com" ~password_digest:None ~bio:""
           ~timestamp:"2026-10-07 12:00:16.000");
      check "duplicate profile email is rejected atomically" false
        (Database.update_profile (Some setup_db) ~user_id ~name:"Should not save"
           ~email_address:"duplicate@example.com" ~password_digest:None ~bio:"bad"
           ~timestamp:"2026-10-07 12:00:17.000");
      check "failed duplicate-email update keeps existing profile"
        (Some ("Ada Lovelace", "ada.byron@example.com", ""))
        (Database.find_profile (Some setup_db) user_id
        |> Option.map (fun (profile : Database.profile) ->
               (profile.Database.name, profile.Database.email_address, profile.Database.bio)));
      check "first-run user is administrator" 1
        (Database.with_statement setup_db "SELECT role FROM users WHERE id=?" (fun statement ->
             ignore (Sqlite3.bind_int statement 1 user_id);
             ignore (Sqlite3.step statement);
             Sqlite3.column_int statement 0));
      check "first-run creates Campfire account" "Campfire"
        (Database.with_statement setup_db "SELECT name FROM accounts" (fun statement ->
             ignore (Sqlite3.step statement);
             Sqlite3.column_text statement 0));
      check "first-run Rails join code format" true
        (Database.with_statement setup_db "SELECT join_code FROM accounts" (fun statement ->
             ignore (Sqlite3.step statement);
             let code = Sqlite3.column_text statement 0 in
             String.length code = 14 && code.[4] = '-' && code.[9] = '-'));
      check "first-run creates All Talk open room" "Rooms::Open:All Talk"
        (Database.with_statement setup_db "SELECT type||':'||name FROM rooms" (fun statement ->
             ignore (Sqlite3.step statement);
             Sqlite3.column_text statement 0));
      check "membership-scoped room list" [ ("All Talk", "Rooms::Open") ]
        (Database.rooms_for_user (Some setup_db) user_id
        |> List.map (fun (room : Database.room) -> (room.name, room.kind)));
      check "room lookup rejects non-member" None
        (Database.find_room_for_user (Some setup_db) (user_id + 1) 1);
      check "first-run membership defaults to mentions" (Some "mentions")
        (Database.membership_involvement (Some setup_db) ~room_id:1
           ~user_id);
      check "sidebar returns visible shared rooms" [ ("All Talk", false) ]
        (Database.sidebar_rooms (Some setup_db) user_id
        |> List.map (fun (entry : Database.sidebar_room) ->
               (entry.Database.room.Database.name, entry.Database.unread)));
      exec setup_db
        "UPDATE memberships SET unread_at='2026-10-07 12:00:20' WHERE room_id=1";
      check "sidebar exposes Rails unread state" [ ("All Talk", true) ]
        (Database.sidebar_rooms (Some setup_db) user_id
        |> List.map (fun (entry : Database.sidebar_room) ->
               (entry.Database.room.Database.name, entry.Database.unread)));
      check "sidebar excludes hidden memberships" true
        (Database.update_membership_involvement (Some setup_db) ~room_id:1
           ~user_id ~involvement:"invisible" ~timestamp:"2026-10-07 12:00:25.000"
        && Database.sidebar_rooms (Some setup_db) user_id = []);
      ignore
        (Database.update_membership_involvement (Some setup_db) ~room_id:1
           ~user_id ~involvement:"mentions" ~timestamp:"2026-10-07 12:00:26.000");
      check "membership notification preference can be updated" true
        (Database.update_membership_involvement (Some setup_db) ~room_id:1
           ~user_id ~involvement:"everything" ~timestamp:"2026-10-07 12:00:30.000");
      check "updated membership preference is stored" (Some "everything")
        (Database.membership_involvement (Some setup_db) ~room_id:1 ~user_id);
      check "invalid membership preference is rejected" false
        (Database.update_membership_involvement (Some setup_db) ~room_id:1
           ~user_id ~involvement:"invalid" ~timestamp:"2026-10-07 12:00:31.000");
      check "non-member preference update is rejected" false
        (Database.update_membership_involvement (Some setup_db) ~room_id:1
           ~user_id:(user_id + 1) ~involvement:"nothing"
           ~timestamp:"2026-10-07 12:00:32.000");
      Database.create_message (Some setup_db) ~room_id:1 ~creator_id:user_id
        ~body:"hello <script>&\nagain" ~client_message_id:"00000000-0000-4000-8000-000000000001"
        ~timestamp:"2026-10-07 12:01:00.000";
      check "message reads through Rails Action Text" 1
        (Database.messages_for_room (Some setup_db) 1 |> List.length);
      check "message text is escaped in Rails Action Text storage"
        "<div>hello &lt;script&gt;&amp;</div><div>again</div>"
        (Database.messages_for_room (Some setup_db) 1
        |> List.hd |> fun (message : Database.message) -> message.body_html);
      check "message is indexed for Rails search" 1
        (Database.with_statement setup_db
           "SELECT count(*) FROM message_search_index WHERE message_search_index MATCH 'hello'"
           (fun statement ->
             ignore (Sqlite3.step statement);
             Sqlite3.column_int statement 0));
      check "search finds only messages in the user's rooms" [ ("All Talk", "Ada Lovelace") ]
        (Database.search_messages (Some setup_db) user_id "\"hello\""
        |> List.map (fun (result : Database.search_result) ->
               (result.Database.room_name, result.Database.message.Database.creator_name)));
      check "search excludes rooms without membership" []
        (Database.search_messages (Some setup_db) (user_id + 1) "\"hello\"");
      Database.record_search (Some setup_db) user_id "hello campfire"
        "2026-10-07 12:02:00.000";
      Database.record_search (Some setup_db) user_id "hello campfire"
        "2026-10-07 12:03:00.000";
      check "recent search is updated rather than duplicated" [ "hello campfire" ]
        (Database.recent_searches (Some setup_db) user_id);
      for index = 1 to 11 do
        Database.record_search (Some setup_db) user_id
          (Printf.sprintf "recent-%02d" index)
          (Printf.sprintf "2026-10-07 12:%02d:00.000" (index + 3))
      done;
      let recents = Database.recent_searches (Some setup_db) user_id in
      check "recent search list is capped at ten" 10 (List.length recents);
      check "recent searches are newest first" "recent-11" (List.hd recents);
      Database.record_search (Some setup_db) (user_id + 1) "private history"
        "2026-10-07 12:20:00.000";
      Database.clear_searches (Some setup_db) user_id;
      check "clear removes only that user's searches" []
        (Database.recent_searches (Some setup_db) user_id);
      check "clearing history preserves other users" [ "private history" ]
        (Database.recent_searches (Some setup_db) (user_id + 1));
      (try
         Database.create_message (Some setup_db) ~room_id:1
           ~creator_id:(user_id + 1) ~body:"forbidden"
           ~client_message_id:"00000000-0000-4000-8000-000000000002"
           ~timestamp:"2026-10-07 12:02:00.000";
         failwith "non-member message unexpectedly succeeded"
       with Failure message when message = "room membership required" -> ());
      check "unauthorized message leaves no row" 1
        (Database.with_statement setup_db "SELECT count(*) FROM messages" (fun statement ->
             ignore (Sqlite3.step statement);
             Sqlite3.column_int statement 0));
      exec setup_db
        "INSERT INTO users(id,name,email_address,role,status,created_at,updated_at) VALUES(2,'Grace','grace@example.com',0,0,'2026-10-07','2026-10-07'),(3,'Inactive','inactive@example.com',0,1,'2026-10-07','2026-10-07')";
      exec setup_db
        "INSERT INTO memberships(room_id,user_id,created_at,updated_at) VALUES(1,2,'2026-10-07','2026-10-07')";
      (try
         Database.update_message (Some setup_db) ~room_id:1 ~message_id:1
           ~user_id:2 ~role:0 ~body:"unauthorized edit"
           ~timestamp:"2026-10-07 12:02:30.000";
         failwith "non-author message edit unexpectedly succeeded"
       with Database.Message_not_authorized -> ());
      check "unauthorized edit preserves the original body" true
        (Database.find_message (Some setup_db) 1 1
        |> Option.map (fun (message : Database.message) ->
               String.starts_with ~prefix:"<div>hello" message.body_html)
        |> Option.value ~default:false);
      Database.update_message (Some setup_db) ~room_id:1 ~message_id:1
        ~user_id:user_id ~role:1 ~body:"edited <b>message</b>"
        ~timestamp:"2026-10-07 12:02:31.000";
      check "message edit stores escaped Action Text" "<div>edited &lt;b&gt;message&lt;/b&gt;</div>"
        (Database.find_message (Some setup_db) 1 1
        |> Option.get |> fun (message : Database.message) -> message.body_html);
      check "message edit removes stale FTS terms" 0
        (Database.with_statement setup_db
           "SELECT count(*) FROM message_search_index WHERE message_search_index MATCH 'hello'"
           (fun statement ->
             ignore (Sqlite3.step statement);
             Sqlite3.column_int statement 0));
      check "message edit indexes the new body" 1
        (Database.with_statement setup_db
           "SELECT count(*) FROM message_search_index WHERE message_search_index MATCH 'edited'"
           (fun statement ->
             ignore (Sqlite3.step statement);
             Sqlite3.column_int statement 0));
      check "active room-form users omit inactive accounts"
        [ "Ada Lovelace"; "Grace" ]
        (Database.active_users (Some setup_db)
        |> List.map (fun (user : Database.user_option) -> user.Database.name));
      check "room creation restriction defaults to false" false
        (Database.room_creation_restricted (Some setup_db));
      let open_room_id =
        Database.create_open_room (Some setup_db) ~name:"Planning"
          ~creator_id:user_id ~timestamp:"2026-10-07 12:03:00.000"
        |> Option.get
      in
      check "new open room type and creator" "Rooms::Open:1"
        (Database.with_statement setup_db
           "SELECT type||':'||creator_id FROM rooms WHERE id=?"
           (fun statement ->
             ignore (Sqlite3.bind_int statement 1 open_room_id);
             ignore (Sqlite3.step statement);
             Sqlite3.column_text statement 0));
      check "open room grants memberships to all active users" [ 1; 2 ]
        (Database.with_statement setup_db
           "SELECT user_id FROM memberships WHERE room_id=? ORDER BY user_id"
           (fun statement ->
             ignore (Sqlite3.bind_int statement 1 open_room_id);
             let rec collect acc =
               match Sqlite3.step statement with
               | Sqlite3.Rc.ROW -> collect (Sqlite3.column_int statement 0 :: acc)
               | Sqlite3.Rc.DONE -> List.rev acc
               | error -> failwith (Sqlite3.Rc.to_string error)
             in
             collect []));
      check "room creator can administer shared room" true
        (Database.can_administer_room (Some setup_db) ~room_id:open_room_id
           ~user_id ~role:0);
      check "ordinary member cannot administer shared room" false
        (Database.can_administer_room (Some setup_db) ~room_id:open_room_id
           ~user_id:(user_id + 1) ~role:0);
      check "ordinary member cannot rename or convert shared room" false
        (Database.update_shared_room (Some setup_db) ~room_id:open_room_id
           ~user_id:(user_id + 1) ~role:0 ~name:"forged"
           ~kind:"Rooms::Closed" ~member_ids:[ user_id + 1 ]
           ~timestamp:"2026-10-07 12:03:10.000");
      check "open room converts to private and revokes unselected users" true
        (Database.update_shared_room (Some setup_db) ~room_id:open_room_id
           ~user_id ~role:1 ~name:"Private planning"
           ~kind:"Rooms::Closed" ~member_ids:[ user_id ]
           ~timestamp:"2026-10-07 12:03:15.000");
      check "private conversion retains only selected users" [ user_id ]
        (Database.room_member_ids (Some setup_db) open_room_id);
      check "private room converts to public and grants all active users" true
        (Database.update_shared_room (Some setup_db) ~room_id:open_room_id
           ~user_id ~role:1 ~name:"Planning"
           ~kind:"Rooms::Open" ~member_ids:[]
           ~timestamp:"2026-10-07 12:03:20.000");
      check "public conversion restores all active memberships" [ 1; 2 ]
        (Database.room_member_ids (Some setup_db) open_room_id);
      exec setup_db
        "INSERT INTO rooms(name,type,creator_id,created_at,updated_at) VALUES('Direct test','Rooms::Direct',1,'2026-10-07','2026-10-07')";
      exec setup_db
        "INSERT INTO memberships(room_id,user_id,created_at,updated_at) SELECT id,1,'2026-10-07','2026-10-07' FROM rooms WHERE name='Direct test'";
      let direct_test_id =
        Database.with_statement setup_db
          "SELECT id FROM rooms WHERE name='Direct test'"
          (fun statement ->
            ignore (Sqlite3.step statement);
            Sqlite3.column_int statement 0)
      in
      check "direct room history cannot be promoted to shared" false
        (Database.update_shared_room (Some setup_db) ~room_id:direct_test_id ~user_id ~role:1
           ~name:"Exposed history" ~kind:"Rooms::Open" ~member_ids:[]
           ~timestamp:"2026-10-07 12:03:25.000");
      let closed_room_id =
        Database.create_closed_room (Some setup_db) ~name:"Private planning"
          ~creator_id:user_id ~member_ids:[ user_id + 1; user_id + 1; 999 ]
          ~timestamp:"2026-10-07 12:03:30.000"
        |> Option.get
      in
      check "new private room type" "Rooms::Closed"
        (Database.with_statement setup_db
           "SELECT type FROM rooms WHERE id=?"
           (fun statement ->
             ignore (Sqlite3.bind_int statement 1 closed_room_id);
             ignore (Sqlite3.step statement);
             Sqlite3.column_text statement 0));
      check "private room grants creator and selected users once" [ 1; 2 ]
        (Database.with_statement setup_db
           "SELECT user_id FROM memberships WHERE room_id=? ORDER BY user_id"
           (fun statement ->
             ignore (Sqlite3.bind_int statement 1 closed_room_id);
             let rec collect acc =
               match Sqlite3.step statement with
               | Sqlite3.Rc.ROW -> collect (Sqlite3.column_int statement 0 :: acc)
               | Sqlite3.Rc.DONE -> List.rev acc
               | error -> failwith (Sqlite3.Rc.to_string error)
             in
             collect []));
      let direct_room_id =
        Database.find_or_create_direct_room (Some setup_db) ~creator_id:user_id
          ~member_ids:[ user_id + 1; user_id + 1; 99 ]
          ~timestamp:"2026-10-07 12:03:45.000"
        |> Option.get
      in
      check "direct room has direct Rails type" "Rooms::Direct"
        (Database.with_statement setup_db "SELECT type FROM rooms WHERE id=?"
           (fun statement ->
             ignore (Sqlite3.bind_int statement 1 direct_room_id);
             ignore (Sqlite3.step statement);
             Sqlite3.column_text statement 0));
      check "direct room grants existing selected participants" [ 1; 2; 99 ]
        (Database.room_member_ids (Some setup_db) direct_room_id);
      check "direct room defaults to all-message involvement"
        [ Some "everything"; Some "everything"; Some "everything" ]
        (List.map
           (fun participant ->
             Database.membership_involvement (Some setup_db) ~room_id:direct_room_id
               ~user_id:participant)
           [ 1; 2; 99 ]);
      check "direct room display name excludes current user" "Grace, Inactive"
        (Database.find_room_for_user (Some setup_db) user_id direct_room_id
        |> Option.map (fun (room : Database.room) -> room.Database.name)
        |> Option.value ~default:"");
      check "direct room is singleton for the exact participant set" direct_room_id
        (Database.find_or_create_direct_room (Some setup_db) ~creator_id:(user_id + 1)
           ~member_ids:[ user_id; 99 ] ~timestamp:"2026-10-07 12:03:50.000"
        |> Option.get);
      check "different direct participant set creates a separate room" true
        (Database.find_or_create_direct_room (Some setup_db) ~creator_id:user_id
           ~member_ids:[] ~timestamp:"2026-10-07 12:03:55.000"
        |> Option.get <> direct_room_id);
      exec setup_db
        "UPDATE accounts SET settings='{\"restrict_room_creation_to_administrators\":true}'";
      check "account setting restricts room creation" true
        (Database.room_creation_restricted (Some setup_db));
      (try
         ignore
           (Database.create_open_room (Some setup_db) ~name:"Forbidden"
              ~creator_id:(user_id + 2) ~timestamp:"2026-10-07 12:04:00.000");
         failwith "inactive room creator unexpectedly succeeded"
       with Failure message when message = "active room creator required" -> ());
      check "failed open-room creation is rolled back" 5
        (Database.with_statement setup_db "SELECT count(*) FROM rooms" (fun statement ->
             ignore (Sqlite3.step statement);
             Sqlite3.column_int statement 0));
      for index = 2 to 42 do
        Database.create_message (Some setup_db) ~room_id:1 ~creator_id:user_id
          ~body:(Printf.sprintf "page message %02d" index)
          ~client_message_id:(Printf.sprintf "page-%012d" index)
          ~timestamp:(Printf.sprintf "2026-10-07 12:02:%02d.000" index)
      done;
      let latest = Database.messages_for_room (Some setup_db) 1 in
      check "room latest page is capped at forty" 40 (List.length latest);
      check "room latest page is chronological" (3, 42)
        ((List.hd latest).Database.id, (List.hd (List.rev latest)).Database.id);
      check "before cursor returns older chronological page" (21, 1, 21)
        (let page =
           Database.messages_for_room ~before:22 (Some setup_db) 1
         in
         (List.length page, (List.hd page).Database.id,
          (List.hd (List.rev page)).Database.id));
      check "after cursor returns newer chronological page" (20, 23, 42)
        (let page = Database.messages_for_room ~after:22 (Some setup_db) 1 in
         (List.length page, (List.hd page).Database.id,
          (List.hd (List.rev page)).Database.id));
      check "permalink page surrounds its pivot" (42, 1, 42)
        (let page = Database.messages_for_room ~around:22 (Some setup_db) 1 in
         (List.length page, (List.hd page).Database.id,
          (List.hd (List.rev page)).Database.id));
      check "cursor navigation detects adjacent pages" true
        (Database.has_messages_before (Some setup_db) 1 3
        && Database.has_messages_after (Some setup_db) 1 22
        && not (Database.has_messages_before (Some setup_db) 1 1)
        && not (Database.has_messages_after (Some setup_db) 1 42));
      (try
         ignore (Database.messages_for_room ~before:999 (Some setup_db) 1);
         failwith "missing pagination cursor unexpectedly succeeded"
       with Database.Message_not_found -> ());
      check "first-run grants creator room membership" 1
        (Database.with_statement setup_db
           "SELECT count(*) FROM memberships WHERE user_id=? AND room_id=(SELECT id FROM rooms)"
           (fun statement ->
             ignore (Sqlite3.bind_int statement 1 user_id);
             ignore (Sqlite3.step statement);
             Sqlite3.column_int statement 0));
      (try
         ignore
           (Database.create_first_run (Some setup_db) ~name:"Second"
              ~email_address:"second@example.com" ~password_digest
              ~timestamp:"2026-10-07 12:01:00.000");
         failwith "second first-run setup unexpectedly succeeded"
       with Failure message when message = "Campfire has already been set up" -> ());
      check "repeat first-run leaves existing users unchanged" 4
        (Database.with_statement setup_db "SELECT count(*) FROM users" (fun statement ->
             ignore (Sqlite3.step statement);
             Sqlite3.column_int statement 0));
      let join_code =
        Database.with_statement setup_db "SELECT join_code FROM accounts" (fun statement ->
            ignore (Sqlite3.step statement);
            Sqlite3.column_text statement 0)
      in
      check "invitation code matches this account" true
        (Database.valid_join_code (Some setup_db) join_code);
      check "invalid invitation code is rejected" false
        (Database.valid_join_code (Some setup_db) "not-the-join-code");
      let joined_user_id =
        Database.create_join_user (Some setup_db) ~join_code ~name:"Lin"
          ~email_address:"lin@example.com"
          ~password_digest:(Bcrypt.hash "join password")
          ~timestamp:"2026-10-07 12:30:00.000"
        |> Option.get
      in
      check "joined user has member role" 0
        (Database.with_statement setup_db "SELECT role FROM users WHERE id=?"
           (fun statement ->
             ignore (Sqlite3.bind_int statement 1 joined_user_id);
             ignore (Sqlite3.step statement);
             Sqlite3.column_int statement 0));
      check "joined user receives every existing open room only"
        [ "All Talk"; "Planning" ]
        (Database.with_statement setup_db
           "SELECT r.name FROM rooms r JOIN memberships m ON m.room_id=r.id WHERE m.user_id=? ORDER BY r.id"
           (fun statement ->
             ignore (Sqlite3.bind_int statement 1 joined_user_id);
             let rec collect names =
               match Sqlite3.step statement with
               | Sqlite3.Rc.ROW -> collect (Sqlite3.column_text statement 0 :: names)
               | Sqlite3.Rc.DONE -> List.rev names
               | error -> failwith (Sqlite3.Rc.to_string error)
             in
             collect []));
      (try
         ignore
           (Database.create_join_user (Some setup_db) ~join_code
              ~name:"Duplicate" ~email_address:"grace@example.com"
              ~password_digest:(Bcrypt.hash "another password")
              ~timestamp:"2026-10-07 12:31:00.000");
         failwith "duplicate join email unexpectedly succeeded"
       with Database.Duplicate_email -> ());
      (try
         ignore
           (Database.create_join_user (Some setup_db) ~join_code:"wrong"
              ~name:"Wrong invite" ~email_address:"wrong@example.com"
              ~password_digest:(Bcrypt.hash "join password")
              ~timestamp:"2026-10-07 12:32:00.000");
         failwith "invalid join code unexpectedly succeeded"
       with Database.Invalid_join_code -> ());
      check "failed invitations leave no extra user" 5
        (Database.with_statement setup_db "SELECT count(*) FROM users" (fun statement ->
             ignore (Sqlite3.step statement);
             Sqlite3.column_int statement 0));
      Database.create_message (Some setup_db) ~room_id:1 ~creator_id:user_id
        ~body:"delete me" ~client_message_id:"00000000-0000-4000-8000-000000000099"
        ~timestamp:"2026-10-07 12:40:00.000";
      let delete_id =
        Database.with_statement setup_db
          "SELECT id FROM messages WHERE client_message_id='00000000-0000-4000-8000-000000000099'"
          (fun statement ->
            ignore (Sqlite3.step statement);
            Sqlite3.column_int statement 0)
      in
      exec setup_db
        (Printf.sprintf "INSERT INTO active_storage_attachments(record_type,record_id,name,blob_id) VALUES('Message',%d,'attachment',1)" delete_id);
      (try
         Database.update_message (Some setup_db) ~room_id:1 ~message_id:delete_id
           ~user_id ~role:1 ~body:"drop attachment" ~timestamp:"2026-10-07 12:41:00.000";
         failwith "attached message edit unexpectedly succeeded"
       with Database.Message_has_attachments -> ());
      check "unsupported attached-message edit is non-destructive" "<div>delete me</div>"
        (Database.find_message (Some setup_db) 1 delete_id
        |> Option.get |> fun (message : Database.message) -> message.body_html);
      (try
         Database.delete_message (Some setup_db) ~room_id:1 ~message_id:delete_id
           ~user_id ~role:1;
         failwith "attached message deletion unexpectedly succeeded"
       with Database.Message_has_attachments -> ());
      check "unsupported attached-message deletion is non-destructive" true
        (Database.find_message (Some setup_db) 1 delete_id <> None);
      exec setup_db
        (Printf.sprintf "DELETE FROM active_storage_attachments WHERE record_id=%d" delete_id);
      (try
         Database.delete_message (Some setup_db) ~room_id:1 ~message_id:delete_id
           ~user_id:2 ~role:0;
         failwith "non-author message deletion unexpectedly succeeded"
       with Database.Message_not_authorized -> ());
      check "unauthorized delete preserves the message" true
        (Database.find_message (Some setup_db) 1 delete_id <> None);
      Database.delete_message (Some setup_db) ~room_id:1 ~message_id:delete_id
        ~user_id ~role:1;
      check "message deletion removes the row and FTS entry" 0
        (Database.with_statement setup_db
           "SELECT count(*) FROM message_search_index WHERE message_search_index MATCH 'delete'"
           (fun statement ->
             ignore (Sqlite3.step statement);
             Sqlite3.column_int statement 0));
      let deletion_room_id =
        Database.create_closed_room (Some setup_db) ~name:"Cascade delete"
          ~creator_id:user_id ~member_ids:[ user_id + 1 ]
          ~timestamp:"2026-10-07 12:42:00.000"
        |> Option.get
      in
      Database.create_message (Some setup_db) ~room_id:deletion_room_id
        ~creator_id:user_id ~body:"roomcascade searchable"
        ~client_message_id:"00000000-0000-4000-8000-000000000098"
        ~timestamp:"2026-10-07 12:42:01.000";
      let cascade_message_id =
        Database.with_statement setup_db
          "SELECT id FROM messages WHERE client_message_id='00000000-0000-4000-8000-000000000098'"
          (fun statement ->
            ignore (Sqlite3.step statement);
            Sqlite3.column_int statement 0)
      in
      exec setup_db
        (Printf.sprintf "INSERT INTO boosts(message_id,booster_id,content,created_at,updated_at) VALUES(%d,%d,'👍','2026-10-07','2026-10-07')"
           cascade_message_id user_id);
      (try
         ignore
           (Database.delete_room (Some setup_db) ~room_id:deletion_room_id
              ~user_id:(user_id + 1) ~role:0);
         failwith "non-admin non-creator room deletion unexpectedly succeeded"
       with Database.Room_not_authorized -> ());
      check "unauthorized room delete has no effect" true
        (Database.find_room_for_user (Some setup_db) (user_id + 1) deletion_room_id
        <> None);
      check "shared-room deletion is atomic and cascades indexed content" true
        (Database.delete_room (Some setup_db) ~room_id:deletion_room_id ~user_id
           ~role:1);
      check "room delete removes its message, boost, rich text, and FTS row" [ 0; 0; 0; 0 ]
        (List.map
           (fun sql ->
             Database.with_statement setup_db sql (fun statement ->
               ignore (Sqlite3.step statement);
               Sqlite3.column_int statement 0))
           [ "SELECT count(*) FROM messages WHERE room_id=" ^ string_of_int deletion_room_id;
             "SELECT count(*) FROM boosts WHERE message_id=" ^ string_of_int cascade_message_id;
             "SELECT count(*) FROM action_text_rich_texts WHERE record_type='Message' AND record_id=" ^ string_of_int cascade_message_id;
             "SELECT count(*) FROM message_search_index WHERE rowid=" ^ string_of_int cascade_message_id ]);
      let attached_room_id =
        Database.create_closed_room (Some setup_db) ~name:"Keep attachments"
          ~creator_id:user_id ~member_ids:[] ~timestamp:"2026-10-07 12:43:00.000"
        |> Option.get
      in
      Database.create_message (Some setup_db) ~room_id:attached_room_id
        ~creator_id:user_id ~body:"attached"
        ~client_message_id:"00000000-0000-4000-8000-000000000097"
        ~timestamp:"2026-10-07 12:43:01.000";
      let attached_message_id =
        Database.with_statement setup_db
          "SELECT id FROM messages WHERE client_message_id='00000000-0000-4000-8000-000000000097'"
          (fun statement ->
            ignore (Sqlite3.step statement);
            Sqlite3.column_int statement 0)
      in
      exec setup_db
        (Printf.sprintf "INSERT INTO active_storage_attachments(record_type,record_id,name,blob_id) VALUES('Message',%d,'attachment',1)" attached_message_id);
      (try
         ignore
           (Database.delete_room (Some setup_db) ~room_id:attached_room_id
              ~user_id ~role:1);
         failwith "attached room deletion unexpectedly succeeded"
       with Database.Room_has_attachments -> ());
      check "room with unsupported attachments stays intact" true
        (Database.find_room_for_user (Some setup_db) user_id attached_room_id <> None
        && Database.find_message (Some setup_db) attached_room_id attached_message_id <> None);
      exec setup_db
        (Printf.sprintf "DELETE FROM active_storage_attachments WHERE record_id=%d" attached_message_id);
      ignore
        (Database.delete_room (Some setup_db) ~room_id:attached_room_id ~user_id
           ~role:1);
      check "direct conversation is deletable by any participant" true
        (Database.delete_room (Some setup_db) ~room_id:direct_room_id
           ~user_id:(user_id + 1) ~role:0);
      check "direct conversation deletion removes all memberships" []
        (Database.room_member_ids (Some setup_db) direct_room_id))
