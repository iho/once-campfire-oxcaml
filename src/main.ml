let response ?(headers = Cohttp.Header.init ()) status body =
  Cohttp_eio.Server.respond_string ~status ~headers ~body ()

let redirect target =
  response
    ~headers:(Cohttp.Header.init_with "location" target)
    `Found ""

let serve ~database ~port ~domains =
  Eio_main.run (fun env ->
      let server =
        Cohttp_eio.Server.make
          ~callback:(fun _connection request _body ->
            let path = Cohttp.Request.resource request |> String.split_on_char '?' |> List.hd in
            match (Cohttp.Request.meth request, path) with
            | `GET, "/up" ->
                response `OK
                  "<!doctype html><html><body style=\"background-color: green\">OK</body></html>"
            | `GET, "/" ->
                redirect
                  (if Database.account_exists database then "/session/new"
                   else "/first_run")
            | `GET, "/first_run" ->
                if Database.account_exists database then redirect "/"
                else
                  response ~headers:(Cohttp.Header.init_with "content-type" "text/html; charset=utf-8")
                    `OK
                    "<!doctype html><html><body><main><h1>Set up Campfire</h1><p>First-run account creation is not implemented in this OxCaml port yet.</p></main></body></html>"
            | `GET, "/session/new" ->
                if Database.user_exists database then
                  response ~headers:(Cohttp.Header.init_with "content-type" "text/html; charset=utf-8")
                    `Not_implemented "Campfire sign-in is not implemented in this OxCaml port yet."
                else redirect "/first_run"
            | _ -> response `Not_found "Not found")
          ()
      in
      Eio.Switch.run (fun sw ->
          let socket =
            Eio.Net.listen ~reuse_addr:true ~backlog:128 ~sw env#net
              (`Tcp (Eio.Net.Ipaddr.V4.any, port))
          in
          Cohttp_eio.Server.run ~additional_domains:(env#domain_mgr, domains - 1) socket server
            ~on_error:(fun error -> prerr_endline (Printexc.to_string error))))

let () =
  let port =
    match Sys.getenv_opt "HTTP_PORT" with
    | None -> 3000
    | Some value -> (try int_of_string value with Failure _ -> 3000)
  in
  let domains =
    match Sys.getenv_opt "WEB_WORKERS" with
    | None -> 4
    | Some value -> (try max 1 (min 64 (int_of_string value)) with Failure _ -> 4)
  in
  let storage_root =
    match Sys.getenv_opt "CAMPFIRE_STORAGE_PATH" with
    | None -> "storage"
    | Some value -> value
  in
  let database = Database.open_existing storage_root in
  Fun.protect
    ~finally:(fun () -> Option.iter (fun db -> ignore (Sqlite3.db_close db)) database)
    (fun () -> serve ~database ~port ~domains)
