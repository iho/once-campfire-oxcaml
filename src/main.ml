let serve ~port ~domains =
  Eio_main.run (fun env ->
      let server =
        Cohttp_eio.Server.make
          ~callback:(fun _connection request _body ->
            let path = Cohttp.Request.resource request |> String.split_on_char '?' |> List.hd in
            match path with
            | "/up" ->
                Cohttp_eio.Server.respond_string ~status:`OK
                  ~body:
                    "<!doctype html><html><body style=\"background-color: green\">OK</body></html>"
                  ()
            | _ -> Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"Not found" ())
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
    (fun () -> serve ~port ~domains)
