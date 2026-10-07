let check label expected actual =
  if expected <> actual then failwith (label ^ ": unexpected result")

let () = Eio_main.run (fun _env ->
  let bus = Cable_bus.create () in
  let queue = Eio.Stream.create 8 in
  let subscription =
    Cable_bus.subscribe bus ~room_id:12 ~identifier:"room-stream" ~queue
  in
  let message : Database.message =
    { id = 7; creator_id = 3; client_message_id = "uuid-7"; creator_name = "Ada";
      body_html = "<div>hello</div>"; created_at = "2026-10-07 12:00:00.000" }
  in
  Cable_bus.publish bus ~room_id:11 message;
  check "room events are isolated" None (Eio.Stream.take_nonblocking queue);
  Cable_bus.publish bus ~room_id:12 message;
  check "room events carry their signed stream and persisted message"
    (Some (Cable_bus.Message ("room-stream", 12, message)))
    (Eio.Stream.take_nonblocking queue);
  Cable_bus.publish_replace bus ~room_id:12 message;
  check "edited message replaces its subscribed room item"
    (Some (Cable_bus.Replace ("room-stream", 12, message)))
    (Eio.Stream.take_nonblocking queue);
  Cable_bus.publish_remove bus ~room_id:12 ~message_dom_id:"message_uuid-7";
  check "deleted message removes its subscribed room item"
    (Some (Cable_bus.Remove ("room-stream", "message_uuid-7")))
    (Eio.Stream.take_nonblocking queue);
  Cable_bus.unsubscribe subscription;
  Cable_bus.publish bus ~room_id:12 message;
  check "unsubscribed stream receives no messages" None
    (Eio.Stream.take_nonblocking queue);
  let unread_queue = Eio.Stream.create 8 in
  let unread_subscription =
    Cable_bus.subscribe_unreads bus ~user_id:3 ~identifier:"unreads"
      ~queue:unread_queue
  in
  Cable_bus.publish_unread bus ~user_id:4 ~room_id:12;
  check "unread notifications are isolated by user" None
    (Eio.Stream.take_nonblocking unread_queue);
  Cable_bus.publish_unread bus ~user_id:3 ~room_id:12;
  check "unread notification contains the room id"
    (Some (Cable_bus.Unread ("unreads", 12)))
    (Eio.Stream.take_nonblocking unread_queue);
  Cable_bus.unsubscribe unread_subscription;
  Cable_bus.publish_unread bus ~user_id:3 ~room_id:12;
  check "unsubscribed user receives no unread events" None
    (Eio.Stream.take_nonblocking unread_queue);
  let read_queue = Eio.Stream.create 8 in
  let read_subscription =
    Cable_bus.subscribe_reads bus ~user_id:3 ~identifier:"reads" ~queue:read_queue
  in
  Cable_bus.publish_read bus ~user_id:4 ~room_id:12;
  check "read notifications are isolated by user" None
    (Eio.Stream.take_nonblocking read_queue);
  Cable_bus.publish_read bus ~user_id:3 ~room_id:12;
  check "read notification contains the room id"
    (Some (Cable_bus.Read ("reads", 12)))
    (Eio.Stream.take_nonblocking read_queue);
  Cable_bus.unsubscribe read_subscription;
  Cable_bus.publish_read bus ~user_id:3 ~room_id:12;
  check "unsubscribed user receives no read events" None
    (Eio.Stream.take_nonblocking read_queue))
