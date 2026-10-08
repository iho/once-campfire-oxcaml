type event =
  | Message of string * int * string * Database.message * Database.boost list
      * Database.message_attachment list
  | Replace of string * int * string * Database.message * Database.boost list
      * Database.message_attachment list
  | Remove of string * string
  | Boost_append of string * string * string
  | Boost_remove of string * string
  | Unread of string * int
  | Read of string * int
  | Sidebar of string * string
  | Typing of string * string * int * string
type queue = event Eio.Stream.t

type topic =
  | Room of int
  | User_unreads of int
  | User_reads of int
  | Room_typing of int
  | Global_sidebar
  | User_sidebar of int
type subscriber = { id : int; topic : topic; identifier : string; queue : queue }
type t = { lock : Eio.Mutex.t; mutable subscribers : subscriber list; mutable next_id : int }
type subscription = { bus : t; id : int }

let create () = { lock = Eio.Mutex.create (); subscribers = []; next_id = 1 }

let subscribe bus ~room_id ~identifier ~queue =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      let id = bus.next_id in
      bus.next_id <- id + 1;
      bus.subscribers <-
        { id; topic = Room room_id; identifier; queue } :: bus.subscribers;
      { bus; id })

let subscribe_unreads bus ~user_id ~identifier ~queue =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      let id = bus.next_id in
      bus.next_id <- id + 1;
      bus.subscribers <-
        { id; topic = User_unreads user_id; identifier; queue } :: bus.subscribers;
      { bus; id })

let subscribe_reads bus ~user_id ~identifier ~queue =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      let id = bus.next_id in
      bus.next_id <- id + 1;
      bus.subscribers <-
        { id; topic = User_reads user_id; identifier; queue } :: bus.subscribers;
      { bus; id })

let subscribe_typing bus ~room_id ~identifier ~queue =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      let id = bus.next_id in
      bus.next_id <- id + 1;
      bus.subscribers <-
        { id; topic = Room_typing room_id; identifier; queue } :: bus.subscribers;
      { bus; id })

let subscribe_sidebar_global bus ~identifier ~queue =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      let id = bus.next_id in
      bus.next_id <- id + 1;
      bus.subscribers <-
        { id; topic = Global_sidebar; identifier; queue } :: bus.subscribers;
      { bus; id })

let subscribe_sidebar_user bus ~user_id ~identifier ~queue =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      let id = bus.next_id in
      bus.next_id <- id + 1;
      bus.subscribers <-
        { id; topic = User_sidebar user_id; identifier; queue } :: bus.subscribers;
      { bus; id })

let unsubscribe subscription =
  let bus = subscription.bus in
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      bus.subscribers <-
        List.filter (fun (subscriber : subscriber) -> subscriber.id <> subscription.id)
          bus.subscribers)

let publish bus ~room_id ~room_name ~boosts ~attachments message =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      List.iter
        (fun subscriber ->
          if subscriber.topic = Room room_id then
            Eio.Stream.add subscriber.queue
              (Message (subscriber.identifier, room_id, room_name, message, boosts, attachments)))
        bus.subscribers)

let publish_replace bus ~room_id ~room_name ~boosts ~attachments message =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      List.iter
        (fun subscriber ->
          if subscriber.topic = Room room_id then
            Eio.Stream.add subscriber.queue
              (Replace (subscriber.identifier, room_id, room_name, message, boosts, attachments)))
        bus.subscribers)

let publish_remove bus ~room_id ~message_dom_id =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      List.iter
        (fun subscriber ->
          if subscriber.topic = Room room_id then
            Eio.Stream.add subscriber.queue
              (Remove (subscriber.identifier, message_dom_id)))
        bus.subscribers)

let publish_boost_append bus ~room_id ~target ~html =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      List.iter
        (fun subscriber ->
          if subscriber.topic = Room room_id then
            Eio.Stream.add subscriber.queue
              (Boost_append (subscriber.identifier, target, html)))
        bus.subscribers)

let publish_boost_remove bus ~room_id ~boost_dom_id =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      List.iter
        (fun subscriber ->
          if subscriber.topic = Room room_id then
            Eio.Stream.add subscriber.queue
              (Boost_remove (subscriber.identifier, boost_dom_id)))
        bus.subscribers)

let publish_unread bus ~user_id ~room_id =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      List.iter
        (fun subscriber ->
          if subscriber.topic = User_unreads user_id then
            Eio.Stream.add subscriber.queue (Unread (subscriber.identifier, room_id)))
        bus.subscribers)

let publish_read bus ~user_id ~room_id =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      List.iter
        (fun subscriber ->
          if subscriber.topic = User_reads user_id then
            Eio.Stream.add subscriber.queue (Read (subscriber.identifier, room_id)))
        bus.subscribers)

let publish_typing bus ~room_id ~action ~user_id ~user_name =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      List.iter
        (fun subscriber ->
          if subscriber.topic = Room_typing room_id then
            Eio.Stream.add subscriber.queue
              (Typing (subscriber.identifier, action, user_id, user_name)))
        bus.subscribers)

let publish_sidebar_global bus html =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      List.iter
        (fun subscriber ->
          if subscriber.topic = Global_sidebar then
            Eio.Stream.add subscriber.queue (Sidebar (subscriber.identifier, html)))
        bus.subscribers)

let publish_sidebar_user bus ~user_id html =
  Eio.Mutex.use_rw ~protect:true bus.lock (fun () ->
      List.iter
        (fun subscriber ->
          if subscriber.topic = User_sidebar user_id then
            Eio.Stream.add subscriber.queue (Sidebar (subscriber.identifier, html)))
        bus.subscribers)
