const cableUrl = () => `${location.protocol === "https:" ? "wss:" : "ws:"}//${location.host}/cable`;

function applyTurboStreams(html) {
  const documentFragment = new DOMParser().parseFromString(html, "text/html");
  for (const stream of documentFragment.querySelectorAll("turbo-stream")) {
    const targetName = stream.getAttribute("target");
    const target = targetName ? document.getElementById(targetName) : null;
    const template = stream.querySelector("template");
    if (!target || !template) continue;
    const content = template.content.cloneNode(true);
    switch (stream.getAttribute("action")) {
      case "append": target.append(content); break;
      case "prepend": target.prepend(content); break;
      case "before": target.before(content); break;
      case "after": target.after(content); break;
      case "replace": target.replaceWith(content); break;
      case "update": target.replaceChildren(content); break;
      case "remove": target.remove(); break;
    }
  }
}

const roomTargets = roomId => document.querySelectorAll(`[data-room-id="${CSS.escape(String(roomId))}"]`);
const currentRoomId = Number(document.querySelector("main[data-room-id]")?.dataset.roomId || 0);
const currentUserId = Number(document.querySelector("main[data-user-id]")?.dataset.userId || 0);

function setUnread(roomId, unread) {
  for (const target of roomTargets(roomId)) {
    if (Number(target.dataset.roomId) === currentRoomId) continue;
    target.classList.toggle("unread", unread);
  }
}

function startCable() {
  if (!("WebSocket" in window)) return;
  let stopped = false;
  let retryDelay = 500;
  let socket;
  let retryTimer;
  let presenceIdentifier;
  let typingIdentifier;
  let typingStopTimer;
  let typingNames = new Map();
  const sources = [...document.querySelectorAll("turbo-cable-stream-source")];
  const subscriptions = sources.map(source => ({
    channel: source.getAttribute("channel"),
    signed_stream_name: source.getAttribute("signed-stream-name"),
  })).filter(source => source.channel && source.signed_stream_name);
  subscriptions.push({ channel: "UnreadRoomsChannel" }, { channel: "ReadRoomsChannel" });
  if (currentRoomId) {
    presenceIdentifier = JSON.stringify({ channel: "PresenceChannel", room_id: currentRoomId });
    typingIdentifier = JSON.stringify({ channel: "TypingNotificationsChannel", room_id: currentRoomId });
    subscriptions.push(JSON.parse(presenceIdentifier), JSON.parse(typingIdentifier));
  }

  const send = payload => {
    if (socket?.readyState === WebSocket.OPEN) socket.send(JSON.stringify(payload));
  };
  const perform = (identifier, action) => send({
    command: "message", identifier, data: JSON.stringify({ action }),
  });
  const updateTyping = () => {
    const target = document.getElementById("typing-indicator");
    if (!target) return;
    const names = [...typingNames.values()];
    target.textContent = names.length ? `${names.join(", ")} ${names.length === 1 ? "is" : "are"} typing…` : "";
    target.hidden = names.length === 0;
  };

  const connect = () => {
    if (stopped) return;
    try {
      socket = new WebSocket(cableUrl(), "actioncable-v1-json");
    } catch {
      retryTimer = setTimeout(connect, retryDelay);
      retryDelay = Math.min(retryDelay * 2, 10_000);
      return;
    }
    socket.addEventListener("open", () => {
      retryDelay = 500;
      for (const channel of subscriptions) {
        const identifier = JSON.stringify(channel);
        send({ command: "subscribe", identifier });
      }
    });
    socket.addEventListener("message", event => {
      let frame;
      try { frame = JSON.parse(event.data); } catch { return; }
      if (frame.type === "confirm_subscription" && frame.identifier === presenceIdentifier) {
        perform(presenceIdentifier, "present");
      }
      if (typeof frame.message === "string") {
        applyTurboStreams(frame.message);
      } else if (frame.message && typeof frame.message === "object") {
        if ("roomId" in frame.message) setUnread(frame.message.roomId, frame.message.roomId !== currentRoomId);
        if ("room_id" in frame.message) setUnread(frame.message.room_id, false);
        if ("action" in frame.message && "user" in frame.message) {
          const user = frame.message.user;
          if (Number(user.id) !== currentUserId) {
            if (frame.message.action === "start") typingNames.set(user.id, user.name);
            else if (frame.message.action === "stop") typingNames.delete(user.id);
            updateTyping();
          }
        }
      }
    });
    socket.addEventListener("close", () => {
      typingNames.clear();
      updateTyping();
      if (!stopped) {
        retryTimer = setTimeout(connect, retryDelay);
        retryDelay = Math.min(retryDelay * 2, 10_000);
      }
    });
    socket.addEventListener("error", () => socket.close());
  };

  const composer = document.querySelector(".composer textarea");
  if (composer && typingIdentifier) {
    composer.addEventListener("input", () => {
      perform(typingIdentifier, "start");
      clearTimeout(typingStopTimer);
      typingStopTimer = setTimeout(() => perform(typingIdentifier, "stop"), 900);
    });
    composer.addEventListener("blur", () => {
      clearTimeout(typingStopTimer);
      perform(typingIdentifier, "stop");
    });
  }
  const refreshPresence = () => perform(presenceIdentifier, "refresh");
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "visible") refreshPresence();
  });
  window.addEventListener("pagehide", () => {
    stopped = true;
    clearTimeout(retryTimer);
    clearTimeout(typingStopTimer);
    if (presenceIdentifier) perform(presenceIdentifier, "absent");
    socket?.close(1000, "page navigation");
  }, { once: true });
  connect();
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", startCable, { once: true });
} else {
  startCable();
}
