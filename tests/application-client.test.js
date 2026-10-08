import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { runInNewContext } from "node:vm";
import { test } from "node:test";

function eventTarget() {
  const handlers = new Map();
  return {
    addEventListener(name, handler) {
      const values = handlers.get(name) ?? [];
      values.push(handler);
      handlers.set(name, values);
    },
    dispatch(name, value = {}) {
      for (const handler of handlers.get(name) ?? []) handler(value);
    },
  };
}

test("OxCaml browser client subscribes and handles live room state", () => {
  const socketEvents = [];
  const sockets = [];
  class MockWebSocket {
    static OPEN = 1;
    readyState = MockWebSocket.OPEN;
    events = eventTarget();
    sent = [];
    constructor(url, protocol) {
      this.url = url;
      this.protocol = protocol;
      sockets.push(this);
    }
    addEventListener(...args) { this.events.addEventListener(...args); }
    send(value) { this.sent.push(JSON.parse(value)); }
    close(code, reason) { socketEvents.push({ type: "close", code, reason }); }
    receive(value) { this.events.dispatch("message", { data: JSON.stringify(value) }); }
    open() { this.events.dispatch("open"); }
  }

  const timers = new Map();
  let nextTimer = 0;
  const setTimeoutMock = (fn, delay) => {
    const id = ++nextTimer;
    timers.set(id, { fn, delay });
    return id;
  };
  const streamSources = ["Turbo::StreamsChannel", "Turbo::StreamsChannel", "RoomMessagesChannel"].map(
    (channel, index) => ({
      getAttribute: name => name === "channel" ? channel : `signed-${index}`,
    }),
  );
  const sidebarTargets = [12, 34].map(roomId => ({
    dataset: { roomId: String(roomId) },
    toggles: [],
    classList: { toggle: (name, value) => sidebarTargets[roomId === 12 ? 0 : 1].toggles.push([name, value]) },
  }));
  const indicator = { textContent: "", hidden: true };
  const composer = eventTarget();
  const main = { dataset: { roomId: "12", userId: "7" } };
  const documentEvents = eventTarget();
  const windowEvents = eventTarget();
  const appended = [];
  const document = {
    readyState: "complete",
    querySelector(selector) {
      if (selector.startsWith("main[data-")) return main;
      if (selector === ".composer textarea") return composer;
      return null;
    },
    querySelectorAll(selector) {
      if (selector === "turbo-cable-stream-source") return streamSources;
      if (selector.startsWith("[data-room-id=")) return sidebarTargets;
      return [];
    },
    getElementById(id) { return id === "typing-indicator" ? indicator : null; },
    addEventListener: (...args) => documentEvents.addEventListener(...args),
  };
  class MockDOMParser {
    parseFromString(html) {
      assert.equal(html, "<turbo-stream>fixture</turbo-stream>");
      return {
        querySelectorAll: () => [{
          getAttribute: name => name === "action" ? "append" : "messages",
          querySelector: () => ({ content: { cloneNode: () => "stream-fragment" } }),
        }],
      };
    }
  }
  const target = { append: value => appended.push(value) };
  document.getElementById = id => id === "typing-indicator" ? indicator : id === "messages" ? target : null;

  const source = readFileSync(new URL("../assets/application.js", import.meta.url), "utf8");
  runInNewContext(source, {
    window: { WebSocket: MockWebSocket, addEventListener: (...args) => windowEvents.addEventListener(...args) },
    document,
    location: { protocol: "https:", host: "campfire.example.test" },
    WebSocket: MockWebSocket,
    DOMParser: MockDOMParser,
    CSS: { escape: String },
    setTimeout: setTimeoutMock,
    clearTimeout: id => timers.delete(id),
  });

  assert.equal(sockets.length, 1);
  const socket = sockets[0];
  assert.equal(socket.url, "wss://campfire.example.test/cable");
  assert.equal(socket.protocol, "actioncable-v1-json");
  socket.open();
  const subscriptions = socket.sent
    .filter(frame => frame.command === "subscribe")
    .map(frame => JSON.parse(frame.identifier));
  assert.deepEqual(subscriptions, [
    { channel: "Turbo::StreamsChannel", signed_stream_name: "signed-0" },
    { channel: "Turbo::StreamsChannel", signed_stream_name: "signed-1" },
    { channel: "RoomMessagesChannel", signed_stream_name: "signed-2" },
    { channel: "UnreadRoomsChannel" },
    { channel: "ReadRoomsChannel" },
    { channel: "PresenceChannel", room_id: 12 },
    { channel: "TypingNotificationsChannel", room_id: 12 },
  ]);

  const presence = JSON.stringify({ channel: "PresenceChannel", room_id: 12 });
  socket.receive({ type: "confirm_subscription", identifier: presence });
  assert(socket.sent.some(frame => frame.command === "message" && frame.identifier === presence &&
    JSON.parse(frame.data).action === "present"));

  socket.receive({ message: "<turbo-stream>fixture</turbo-stream>" });
  assert.deepEqual(appended, ["stream-fragment"]);
  socket.receive({ message: { roomId: 34 } });
  socket.receive({ message: { room_id: 34 } });
  assert.deepEqual(sidebarTargets[1].toggles, [["unread", true], ["unread", false]]);

  const typing = JSON.stringify({ channel: "TypingNotificationsChannel", room_id: 12 });
  socket.receive({ identifier: typing, message: { action: "start", user: { id: 8, name: "Alex" } } });
  assert.equal(indicator.textContent, "Alex is typing…");
  assert.equal(indicator.hidden, false);
  socket.receive({ identifier: typing, message: { action: "start", user: { id: 7, name: "Me" } } });
  assert.equal(indicator.textContent, "Alex is typing…");
  socket.receive({ identifier: typing, message: { action: "stop", user: { id: 8, name: "Alex" } } });
  assert.equal(indicator.textContent, "");
  assert.equal(indicator.hidden, true);

  composer.dispatch("input");
  assert(socket.sent.some(frame => frame.command === "message" && frame.identifier === typing && JSON.parse(frame.data).action === "start"));
  const stopTimer = [...timers.entries()].find(([, timer]) => timer.delay === 900);
  assert(stopTimer, "typing start schedules a stop");
  stopTimer[1].fn();
  assert(socket.sent.some(frame => frame.command === "message" && frame.identifier === typing && JSON.parse(frame.data).action === "stop"));

  windowEvents.dispatch("pagehide");
  assert(socket.sent.some(frame => frame.command === "message" && frame.identifier === presence && JSON.parse(frame.data).action === "absent"));
  assert(socketEvents.some(event => event.type === "close" && event.code === 1000));
});
