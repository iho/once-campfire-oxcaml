# ONCE Campfire in OxCaml

An OxCaml implementation of ONCE Campfire, intended to use the Rails application's
existing SQLite database, file storage and signed cookies. The public Rails application
is the behavior reference; this project is a separate implementation, not a replacement
for the Express repository.

OxCaml is Jane Street's performance-oriented set of OCaml extensions. The compiler is
experimental and does not promise extension stability. This project will pin the compiler
and library versions used for each verified build. CI and Docker use a fixed OxCaml opam
repository snapshot; the Linux Eio build also pins the liburing binding to its compatible
version.

## Development

Install [OxCaml](https://oxcaml.org/get-oxcaml/), Dune, curl, libvips, FFmpeg, Poppler,
qrencode, zlib and OpenSSL 3 development headers (`brew install curl vips ffmpeg poppler qrencode zlib openssl@3` on macOS; install `zlib1g-dev` on Debian), then run:

```sh
opam switch create 5.2.0+ox \
  --empty \
  --repos ox=git+https://github.com/oxcaml/opam-repository.git#3416edee6b2416f5752ce101e3f7a1933e570a32,default
eval "$(opam env --switch=5.2.0+ox)"
opam install ./once_campfire_oxcaml.opam --deps-only
opam exec --switch=5.2.0+ox -- dune build --profile=release
opam exec --switch=5.2.0+ox -- dune exec campfire-oxcaml
```

The server binds `0.0.0.0:3000`; `HTTP_PORT` selects another port and
`HTTP_BIND_ADDRESS` optionally selects `0.0.0.0`, `127.0.0.1`, or `::1`. Set `SECRET_KEY_BASE`
to the same Rails secret used by the reference installation. `/up` is a basic process health
check. `GET /` follows the initial Rails setup/sign-in redirects, and the server opens an
existing `CAMPFIRE_STORAGE_PATH/db/production.sqlite3` without creating or migrating it.
Existing `$2a$`, `$2b$` and `$2y$` bcrypt digests can be verified; `GET /session/new` issues a
Rails-compatible encrypted session cookie and CSRF form token, and `POST /session` verifies
credentials, persists a Rails-schema session row, and returns a signed `session_token` cookie.
First-run setup creates the Rails-shaped Campfire account, administrator, “All Talk” open
room, and membership atomically, then starts a session. Authenticated users can navigate
membership-scoped room pages, create public rooms for all active users and private rooms
for selected members, rename/delete shared rooms, convert open/closed rooms while revising
memberships transactionally, create/reuse participant-scoped direct conversations, and post
sanitized Action Text messages (basic emphasis, lists, quotes, code, and safe links) stored in
Rails Action Text and FTS
tables. The authenticated `/users/me/sidebar` endpoint renders visible shared rooms and
direct conversations from Rails memberships, including unread indicators. Room pages expose
benchmark message IDs and link the native stylesheet, importmap, app icon, module, and SVG
asset endpoints checked by the shared HTTP preflight. The timed stylesheet route serves the
exact pinned Rails reset stylesheet; OxCaml's benchmark preflight compares the response bytes
against the reference asset. Text responses of at least 1 KiB negotiate gzip when requested,
matching the encoding threshold observed in the Express and Mojo HTTP workloads.
Authenticated `/autocompletable/users` responses return active, room-scoped Lexxy mentions
as HTML or JSON with Rails-compatible signed user GlobalIDs. Message create/edit verifies those
GlobalIDs before retaining Action Text User mention attachments; room, search, Cable and bot
message representations resolve them to escaped mention names. The native edit form is still a
plain textarea without a Lexxy mention picker, so editing a mention as plain text removes it.
Room message rendering also includes Rails-ordered boosts and attached message files. Image
attachments use Rails-signed Active Storage variation URLs and cached libvips representations
resized to a 1200×800 limit; video attachments use a cached first-frame WebP poster, and PDFs
use a cached first-page image preview. Image/video dimensions from Active Storage metadata drive
Rails-compatible inline-media wrappers; PNG, JPEG, GIF, WebP and BMP multipart uploads are
dimension-analyzed natively. TIFF, AVIF, HEIC/HEIF, JPEG 2000 and ICO uploads are signature-checked and
use libvips `vipsheader` for dimensions when its loader supports the format; existing image blobs
with missing dimensions use the same fallback. Video uploads use ffprobe and PDFs use Poppler's
pdfinfo for first-page dimensions. Other raster formats and per-format production upload coverage
remain incomplete; a current-image multipart TIFF upload was verified to persist and render as
40×20. Original bytes remain available through the lightbox URL. Active Storage
blob redirects use Rails-signed IDs, require an authenticated session and room membership (or an
account/user attachment), stream original bytes with byte-range support, and honor download
disposition. Message composer uploads accept one multipart file (up to 50 MiB),
write it into the Rails-compatible local storage layout, and create the matching
Active Storage blob before attaching it transactionally to the message. Attachment-only messages
are accepted like Rails, with the filename used as the visible plain-text fallback. The authenticated,
CSRF-protected Active Storage direct-upload endpoint also issues Rails-signed blob IDs and
short-lived disk upload tokens, verifies byte length and MD5 checksums before writing,
and leaves no partial object after a rejected checksum. Uploaded message types are
signature-checked for common image, audio, video, PDF, and plain-text formats; unknown or
mismatched data is stored as `application/octet-stream`.
The profile accepts common signature-checked raster avatar uploads and replaces the
Rails user attachment while purging only orphaned prior blobs. Resized representations
are generated as cached 512×512-limit WebP files for variable image attachments, matching
the Rails `:square` avatar variant. The production image includes libvips for processing;
source blobs stay in the original Rails storage layout, and derived variants are stored
separately from Active Storage files.
Room and sidebar responses expose Rails-signed Turbo stream names. `/cable` accepts
authenticated Action Cable WebSocket upgrades, validates signed room streams against current
membership, tracks connection counts, clears unread state on presence, and sends committed
room-message Turbo append/replace/remove events, user-scoped unread/read notifications,
global and per-user sidebar lifecycle broadcasts, and room-scoped typing start/stop events
through an in-process Eio event bus. Sidebar refresh beyond lifecycle changes and
reconnect/backpressure behavior remain incomplete.
Authors and administrators can edit or delete messages, with Action Text and FTS updated
transactionally. Edits preserve existing attachments; deleting a message detaches its
Active Storage attachments and purges unshared blobs/files. Join-code invitations create member accounts, grant access to existing public
rooms, and start a session. Room creation honors the account's administrator-only setting.
Search queries use the existing FTS index and retain the user's ten most recent
searches. Message history supports 40-message older/newer cursors, room permalinks centered
on a message, and Rails-compatible incremental refresh at `/rooms/:id/refresh?since=<milliseconds>`
for appended and edited messages. Members can hide a room or choose no, mention-only, or all-message notification
involvement per membership. The authenticated profile page updates a user's name, email, bio,
and optional bcrypt password while preserving the password when the field is left blank.
Direct conversations are created or reused by exact participant set and default to notifying
members about every message; any participant can delete a direct conversation. Room deletion
cleans text messages, boosts, search entries, and Active Storage associations transactionally,
then removes unshared blob files.
Signed avatar URLs verify Rails Active Record IDs and return a local stored image or an
initials SVG fallback. The profile can remove an avatar with orphaned Active Storage blob cleanup.
The public `/webmanifest` endpoint returns an install manifest named for the existing account,
and `/service-worker` serves the root-scoped push notification, badge, and notification-click
handler. Administrators can edit the account name and administrator-only room-creation setting
at `/account/edit`; this update preserves unknown Rails account settings and enforces CSRF and
administrator authorization. The same admin page lists active account members, supports
CSRF-protected role changes and join-code rotation, and deactivates members by anonymizing their
email, revoking sessions/searches/push subscriptions, and removing shared-room memberships while
preserving direct-conversation membership rows. Native database tests and the current production-
image preflight cover those persistence and cleanup rules. Profiles issue four-hour Rails-compatible signed transfer IDs; the public
transfer page submits them to establish a Rails session, and transfer updates reject tampered or
expired IDs. A production-image smoke against a disposable Rails-shaped database verified
login, transfer-page rendering, invalid-CSRF rejection, successful transfer, and use of the
newly transferred session. Profile transfer links are absolute request-origin URLs and have a QR-code link;
account settings expose the current join URL and its QR code to members. Crypto
tests compare generation against an independently generated OpenSSL vector. The first
`X-Forwarded-Proto` value controls absolute request URLs, Origin validation, and the `Secure`
attribute on issued/cleared cookies; plain HTTP requests keep cookies usable without that flag.
The proxy must overwrite forwarding headers. Lexxy-specific embeds and advanced formatting
remain unsupported.
Administrators can view user profiles and ban/unban other
non-bot users with Rails-compatible status values. Bans record distinct public session IPs,
revoke the user's sessions, synchronously delete authored messages/boosts/FTS/attachments, and
broadcast message removals; unbanning removes the IP-ban rows and restores active status. Rails
enqueues banned-message removal, while OxCaml currently performs it synchronously. Native tests
cover bans, IP filtering, session revocation, and orphan attachment cleanup. The current
production-image preflight verifies ban and unban. Account bot
management lists active bots and supports creation, name/webhook updates, Rails-style key
rotation, and deactivation. Creation grants access to existing open rooms; deactivation revokes
authentication, removes shared-room memberships and user state, and retains direct-conversation
membership rows as Rails does. Database tests and the current production-image preflight cover this
lifecycle. Public `/qr_code/:id` decodes a URL-safe Base64 path
segment and serves an SVG QR code with Rails-compatible one-year public caching. Push
subscriptions can be listed, created, refreshed, and deleted per user using the original
Rails schema. Native code now generates RFC 8291 P-256 ECDH and `aes128gcm` payload records;
the RFC's independently published ECDH/HKDF/AES-GCM vector passes. Native VAPID ES256 JWT
signing/verification and authorization-header construction are also covered by tests. The
owner-scoped test-notification route performs DNS-pinned HTTPS delivery when
`VAPID_PRIVATE_KEY`, `VAPID_PUBLIC_KEY`, and `VAPID_SUBJECT` are configured, deleting expired
subscriptions after 404/410 responses. The current production-image preflight verifies
subscription CSRF rejection, create/refresh/delete, and notification-route CSRF without contacting
a push provider. A production-image smoke verified valid-CSRF denial (404) for a subscription not
owned by the user. Message creation
now queues post-commit fan-out for user and bot messages; target selection covers disconnected,
visible members using Rails' precise 60-second connection cutoff, with `everything` involvement
and verified Action Text User mentions for
`mentions` involvement. Database targeting and independently generated signed GlobalID tests
pass. End-to-end fan-out and live provider delivery have not yet been verified. Push-subscription
creation validates its provider, HTTPS endpoint, and public DNS answers.
the original Rails schema. Room bot-key endpoints support membership-scoped text or multipart
file-attachment message creation, message list/edit/delete, and boost create/delete, using Rails-shaped
JSON and message Cable broadcasts. Bot boost create/delete now publish Rails-targeted append/remove
Turbo streams to subscribed room clients; the current production-image Cable preflight verifies
bot API/key lifecycle and bot boost delivery, including attachment persistence and orphan-blob
cleanup. Authenticated `POST /unfurl_link` pins requests to validated public DNS
addresses, follows at most ten revalidated redirects, caps downloaded HTML, extracts Open Graph
metadata, and checks image MIME types before returning an embed. Administrator custom styles can be edited at `/account/custom_styles/edit`, persist
in the Rails `accounts.custom_styles` column, and are injected into HTML pages. The database
tests cover persistence; the current production-image preflight verifies custom-style persistence
and HTML injection. Account logos now support
multipart upload/replacement, public PNG variants, stock-icon fallback, and administrator removal
using the existing Rails Active Storage association; the shared preflight covers that lifecycle.
The shared
preflight checks image and video representation authentication, room membership, and tampered variation rejection; direct smoke checks additionally
verified a PDF preview returns PNG and denies anonymous/tampered requests. Typing start/stop
payloads are checked as well. The current production-image WebSocket preflight verified message,
sidebar, typing, and read notifications, including rejection for a non-member. Logout
deletes the session row and clears the signed cookie. Login attempts are limited to 10 per IP in three
minutes using the separate `storage/db/jobs.sqlite3` database (or `JOBS_DATABASE_PATH`),
leaving the Rails database schema untouched. The app uses Cohttp's Eio HTTP/1 server; direct
TLS and HTTP/2 remain future work. A loopback raw-WebSocket smoke verified the shared
benchmark's six subscriptions (Presence, UnreadRooms, Heartbeat, and the three scraped Turbo
streams), and delivery of a marked message. The shared benchmark now runs a Cable message
create/edit/delete lifecycle requiring append/replace/remove frames; it passed against the
current host build. It also checks public/global and private/per-user sidebar room broadcasts
through create, rename, conversion, and delete. The current production image passed the full
shared HTTP preflight against the Rails-generated canonical default seed, including room
create/edit, message writes, attachment checks, and authenticated Cable mutations; it also
verified the updated sidebar markup.
The current production image passed `--validation-only --apps oxcaml --suites cable
--cable-tput-secs 0` twice each at 100, 500, and 1,000 clients, with six subscriptions per
client and all 30 paced messages delivered to every client. These are correctness/latency checks,
not saturation throughput results. Throughput is not measured on macOS; cross-implementation
preflight remains under investigation.

Current-source verification on 2026-10-08: the production Docker build completed the pinned
OxCaml release build and Dune suite. The root benchmark's production-image HTTP/Cable preflight
then passed twice against a disposable copy of the Rails-generated seed, covering room rendering,
messages, attachments and direct uploads, account/user administration, push subscriptions, bot
API/key lifecycle, and Cable message/sidebar/typing/read mutations. The room response contained
40 message IDs and 380,525 decoded bytes (14,708 gzip bytes); `data-message-updated-at` values
were present. The fixture source was left unchanged. This is functional preflight evidence, not a
cross-implementation performance result. The host is macOS, so comparative throughput was not
measured.

Run the native SQLite, bcrypt and Rails cookie/CSRF checks with
`opam exec --switch=5.2.0+ox -- dune runtest --profile=release`.

## Performance work

Rails-derived cryptographic keys are cached per domain, bounded to 16 entries for
the most recently used installation secret. PBKDF2 parameters, token formats and
per-token HMAC/GCM verification are unchanged; no authenticated responses are cached.
A five-round release microbenchmark on macOS ARM64 improved from 1,281 to 32,661
operations/second (25.5×) for cookie verification/decryption plus stream signing.
This is not an HTTP result and does not establish that this app is faster than Rust
or Go. See [measurement and reproduction notes](bench/README.md).
The manually dispatched **Rust head-to-head** workflow uses pinned Rust and shared
verification sources, equal Linux CPU allocations and audited writes. A passing
workflow result is still required before reporting an application-level comparison.

## Compatibility status

No full Rails compatibility or production-readiness claim is made yet. The authenticated
feature set is partial, and production-database verification remains incomplete. The current
source passed the shared HTTP/Cable preflight and two rounds of current-image Cable scale
validation against a Rails-generated canonical seed. A focused smoke against the current
production image and a disposable Rails-shaped database served the seeded JPEG avatar as a
512×512 WebP using a Rails-signed avatar token. The full current-image HTTP/Cable preflight has
now passed twice; other image formats and EXIF orientations remain unverified. These checks do
not establish comparative performance. Current-source attachment serving/auth/range,
multipart message/avatar upload, and direct-upload checksum behavior have native host
verification against disposable Rails-schema databases; load/throughput comparison remains
unverified.
See
[verification status](plans/contracts.md).

## License

MIT. The Rails reference remains a separate pinned submodule and is not modified. The
vendored bcrypt/Blowfish notices are in [`THIRD_PARTY_NOTICES.md`](THIRD_PARTY_NOTICES.md).
