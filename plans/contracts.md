# Compatibility and verification

The immutable public Rails application is the behavior reference. The existing database,
storage files, bcrypt credentials and Rails cookie formats are the compatibility contract.

| Area | Evidence |
|---|---|
| Build and server | OxCaml 5.2+ox / Dune release build and native `dune runtest` pass. `/up` returns 200. `/` redirects to `/first_run` with empty storage and `/session/new` when an account/user is present. |
| Installability | `/webmanifest` returns account-named JSON with install shortcuts and app icons; `/service-worker` returns the root-scoped push, badge, and notification-click worker. Both routes were fetched from the rebuilt native server on 2026-10-07. |
| Account settings | `/account/edit` is membership-authenticated, and only administrators can update account name/room-creation restriction. The update preserves unknown JSON settings and requires Rails-compatible CSRF; Dune database tests cover persistence and unknown-setting preservation. The shared preflight is configured to check the read page, invalid-CSRF rejection, persistence, and restoration. A production-image smoke on a disposable Rails-shaped DB verified 200 page, 422 invalid-CSRF denial, 302 administrator update/restore, and 403 member denial. |
| Account administration | The account page lists active users; admin-only CSRF-protected endpoints rotate the join code, change member/admin roles, and deactivate active users. Deactivation anonymizes email, disables future login, revokes sessions/searches/push subscriptions, deletes shared-room memberships, and preserves direct-conversation memberships. Native SQLite tests and the current two-round production-image preflight verify these state changes. |
| Session transfer | Profile pages issue Rails Active Record signed IDs with the `user/transfer` purpose and a four-hour expiry. Public transfer pages submit a CSRF-protected `PUT`; successful transfers create a row in the original `sessions` table and issue Rails-compatible session cookies, while invalid/expired IDs are rejected. An independently generated OpenSSL vector checks the token format and signature. On the rebuilt production image and disposable Rails-shaped database, authenticated login, transfer-page rendering, invalid-CSRF rejection (422), valid transfer (302), and access using the transferred session (200) all passed. Canonical-seed shared-preflight coverage remains unverified. |
| User bans | Admins may ban/unban other non-bot users. Ban status, distinct valid public IP records, and session revocation are transactional in the Rails schema. Authored messages, boosts, Action Text, FTS rows and orphaned attachment blobs are then removed and live room clients receive Turbo remove events. Rails delegates content removal to a background job; OxCaml currently removes it synchronously. Dune tests and the current two-round production-image preflight cover invalid-CSRF rejection, ban/unban, public-IP persistence, session revocation, content/FTS cleanup and orphan-blob cleanup. |
| Database and sessions | Native tests exercise Rails-shaped `accounts`, `users`, `rooms`, `memberships`, `messages`, Action Text, FTS, `searches`, and `sessions` against in-memory SQLite. First-run setup creates the singleton Campfire account, administrator, “All Talk” open room and membership transactionally. Join-by-code signup creates a member and grants access to all existing open rooms. Public-room creation grants membership to all active users; private-room creation grants the creator and selected users, both transactionally. Direct-room creation atomically reuses a room only for an exact participant set and assigns Rails' `everything` involvement by default. Shared-room edits rename rooms or convert between open/closed transactionally, reconcile memberships, enforce creator/admin rights and refuse direct-room promotion. Room deletion enforces shared-room creator/admin access or direct-room participant access and transactionally removes dependent text, boosts, memberships, FTS and attachment rows; unshared Active Storage blobs are purged, while shared blobs remain. Profile updates preserve Rails user fields, change bcrypt credentials only when a replacement password is supplied, and reject duplicate email addresses atomically. Membership notification preferences use the Rails `involvement` enum values and update only that member's room membership. The sidebar query filters invisible memberships, sorts shared rooms by name and exposes each membership's unread state. Message edits update Action Text and FTS atomically while preserving separate Active Storage attachments; deletion removes text, attachment rows, and orphaned blob records transactionally and enforces creator/admin authorization. Request authorization honors the account's administrator-only setting. Room/message/search reads and writes are membership- or user-scoped; history has 40-message cursor pages and pivot-centered permalinks. The incremental room-refresh query separates messages created since a millisecond cursor from older messages edited since that cursor, matching Rails' first/last 40-message windows. Manual end-to-end checks against disposable SQLite databases initialized from the Express schema verified setup, authenticated public/private room creation, invitation signup/session cookies and public-room membership, invalid invitation rejection, duplicate-email redirect, message create/edit/delete and non-author edit denial; a disposable Rails-schema HTTP smoke also verifies incremental refresh Turbo markup, Rails-style malformed-cursor coercion, anonymous redirect, and missing-room 404. These checks do not establish behavior against a production database. Login limits are stored in the separate jobs database so the Rails database schema is not changed. |
| Password hashes | The isolated OpenBSD bcrypt verifier passes independently generated bcryptjs 3.0.3 `$2a$`, `$2b$`, and `$2y$` vectors, and rejects a wrong password and malformed hash. The current production-image preflight also signs in against the Rails-generated seeded credential. |
| Rails cookies, signed IDs, and CSRF | Independent Rails compatibility fixtures verify PBKDF2, signed cookies, AES-256-GCM encrypted cookies, signed `User#avatar_token` IDs, Active Storage blob signed IDs, Turbo stream-name signatures, raw/masked/per-form CSRF tokens, and session-cookie rotation. Manual loopback HTTP testing verified cookie issuance, a valid login, and rejection of an invalid CSRF token. |
| First run and sign-in | First-run setup creates the initial account/admin/open room/membership and starts a session; native tests verify the generated bcrypt digest and transaction contents. Sign-in persists the existing Rails session schema and sets Rails-compatible cookies; logout revokes that session. Session cookies include `Secure` when the first `X-Forwarded-Proto` value is `https`, and Origin validation uses that same scheme; native tests cover the cookie flag, and production-image HTTP smoke verified that forwarded HTTPS issues a Secure cookie, a matching HTTPS Origin passes CSRF validation to field validation, and plain HTTP omits Secure. Name/email/bio/password profile editing and CSRF-protected avatar removal with orphan-blob cleanup are native database-tested. Multipart avatar upload/replacement passed authenticated HTTP smoke against a disposable Rails-schema database, including persisted User/avatar association. The current two-round production-image preflight covers the benchmarked authenticated flows; other Rails authenticated screens/routes and verification against a live production database remain incomplete. |
| HTTP and Action Cable | Cohttp Eio HTTP/1 supports health, first-run form and creation, join-code signup, root-to-room redirects, membership-scoped room pages and sidebar, public/private room creation, editing and deletion, open/closed conversion, singleton direct-conversation creation/deletion, per-membership notification preferences, own-profile name/email/bio/password editing, authenticated room-scoped Lexxy user autocomplete in HTML and JSON with signed GlobalIDs, sanitized Action Text message create/edit/delete with basic emphasis, list, quote, code and safe-link tags, FTS search with recent-search history, sign-in/sign-out, and signed avatar reads (Rails-compatible cached 512×512-limit WebP variants for variable raster attachments, or initials SVG fallback). Image, video-poster, and PDF-page message previews use Rails-signed Active Storage variations. Host HTTP smoke on a disposable Rails-shaped database verified PDF preview PNG output (800×800), anonymous denial, and tampered variation denial; earlier shared preflight covers image/video authorization and signature rejection. Measured parity against Rails remains unverified. The shared benchmark checks autocomplete HTML and JSON for OxCaml. Room pages render ordered boosts and message attachments; boost create/delete supports Rails-style CSRF, booster ownership, and Turbo responses. Bot-key-authenticated room JSON message list/create/edit/delete use active bot credentials and room membership, return message JSON and pagination metadata, update FTS, and publish Cable message events; database authentication tests pass, while production-image route verification is pending. Account bot management supports administrator-only list/create/edit/deactivate and key rotation; creation enrolls bots in existing open rooms, and deactivation revokes bot access, removes shared memberships and user state, while retaining direct memberships. Isolated SQLite database tests pass; the production-image route lifecycle check is in the shared benchmark preflight and awaits a successful full run. Bot message attachments accept multipart uploads; current production-image lifecycle verification is still pending. Bot boost create/delete publishes append/remove Turbo stream events to subscribed room clients, and the shared Cable preflight now checks this delivery; production-image verification is pending. Message and profile avatar multipart uploads persist local Active Storage blobs; authenticated HTTP smoke against a disposable Rails-schema DB verified both multipart message attachment and avatar association. Database tests cover blob metadata, avatar replacement, and orphan cleanup. The authenticated, CSRF-protected direct-upload endpoint returns signed blob IDs and short-lived disk tokens, checks MD5/byte length before writing, and passed host HTTP lifecycle smoke for upload, attachment, exact-byte download, and mismatched-checksum cleanup. The same check passed in the shared benchmark against the current production image and canonical Rails seed. Authenticated Active Storage blob redirects enforce membership/owner access, serve original bytes and support byte ranges/download disposition using Rails-compatible inline/binary rules; signed existing blobs can also be attached to a new message. Room and search result pages expose benchmark message IDs; room and sidebar responses include Rails-signed stream names; native endpoints serve the shared benchmark's CSS, JS, SVG and PNG assets from a compact importmap. `/cable` implements the Action Cable JSON WebSocket handshake, signed room-message authorization, membership presence/connect/disconnect/refresh and unread clearing, committed-message Turbo append/replace/remove events, and user-scoped read/unread notifications via an in-process Eio event bus. The current source's full production-image HTTP/Cable preflight passed against the Rails-generated canonical seed, including message append/edit/delete frames and public/private sidebar broadcasts. Earlier shared Cable validation passed twice each at 100, 500 and 1,000 clients, with six subscriptions per client and all 30 paced messages delivered to every client (zero saturation time). An initial current-image `--validation-only` run failed in unrelated HTTP preflight, then a Cable-only run completed twice at all three client counts. Sidebar refresh semantics beyond room lifecycle broadcasts, saturation throughput, and cross-implementation validation remain unverified. Lexxy-specific embeds/advanced formatting, the remaining Rails authenticated routes, direct TLS and HTTP/2 are not implemented. |
| Production image | Clean Debian Trixie Docker builds including `dune runtest` pass. Its non-root container returned 200 on `/up` and `/first_run`, 302 from `/` and `/session/new` to `/first_run` with empty storage, and passed authenticated public/private room creation, invitation signup, message create/edit/delete and non-author denial checks against disposable benchmark-schema databases. Authenticated HTTP smoke tests against the production image also verified both open→closed and closed→open room edit submissions return 302; the final name/type/membership row was read back from SQLite. Direct-room HTTP testing confirmed the participant picker, CSRF-protected creation, exact-set reuse on repeat submission, a participant-scoped page, and Rails `Rooms::Direct`/`everything` rows in the original SQLite schema. The membership-involvement flow was additionally exercised against the production image: first-run setup returned 302, the authenticated preferences page returned 200, a CSRF-protected update returned 302, and the original Rails-schema database persisted `everything`. Profile HTTP checks against the production image verified the profile page/update, successful login after leaving password blank, and successful login with the replacement password; persisted name/email/bio were read from SQLite. A rebuilt production image also returned 200 for authenticated `/users/me/sidebar` with `#shared_rooms` and the room, returned a room page containing `data-message-id`, a stylesheet link and Rails-signed room stream, exposed two signed streams in the sidebar, served the stylesheet as `text/css`, returned a >100-byte SVG avatar fallback, and streamed a mounted `image/png` avatar blob byte-for-byte with a valid Rails-signed user token. Most recently, the rebuilt current-source image passed the entire shared authenticated HTTP/Cable preflight on the Rails-generated canonical seed, covering visible sidebar room IDs, open/closed room create/edit flows, direct-room behavior, attachment checks, message mutations, logout, and Cable message/sidebar broadcasts. Each smoke container was stopped afterward. |

No full parity, cross-implementation performance or production-readiness claims are made at this stage.

Current worktree update (2026-10-07): room-scoped `TypingNotificationsChannel` start/stop
broadcasts now authorize against current room membership and include the Rails user id/name
payload. Native event-bus tests pass. Host-native WebSocket smoke against the Rails seed
verified typing delivery, non-member rejection, and a user-scoped read-room notification; the
production-image WebSocket preflight has not yet run.

The current-worktree notes below supersede the earlier route-inventory limitations in the table.

Current-source verification update (2026-10-08): the production OxCaml image rebuilt successfully,
including the pinned OxCaml release build and native Dune test suite. A one-shot container used the
image's default port 80 and an isolated SQLite backup of the Rails-shaped benchmark fixture; a real
sign-in returned 302 and `GET /rooms/486777696/messages` returned HTTP 200 with message fragments,
IDs, and no full room layout. The fixture source was left unchanged. This is focused production-image
evidence only: the full canonical-seed preflight has not been rerun after the message-collection
route correction, and no cross-implementation or throughput result is established by it.

Current-source verification update (2026-10-08): the production Docker build passed the pinned
OxCaml release build and native Dune suite. An in-container HTTP smoke then verified `/up`, gzip
negotiation and byte-exact bundled CSS, the standalone webmanifest, and the service-worker
notification handler. The room page now uses the Rails application layout landmarks (skip link,
`#nav`, `#main-content`, `#footer`, `#sidebar`, lightbox and app logo), responsive metadata, and
the shared stylesheet/importmap; message fragments now include Rails-like action/reaction markup,
avatar links, and message/presentation data attributes. These render changes have compile/test
coverage but have not yet been compared against canonical-seed Rails HTML. Full rendered-page
parity and comparative performance remain unverified.

Current-source seeded room-render smoke (2026-10-08): copied the Rails-generated benchmark seed
to an ignored disposable directory, started the current production image with that SQLite/storage
fixture, signed in as its seeded administrator, and requested `/rooms/486777696`. The page returned
200 and 348,925 decoded bytes (22,610 gzip wire bytes) with 40 message IDs, 320 quick-reaction
forms, the Rails layout and message wrappers, client-keyed edit/boost frames, and the required
reply/copy/edit controls. This exercise caught and fixed missing gzip negotiation on `GET /rooms/:id`.
The seed copy was isolated from its source. This focused request confirms the renderer executes
against actual Rails data, but it is not the complete shared preflight, HTML equality test, or
throughput comparison.

Latest-source verification (2026-10-08): the production Docker build completed the pinned OxCaml
release build and all Dune tests after message timestamps, form parsing, and bot API auth changes.
The root benchmark's HTTP/Cable preflight then passed two rounds against a disposable copy of the
Rails-generated seed. It verified the room response at 380,525 decoded bytes (14,708 gzip bytes),
40 message IDs, and message updated-at attributes, along with attachment/direct-upload lifecycles,
account and user administration, bot API/key lifecycle, push-subscription forms, and Cable message,
sidebar, typing, and read notifications. The source seed was unchanged; its disposable label file
was augmented with IDs for existing image/video/file fixtures. This is functional route/protocol
verification, not exact HTML equivalence or comparative throughput. The root Express suite passed
52/52, `ruby -c bench/compare.rb` passed, and `git diff --check` was clean.

Current worktree update (2026-10-07): bot-key-authenticated room JSON message list/create/edit/delete
and nested boost create/delete routes now use active bot credentials and require room membership.
Message CRUD publishes the same Cable append/replace/remove events as browser message writes;
boost create/delete endpoints broadcast append/remove events to subscribed room clients. Database tests
cover valid, incorrect, non-bot, and deactivated bot-key authentication. The shared production-image preflight exercises
message/boost create and delete plus JSON identity, but could not run in this environment because
the Docker API denied the benchmark runner's child process.

Current worktree update (2026-10-07): bot message creation also accepts Rails-style multipart
`attachment` uploads, persists the original `Message`/`attachment` Active Storage association,
uses the filename for attachment-only JSON plain text, and removes orphaned blobs when the message
is deleted. The shared image preflight checks the full attachment lifecycle; it remains unrun on
the canonical Rails seed.

Current worktree update: `/account/custom_styles/edit` and the Rails `PATCH /account/custom_styles`
flow now provide administrator-only custom CSS editing. Styles persist in `accounts.custom_styles`
and are injected into the HTML `<head>` with Rails' reload-tracked style tag. In-memory database
tests and the production image build pass; dedicated production HTTP route verification is pending.

Current worktree update: public `GET /account/logo` now serves Rails Active Storage logos as
cached PNG variants with stock 192px/512px icon fallbacks. Account settings upload/replacement
uses the existing `Account`/`logo` attachment, and admin-only `DELETE /account/logo` detaches and
purges only orphaned blob data. SQLite tests cover attachment lookup, replacement, destruction,
and account timestamp updates. The shared production preflight exercises multipart upload, both
variant sizes, deletion, and fallback; it has not been run against the current image.

Current worktree update: public `GET /qr_code/:id` decodes Rails-compatible URL-safe Base64 and
returns a black-on-white SVG QR code with one-year public caching. The production runtime installs
the native `qrencode` utility; the shared preflight checks a generated SVG response and cache header.
The route was verified against a rebuilt production container without a database or session.
Account settings now expose the absolute join URL and a QR link; profile transfer URLs are
absolute and link to their QR representation. Database tests check the join-code lookup, and the
production image smoke against a disposable Rails-shaped database verified first-run (302),
profile/account pages (200), absolute URL values, and QR links matching those exact values. The
shared canonical-seed preflight also checks both links; the full benchmark preflight remains
unverified against the current worktree.

Current worktree update: push-subscription listing and owner-scoped creation/deletion now operate
on the existing Rails table. Re-registration touches `updated_at` without duplicating or changing
the stored user agent. Endpoint validation follows Rails' HTTPS/443 and push-provider allowlist and
rejects non-public DNS answers. Focused validator and database CRUD tests pass in the Docker test
build. Native RFC 8291 payload encryption now matches the independently published ECDH, HKDF,
and AES-GCM vector, including the `aes128gcm` single-record framing. VAPID ES256 JWT signing and
verification, key-pair consistency, and authorization-header generation are tested. The
owner-scoped test-notification endpoint checks CSRF and subscription ownership, sends a
DNS-pinned HTTPS request when `VAPID_PRIVATE_KEY`, `VAPID_PUBLIC_KEY`, and `VAPID_SUBJECT` are
configured, and removes expired subscriptions after 404/410 responses. Shared preflight coverage
checks invalid-CSRF denial without external delivery. A production-image smoke verified login,
invalid-CSRF rejection (422), and valid-CSRF denial (404) for a nonexistent/non-owned
subscription. Authenticated-user and bot message creation now queue post-commit notifications for
visible room members using the subsecond-precise 60-second disconnect cutoff: all-message
preferences receive every message and mention-only preferences receive notifications only for
verified Action Text User GlobalIDs. SQLite targeting/boundary tests and independent Rails-format
GlobalID verification tests pass. Full message-to-provider
delivery and live push-provider responses remain unverified.

The full current-source shared preflight used the production image built from this worktree and
the Rails-generated default seed. It completed successfully on 2026-10-07, including authenticated
room and message routes plus Cable message and sidebar mutation checks. A later Cable-only
`--validation-only` run also passed twice at each of 100, 500 and 1,000 connections, with all paced
messages delivered. Validation-only now skips unrelated HTTP mutation checks while retaining
login, scrape, and database-integrity checks. These results are functional correctness/latency
evidence, not a throughput comparison or production-database verification.

Benchmark transfer encoding: OxCaml now negotiates gzip for text responses of at least 1 KiB
when clients request it, using a native zlib binding. Native tests round-trip the generated stream through the system
gzip decoder and cover `Accept-Encoding` quality/wildcard rules. The shared preflight asserts gzip
for each large timed HTML/CSS route and verifies the decoded CSS against Rails. Production-image
encoding and comparative throughput still require a rerun.

Avatar parity: Rails `User::Avatar` serves its `:square` variant as WebP resized to a 512×512
limit. OxCaml now generates that derived representation with libvips, caches it outside the
Active Storage `files/` tree, and removes it when the source blob is purged. On 2026-10-08, the
current production image served the seeded JPEG avatar from a disposable Rails-shaped database
as a 512×512 WebP using a Rails-signed avatar token; the output dimensions were probed inside
the container. The complete canonical preflight has not been rerun since switching to libvips.
Other Rails-variable image formats and every EXIF-orientation case remain unverified.

Message attachment previews now use Rails-signed Active Storage variations: raster images
are resized to 1200×800, videos render a cached first-frame WebP poster, and PDFs render a
cached first-page image. The original download remains separate. The production image includes
FFmpeg, Poppler, and libvips and built successfully with release tests; host HTTP smoke verified
the authenticated WebP video poster and authenticated PNG PDF preview. Authenticated image/video/PDF
preview verification inside the production container is still pending.

Message preview sizing: attachment queries now read Active Storage width/height metadata and
render Rails' 1200×800 inline-media constraints and intrinsic image dimensions. New PNG, JPEG,
GIF, WebP and BMP uploads get dimensions from bounded header parsers. TIFF, AVIF, HEIC/HEIF, JPEG 2000
and ICO upload signatures are validated before their image MIME types are retained; image formats
not handled by the native parser fall back to libvips `vipsheader` for new uploads and existing
local blobs when its loader supports them. Tests cover the signatures and parser output. The
production image's `vipsheader` read an actual TIFF fixture (40×20), and a current-image multipart
upload into an isolated copy of the Rails-shaped database persisted as `image/tiff` and rendered
with those intrinsic dimensions. Video uploads use ffprobe; PDFs use `pdfinfo` for first-page point
dimensions when the process manager is available. Production upload coverage for AVIF, HEIC, JPEG
2000, ICO, other raster formats, and PDF remains incomplete, so unsupported or failed loaders
still leave dimensions absent.

Action Text mention attachables: message create/edit handlers preserve only signed `User` GlobalIDs
when present in the submitted Action Text body; room pages, search results, Cable broadcasts, bot
JSON and push plain text resolve those IDs to escaped user names. Tests cover independently
generated Rails GlobalID verification, duplicate/invalid attachment handling, sanitization, and
persisted SQLite rich text. The native edit UI remains a plain textarea without a Lexxy mention
picker, so editing a mention as plain text will remove its attachment. Other Lexxy embeds and
formatting controls remain unsupported.

Message collection route parity: Rails renders `GET /rooms/:id/messages` as only the
message partials and returns 204 for an empty page. OxCaml now follows that response shape
instead of returning the full room layout; the shared benchmark preflight now asserts both
the partial-only response and the empty-page 204 for OxCaml. Production-image HTTP smoke
against a disposable Rails-shaped SQLite database verified a populated 200 partial response
with the message ID/body and no room layout, plus a 204 empty response. The full
canonical-seed preflight has not been rerun after this change.

Current-source message-upload verification (2026-10-08): Rails' `Message::Attachment` supports a
blank body with an attachment and uses the filename as plain text. OxCaml now allows a blank body
only when a valid multipart or signed attachment is present. A production-image multipart request
against an isolated Rails-shaped database returned 200; the message rendered its filename and
512×512 image dimensions. The shared OxCaml preflight now checks this attachment-only case and
persists/validates the attachment metadata.

Browser Cable client verification (2026-10-08): a dependency-free Node test executes the shipped
`assets/application.js` against mocked browser DOM/WebSocket interfaces. It checks the signed and
room-scoped subscriptions, Turbo stream application, unread/read sidebar updates, typing
indication and composer start/stop messages, and presence teardown on navigation. CI runs this
test alongside Dune tests; Node is a test-only prerequisite and is not included in the production
image. This is protocol/DOM simulation, not a real-browser rendering or accessibility test.

Derived-key caching (2026-10-09): Rails PBKDF2-SHA256 keys now use bounded domain-local
storage (at most 16 entries, each at most 64 key bytes, 128 salt bytes, and a shared
installation secret of at most 1,024 bytes per domain). A secret change prevents reuse
of old-secret entries; oversized requests bypass and clear the cache. Derivation
still uses 1,000 iterations. HMAC/GCM verification, expiry, CSRF and authorization
checks are not skipped. Tests cover independent Node-generated key vectors, key
length/salt/secret separation, eviction, secret rotation and concurrent domains;
the native release suite and production Docker build with release tests passed.
The existing C-FFI portability annotations are not strengthened by this change.

The focused cryptography loop improved 25.5× in five local release samples
(median 1,280.74 → 32,660.58 operations/second); see `bench/README.md` for methodology.
No current-image throughput comparison completed. On retry, direct Docker access
worked: the production image (config SHA-256
`fad2fa93735438f95e09db4ff17b0afde919b31b0997a6af7ca4e1a4615db25c`) passed a
four-domain authenticated smoke using curl inside the container against an isolated
Rails-shaped fixture. Login returned 302; room, sidebar, profile and search pages
succeeded; invalid-CSRF writes returned 422 and a forged cookie redirected to login.
A full `user_id-token` bot key created a message with HTTP 201; host SQLite queries
verified creator identity, rich text, the FTS row and database integrity. The fresh
production rebuild also passed release tests, including oversized-cache-input tests.
The shared runner's Docker child process and host load-generator connections remain
blocked, so full canonical-seed preflight and a same-host Rust comparison are still
pending. SQLite durability, schema and response semantics were not changed to obtain
the microbenchmark result.
