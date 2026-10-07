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

Install [OxCaml](https://oxcaml.org/get-oxcaml/), Dune, and OpenSSL 3 development
headers (`brew install openssl@3` on macOS), then run:

```sh
opam switch create 5.2.0+ox \
  --empty \
  --repos ox=git+https://github.com/oxcaml/opam-repository.git#3416edee6b2416f5752ce101e3f7a1933e570a32,default
eval "$(opam env --switch=5.2.0+ox)"
opam install ./once_campfire_oxcaml.opam --deps-only
opam exec --switch=5.2.0+ox -- dune build --profile=release
opam exec --switch=5.2.0+ox -- dune exec campfire-oxcaml
```

The server binds `0.0.0.0:3000`; `HTTP_PORT` selects another port. Set `SECRET_KEY_BASE`
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
plain-text messages stored in Rails Action Text and FTS
tables. Authors and administrators can edit or delete text messages, with Action Text and
FTS updated transactionally; edit/delete requests for messages with attachments are rejected
until attachment storage is supported. Join-code invitations create member accounts, grant access to existing public
rooms, and start a session. Room creation honors the account's administrator-only setting.
Search queries use the existing FTS index and retain the user's ten most recent
searches. Message history supports 40-message older/newer cursors and room permalinks centered
on a message. Members can hide a room or choose no, mention-only, or all-message notification
involvement per membership. The authenticated profile page updates a user's name, email, bio,
and optional bcrypt password while preserving the password when the field is left blank.
Direct conversations are created or reused by exact participant set and default to notifying
members about every message; any participant can delete a direct conversation. Room deletion
cleans text messages, boosts and search entries transactionally, but is rejected when the room
contains attachments because storage cleanup is not implemented.
Avatar upload, rich formatting, attachments, real-time delivery, and forwarded-HTTPS
cookie handling are not implemented yet. Logout
deletes the session row and clears the signed cookie. Login attempts are limited to 10 per IP in three
minutes using the separate `storage/db/jobs.sqlite3` database (or `JOBS_DATABASE_PATH`),
leaving the Rails database schema untouched. The app uses Cohttp's Eio HTTP/1 server; direct
TLS, HTTP/2 and Action Cable remain future work.

Run the native SQLite, bcrypt and Rails cookie/CSRF checks with
`opam exec --switch=5.2.0+ox -- dune runtest --profile=release`.

## Compatibility status

No Rails compatibility or production-readiness claim is made yet. This is an early port:
account/session authentication and the authenticated room/message feature set are partial,
and production-database verification remains incomplete. See
[verification status](plans/contracts.md).

## License

MIT. The Rails reference remains a separate pinned submodule and is not modified. The
vendored bcrypt/Blowfish notices are in [`THIRD_PARTY_NOTICES.md`](THIRD_PARTY_NOTICES.md).
