# Compatibility and verification

The immutable public Rails application is the behavior reference. The existing database,
storage files, bcrypt credentials and Rails cookie formats are the compatibility contract.

| Area | Evidence |
|---|---|
| Build and server | OxCaml 5.2+ox / Dune release build and native `dune runtest` pass. `/up` returns 200. `/` redirects to `/first_run` with empty storage and `/session/new` when an account/user is present. |
| Database and sessions | Native tests exercise Rails-shaped `accounts`, `users`, `rooms`, `memberships`, `messages`, Action Text, FTS, `searches`, and `sessions` against in-memory SQLite. First-run setup creates the singleton Campfire account, administrator, “All Talk” open room and membership transactionally. Room/message/search reads and writes are membership- or user-scoped; history has 40-message cursor pages and pivot-centered permalinks. An end-to-end local sign-in and setup against a disposable SQLite database created from the Express schema fixture returned 302 and inserted the expected rows; this is not evidence against a production database. Login limits are stored in the separate jobs database so the Rails database schema is not changed. |
| Password hashes | The isolated OpenBSD bcrypt verifier passes independently generated bcryptjs 3.0.3 `$2a$`, `$2b$`, and `$2y$` vectors, and rejects a wrong password and malformed hash. This does not yet verify against a production Rails database credential. |
| Rails cookies and CSRF | Independent Rails compatibility fixtures verify PBKDF2, signed cookies, AES-256-GCM encrypted cookies, raw/masked/per-form CSRF tokens, and session-cookie rotation. Manual loopback HTTP testing verified cookie issuance, a valid login, and rejection of an invalid CSRF token. |
| First run and sign-in | First-run setup creates the initial account/admin/open room/membership and starts a session; native tests verify the generated bcrypt digest and transaction contents. Sign-in persists the existing Rails session schema and sets Rails-compatible cookies; logout revokes that session. Avatar upload, forwarded-HTTPS cookie handling, most authenticated screens, and production-DB verification remain incomplete. |
| HTTP and Action Cable | Cohttp Eio HTTP/1 supports health, first-run form and creation, root-to-room redirects, membership-scoped room pages, plain-text message posting/rendering, FTS search with recent-search history, and sign-in/sign-out. Rich text, attachments, other authenticated routes, direct TLS, HTTP/2, and Action Cable are not implemented. |
| Production image | A clean Debian Trixie Docker build including `dune runtest` passes. Its non-root container returned 200 on `/up` and `/first_run`, 302 from `/` and `/session/new` to `/first_run` with empty storage. |

No parity, performance or production-readiness claims are made at this stage.
