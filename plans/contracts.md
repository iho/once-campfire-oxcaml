# Compatibility and verification

The immutable public Rails application is the behavior reference. The existing database,
storage files, bcrypt credentials and Rails cookie formats are the compatibility contract.

| Area | Evidence |
|---|---|
| Build and server | OxCaml 5.2+ox / Dune release build and native `dune runtest` pass. `/up` returns 200. `/` redirects to `/first_run` with empty storage and `/session/new` when an account/user is present. |
| Database and sessions | Native tests exercise the Rails `users` and `sessions` column contract with in-memory SQLite. An end-to-end local sign-in against a disposable SQLite database created from the Express port's schema fixture returned 302 and inserted a session row; this is not evidence against a production database. No Campfire writes are implemented. Login limits are stored in the separate jobs database so the Rails schema is not changed. |
| Password hashes | The isolated OpenBSD bcrypt verifier passes independently generated bcryptjs 3.0.3 `$2a$`, `$2b$`, and `$2y$` vectors, and rejects a wrong password and malformed hash. This does not yet verify against a production Rails database credential. |
| Rails cookies and CSRF | Independent Rails compatibility fixtures verify PBKDF2, signed cookies, AES-256-GCM encrypted cookies, raw/masked/per-form CSRF tokens, and session-cookie rotation. Manual loopback HTTP testing verified cookie issuance, a valid login, and rejection of an invalid CSRF token. |
| First run and sign-in | Sign-in persists the existing Rails session schema and sets Rails-compatible cookies; logout revokes that session. Account creation, forwarded-HTTPS cookie handling, and authenticated Campfire screens remain unimplemented. The setup page is still a placeholder. |
| HTTP and Action Cable | Cohttp Eio HTTP/1 supports health, setup placeholder, root redirects, and the sign-in/sign-out flow. Other authenticated routes, direct TLS, HTTP/2, and Action Cable are not implemented. |
| Production image | A clean Debian Trixie Docker build including `dune runtest` passes. Its non-root container returned 200 on `/up` and `/first_run`, 302 from `/` and `/session/new` to `/first_run` with empty storage. |

No parity, performance or production-readiness claims are made at this stage.
