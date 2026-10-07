# Compatibility and verification

The immutable public Rails application is the behavior reference. The existing database,
storage files, bcrypt credentials and Rails cookie formats are the compatibility contract.

| Area | Evidence |
|---|---|
| Build and server | OxCaml 5.2+ox / Dune release build and native `dune runtest` pass. `/up` returns 200. `/` redirects to `/first_run` with empty storage and `/session/new` when an account/user is present. |
| Database and sessions | Native tests exercise the account/user existence queries against in-memory SQLite. A disposable SQLite file created from the Express port's schema fixture opened with `NO_CREATE` and drove the existing-install route checks; this is not an independent production database. No session authentication or Campfire writes are implemented. |
| Password hashes | The isolated OpenBSD bcrypt verifier passes independently generated bcryptjs 3.0.3 `$2a$` and `$2y$` vectors, and rejects a wrong password and malformed hash. This does not yet verify against a production Rails database credential. |
| First run and sign-in | The empty-database setup page is an explicit placeholder; sign-in returns 501 when users exist. Account creation, Rails cookies and sessions are not implemented. |
| HTTP and Action Cable | Not implemented. |
| Production image | A clean Debian Trixie Docker build including `dune runtest` passes. Its non-root container returned 200 on `/up` and `/first_run`, 302 from `/` and `/session/new` to `/first_run` with empty storage. |

No parity, performance or production-readiness claims are made at this stage.
