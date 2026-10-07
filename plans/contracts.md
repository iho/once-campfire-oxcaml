# Compatibility and verification

The immutable public Rails application is the behavior reference. The existing database,
storage files, bcrypt credentials and Rails cookie formats are the compatibility contract.

| Area | Evidence |
|---|---|
| Build and server | OxCaml 5.2+ox / Dune release build passes. A local HTTP request to `/up` returned 200 with the expected health page; other routes currently return 404. |
| Database and sessions | The existing `production.sqlite3` is opened with SQLite's full-mutex mode and `NO_CREATE`; there are no Campfire queries or writes yet. Sessions are not implemented. |
| HTTP and Action Cable | Not implemented. |
| Production image | A clean Debian Trixie Docker build passes. The non-root container returned HTTP 200 on `/up` and 404 for an unknown route. This only verifies process bring-up; Campfire behavior is not implemented. |

No parity, performance or production-readiness claims are made at this stage.
