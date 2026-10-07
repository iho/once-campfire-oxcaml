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

Install [OxCaml](https://oxcaml.org/get-oxcaml/) and Dune, then run:

```sh
opam switch create 5.2.0+ox \
  --empty \
  --repos ox=git+https://github.com/oxcaml/opam-repository.git#3416edee6b2416f5752ce101e3f7a1933e570a32,default
eval "$(opam env --switch=5.2.0+ox)"
opam install ./once_campfire_oxcaml.opam --deps-only
opam exec --switch=5.2.0+ox -- dune build --profile=release
opam exec --switch=5.2.0+ox -- dune exec campfire-oxcaml
```

The server binds `0.0.0.0:3000`; `HTTP_PORT` selects another port. `/up` is a basic process
health check. `GET /` follows the initial Rails setup/sign-in redirects, and the server
opens an existing `CAMPFIRE_STORAGE_PATH/db/production.sqlite3` without creating or migrating
it. First-run account creation and sign-in are not implemented; sign-in currently returns
501. No authenticated Campfire routes are implemented yet. The app uses Cohttp's Eio HTTP/1
server; direct TLS, HTTP/2 and Action Cable remain future work.

Run the native SQLite checks with `opam exec --switch=5.2.0+ox -- dune runtest --profile=release`.

## Compatibility status

No Rails compatibility or production-readiness claim is made yet. This is an early port:
account/session authentication and the room/message feature set remain to be implemented
against the pinned Rails reference and real SQLite fixtures. See [verification status](plans/contracts.md).

## License

MIT. The Rails reference remains a separate pinned submodule and is not modified.
