# once-campfire-oxcaml

OxCaml implementation of ONCE Campfire. The pinned public Rails application is the
behavior reference; preserve its SQLite schema, storage layout, bcrypt credentials and
Rails signing/encryption formats. Never edit `reference/`.

Keep production runtime code in OxCaml. Keep generated fixtures, databases, benchmark
results and other raw artifacts in ignored `tmp/`. Verify compatibility against actual
databases and independently generated vectors; do not infer production parity from unit
tests alone. Document deliberate differences and verification limits in `README.md` and
`plans/contracts.md`.
