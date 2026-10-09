# Cryptography microbenchmark

Run from the repository root using a release build:

```sh
mkdir -p tmp
opam exec --switch=5.2.0+ox -- dune exec --profile=release bench/crypto.exe -- \
  --iterations 1000 --rounds 5 > tmp/crypto.jsonl
```

Each operation verifies a signed session-token cookie, decrypts an encrypted
session cookie, and signs a Turbo stream name. Inputs are synthetic; every result
is checked. Token construction and 20 warmup operations are outside the timer.
Each sample starts after a full major GC. The benchmark is single-domain and uses
wall-clock elapsed time; JSONL contains every sample rather than only the best run.

On macOS ARM64, 2026-10-09, five 1,000-operation release samples gave:

| Implementation | Median operations/second |
| --- | ---: |
| Uncached derivation, `73ab3b19933abffcd321a3f7089a2cf430742db1` | 1,280.74 |
| Same source with domain-local derived-key cache | 32,660.58 |

This is a **25.5× improvement for this cryptography loop only**, not HTTP
throughput or a comparison with Rust/Go. Raw local measurements are kept in
ignored `tmp/crypto-before.jsonl` and `tmp/crypto-after.jsonl`. To reproduce the
baseline, run this same benchmark source and Dune stanza against the specified
uncached revision. It is not present in that older commit.

A Rust comparison still requires both release applications on the same host,
the same Rails fixture restored between runs, equal CPU budgets, the shared
load generator, repeated/interleaved runs, response validation and persisted-write
checks. Keep SQLite durability settings and equivalent response work unchanged.
The production image builds and passes a four-domain authenticated HTTP smoke
using curl inside the container. The full comparison runner still encounters a
Docker-access denial from its child process, and host load-generator connections
have not succeeded. No post-change HTTP speedup or Rust win has been established.
