# Performance measurements

## Rust head-to-head

The manually dispatched **Rust head-to-head** workflow builds both production
images from pinned sources and generates a fresh canonical Rails fixture using
the pinned shared verification repository. Four rounds alternate OxCaml/Rust and
Rust/OxCaml, restoring the same database and files for every application/round.
Both servers receive CPUs 0–1; the unchanged shared Rust load generator runs on
CPUs 2–3. Both keep their non-root image users and obtain group access only to the
disposable storage tree. No SQLite durability pragma is weakened.

Each of room, message-page, sidebar, search and POST workloads uses 16 clients,
a two-second warmup and an eight-second sample. The upstream route contracts
validate every response, and every acknowledged write is checked for its exact
ID, room, body and search-index entry. Failed preflight, partial rounds or failed
audits cannot produce a successful summary. Read caches use each app's defaults;
this compares implementations, not language runtimes in isolation.

Run on an otherwise idle Linux host after building the images and shared fixture:

```sh
ruby bench/comparison_test.rb
ruby bench/compare.rb --verification /path/to/once-campfire-verification \
  --seed /path/to/once-campfire-verification/fixtures/default \
  --rust-image campfire-rust:comparison --oxcaml-image campfire-oxcaml:comparison \
  --cpus 0-1 --client-cpus 2-3 --rounds 4 --duration 8 --concurrency 16
```

The output directory must not already exist. Raw samples, write receipts, image
identities, fixture hash and logs go to ignored `tmp/head-to-head`; disposable
databases stay under ignored `tmp/runtime`. No credentials are written to the
result metadata. CI uploads only the results, not fixture credentials/databases.
GitHub-hosted runners provide useful same-run comparisons, not a substitute for
repeated measurements on a dedicated performance host. The workflow is not itself
evidence that OxCaml beats Rust: it must finish successfully before interpreting
its per-route medians. Mixed-write cache-churn and Cable throughput are not covered
by this new head-to-head runner.

## Cryptography microbenchmark

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
