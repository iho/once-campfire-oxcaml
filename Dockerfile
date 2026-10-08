FROM debian:trixie-slim AS build

ENV OPAMYES=1
RUN apt-get update && apt-get install -y --no-install-recommends \
      autoconf build-essential ca-certificates git gzip libsqlite3-dev libssl-dev opam pkg-config rsync zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src
RUN opam init --disable-sandboxing --bare --yes \
    && opam switch create 5.2.0+ox --empty \
      --repos ox=git+https://github.com/oxcaml/opam-repository.git#3416edee6b2416f5752ce101e3f7a1933e570a32,default \
    && opam repository add --switch=5.2.0+ox default https://opam.ocaml.org \
    && opam install --switch=5.2.0+ox --yes ocaml-variants.5.2.0+ox

COPY once_campfire_oxcaml.opam ./once_campfire_oxcaml.opam
RUN opam install --switch=5.2.0+ox --yes ./once_campfire_oxcaml.opam --deps-only

COPY . .
RUN opam exec --switch=5.2.0+ox -- dune build --profile=release @all \
    && opam exec --switch=5.2.0+ox -- dune runtest --profile=release

FROM debian:trixie-slim
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl ffmpeg libsqlite3-0 libssl3 libvips-tools poppler-utils qrencode zlib1g \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd --gid 1000 campfire \
    && useradd --uid 1000 --gid campfire --create-home --home-dir /rails campfire \
    && install -d -o campfire -g campfire /rails/storage

COPY --from=build --chown=campfire:campfire /src/_build/default/src/main.exe /usr/local/bin/campfire
COPY --from=build --chown=campfire:campfire /src/assets /rails/assets
ENV HTTP_PORT=80 CAMPFIRE_STORAGE_PATH=/rails/storage WEB_WORKERS=1
USER campfire
WORKDIR /rails
EXPOSE 80
ENTRYPOINT ["/usr/local/bin/campfire"]
