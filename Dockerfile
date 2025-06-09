# docker build -t haskell-postgres .
# docker run -it --rm --privileged -p 1000:8080 --name haskel-dev -v ${PWD}:/workdir -w /workdir haskell-postgres bash

FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y \
        libpq-dev \
        postgresql-server-dev-all \
        pkg-config \
        postgresql-client \
        build-essential \
        zlib1g-dev \
        libssl-dev \
        curl \
        wget \
        ca-certificates \
        gnupg \
        software-properties-common \
        libtinfo-dev \
        libgmp-dev \
        libnuma-dev \
    && rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH="/root/.ghcup/bin:${PATH}"

RUN ghcup install ghc 9.8.4 && \
    ghcup set ghc 9.8.4 && \
    ghcup install cabal latest && \
    ghcup install stack latest

RUN cabal update

WORKDIR /workdir