#!/usr/bin/env bash
# Ask a question about this library's own source and get a line back.
# Needs TYPESAFE_API_KEY in the environment.
#   scripts/navigate.sh "where is a pool's name stamped into the question that draws on it?"
set -euo pipefail
cd "$(dirname "$0")/.."
: "${TYPESAFE_API_KEY:?set TYPESAFE_API_KEY}"
cabal build -v0 exe:jev-dsl-navigate
exec "$(cabal list-bin exe:jev-dsl-navigate)" "$@"
