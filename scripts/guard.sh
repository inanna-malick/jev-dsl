#!/usr/bin/env bash
# The gate at Greyhaven: a hand-written guard script that Jev traverses.
#   scripts/guard.sh --script     print the script, no network
#   scripts/guard.sh              play it; transport.sh resolves the API key
set -euo pipefail
cd "$(dirname "$0")/.."
cabal build -v0 exe:jev-dsl-guard
exec "$(cabal list-bin exe:jev-dsl-guard)" "$@"
