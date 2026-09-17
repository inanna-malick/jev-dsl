#!/usr/bin/env bash
# The gate at Harrow: a hand-written script that Jev traverses.
#   scripts/guard.sh --script     print the script, no network
#   scripts/guard.sh              play it; needs TYPESAFE_API_KEY in the environment
set -euo pipefail
cd "$(dirname "$0")/.."
if [[ "${1:-}" != --script ]]; then
  : "${TYPESAFE_API_KEY:?set TYPESAFE_API_KEY}"
fi
cabal build -v0 exe:jev-dsl-guard
exec "$(cabal list-bin exe:jev-dsl-guard)" "$@"
