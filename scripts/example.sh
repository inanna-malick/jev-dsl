#!/usr/bin/env bash
# One real Jev call through the worked example: request on stdout, curl,
# response into decode. Uses scripts/transport.sh for credential lookup.
set -euo pipefail
cd "$(dirname "$0")/.."
cabal build -v0 exe:jev-dsl-example
example=$(cabal list-bin exe:jev-dsl-example)

args=(
  --failure "bookmark identity assertion failed after inserting text before the bookmark"
  --diagnostic "d1=warning: unused import StableId"
  --diagnostic "d2=bookmark identity assertion failed after prefix insertion; observed old numerical offset"
  --diagnostic "d3=test suite aborted because bookmark regression failed"
  --check "prefix_insert=Compares saved bookmark offset with the logical character after a prefix insert"
  --check "bookmark.fuzz/quick=Randomised bookmark edits with a small budget"
  --check "lookup_benchmark=Measures lookup time only"
  --check "path_normalization=Checks path normalization"
)

request=$("$example" request "${args[@]}")
echo "--- request" >&2
echo "$request" >&2
echo "--- response" >&2
printf '%s\n' "$request" | scripts/transport.sh | "$example" decode "${args[@]}"
