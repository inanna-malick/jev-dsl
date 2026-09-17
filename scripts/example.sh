#!/usr/bin/env bash
# One real Jev call through the worked example: request on stdout, curl,
# response into decode. Needs TYPESAFE_API_KEY in the environment.
set -euo pipefail
cd "$(dirname "$0")/.."
: "${TYPESAFE_API_KEY:?set TYPESAFE_API_KEY}"
cabal build -v0 exe:jev-dsl-example
example=$(cabal list-bin exe:jev-dsl-example)

args=(
  --failure "bookmark identity assertion failed after inserting text before the bookmark"
  --diagnostic "d1=warning: unused import StableId"
  --diagnostic "d2=bookmark identity assertion failed after prefix insertion; observed old numerical offset"
  --diagnostic "d3=test suite aborted because bookmark regression failed"
  --check "prefix_insert=Compares saved bookmark offset with the logical character after a prefix insert"
  --check "lookup_benchmark=Measures lookup time only"
  --check "path_normalization=Checks path normalization"
)

request=$("$example" request "${args[@]}")
echo "--- request" >&2
echo "$request" >&2
echo "--- response" >&2
"$example" decode "${args[@]}" < <(curl -sS --fail-with-body \
  -X POST https://api.typesafe.ai/v1/systemone \
  -H "Authorization: Bearer $TYPESAFE_API_KEY" \
  -H "Content-Type: application/json" \
  --data "$request")
