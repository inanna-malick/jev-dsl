#!/usr/bin/env bash
# A transport for the examples: request JSON on stdin, response JSON on
# stdout. The key stays in the environment, out of every Haskell process.
set -euo pipefail
: "${TYPESAFE_API_KEY:?set TYPESAFE_API_KEY}"
exec curl -sS --fail-with-body \
  -X POST https://api.typesafe.ai/v1/systemone \
  -H "Authorization: Bearer $TYPESAFE_API_KEY" \
  -H "Content-Type: application/json" \
  --data-binary @-
