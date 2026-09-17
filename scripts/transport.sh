#!/usr/bin/env bash
# A transport for the examples: request JSON on stdin, response JSON on
# stdout. The key stays in the environment, out of every Haskell process.
# Overload and gateway errors are retried; any other body is passed back
# so the library can decode the provider's rejection.
set -euo pipefail
: "${TYPESAFE_API_KEY:?set TYPESAFE_API_KEY}"
body=$(mktemp); trap 'rm -f "$body"' EXIT
cat > "$body"
for attempt in 1 2 3 4 5; do
  out=$(curl -sS -w '\n%{http_code}' \
    -X POST https://api.typesafe.ai/v1/systemone \
    -H "Authorization: Bearer $TYPESAFE_API_KEY" \
    -H "Content-Type: application/json" \
    --data-binary @"$body")
  code=${out##*$'\n'}
  case "$code" in
    500|502|503|504|529) sleep "$attempt"; continue ;;
  esac
  printf '%s\n' "${out%$'\n'*}"
  exit 0
done
echo "transport: gave up after $attempt attempts, last status $code" >&2
exit 1
