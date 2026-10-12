#!/usr/bin/env bash
# A transport for the examples: request JSON on stdin, response JSON on
# stdout. Resolve the key here, without passing it through Haskell or curl argv.
# Overload and gateway errors are retried; any other body is passed back
# so the library can decode the provider's rejection.
set -euo pipefail
typesafe_key=${TYPESAFE_API_KEY:-}
if [[ -z "$typesafe_key" ]]; then
  typesafe_config=${XDG_CONFIG_HOME:-${HOME:?HOME or XDG_CONFIG_HOME is required}/.config}
  typesafe_key_file="$typesafe_config/typesafe/api-key"
  if [[ ! -r "$typesafe_key_file" ]]; then
    echo "transport: set TYPESAFE_API_KEY or provide $typesafe_key_file" >&2
    exit 1
  fi
  typesafe_key=$(< "$typesafe_key_file")
fi
if [[ -z "$typesafe_key" || "$typesafe_key" == *$'\n'* || "$typesafe_key" == *$'\r'* ]]; then
  echo "transport: API key must be a nonempty single line" >&2
  exit 1
fi
body=$(mktemp); trap 'rm -f "$body"' EXIT
cat > "$body"
for attempt in 1 2 3 4 5; do
  out=$(curl -sS -w '\n%{http_code}' \
    -X POST https://api.typesafe.ai/v1/systemone \
    -H @- \
    -H "Content-Type: application/json" \
    --data-binary @"$body" <<< "Authorization: Bearer $typesafe_key")
  code=${out##*$'\n'}
  case "$code" in
    500|502|503|504|529) sleep "$attempt"; continue ;;
  esac
  printf '%s\n' "${out%$'\n'*}"
  exit 0
done
echo "transport: gave up after $attempt attempts, last status $code" >&2
exit 1
