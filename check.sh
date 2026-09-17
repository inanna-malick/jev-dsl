#!/usr/bin/env bash
# Build, run the suite, then require every test/reject/*.hs to fail to compile
# at its own site. Run inside an environment with GHC 9.12 and cabal.
set -euo pipefail
cd "$(dirname "$0")"
cabal build all --enable-tests
cabal test --test-show-details=direct
out=dist-newstyle/reject
mkdir -p "$out"
for f in test/reject/Reject*.hs; do
  fixture=$(basename "$f" .hs)
  if cabal exec -v0 -- ghc -Wall -Werror -Werror=missing-fields -fno-code -package jev-dsl -outputdir "$out" "$f" > "$out/$fixture.log" 2>&1; then
    echo "UNEXPECTED COMPILE SUCCESS: $fixture" >&2
    exit 1
  fi
  if ! grep -q "$fixture.hs:.*error:" "$out/$fixture.log"; then
    cat "$out/$fixture.log" >&2
    exit 1
  fi
  echo "Rejected as expected: $fixture"
done
