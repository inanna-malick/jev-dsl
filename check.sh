#!/usr/bin/env bash
# Build, run the suite, then require every test/reject/*.hs to fail to compile
# at its own site. Run inside an environment with GHC 9.12 and cabal.
set -euo pipefail
cd "$(dirname "$0")"
cabal build all --enable-tests --flag strict
cabal test --flag strict --test-show-details=direct
out=dist-newstyle/reject
mkdir -p "$out"
# Every example and the documentation's code must compile under the same policy.
for f in examples/*.hs test/Readme.hs; do
  [ -f "$f" ] || continue
  cabal exec -v0 -- ghc -Wall -Werror -fno-code -package jev-dsl -outputdir "$out" "$f" > "$out/$(basename "$f" .hs).log" 2>&1 || { cat "$out/$(basename "$f" .hs).log" >&2; exit 1; }
  echo "Compiles: $f"
done
for f in test/reject/Reject*.hs; do
  fixture=$(basename "$f" .hs)
  if cabal exec -v0 -- ghc -Wall -Werror -fno-code -package jev-dsl -outputdir "$out" "$f" > "$out/$fixture.log" 2>&1; then
    echo "UNEXPECTED COMPILE SUCCESS: $fixture" >&2
    exit 1
  fi
  if ! grep -q "$fixture.hs:.*error:" "$out/$fixture.log"; then
    cat "$out/$fixture.log" >&2
    exit 1
  fi
  # The diagnostic phrase is part of the tested interface.
  phrase=$(sed -n 's/^-- expect: //p' "$f" | head -1)
  if [ -n "$phrase" ] && ! grep -qF -- "$phrase" "$out/$fixture.log"; then
    echo "WRONG DIAGNOSTIC for $fixture; expected: $phrase" >&2
    cat "$out/$fixture.log" >&2
    exit 1
  fi
  echo "Rejected as expected: $fixture ($phrase)"
done
