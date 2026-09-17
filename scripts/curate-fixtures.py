#!/usr/bin/env python3
"""Copy a curated subset of captured Jev exchanges into test/fixtures.

Each manifest line is `<name> <path relative to the evidence directory>`.
Only {probe, status, request, response} are kept. A capture whose body was
credential-redacted is refused; a public repository must never carry one.
"""
import argparse, datetime, json, pathlib, sys

ap = argparse.ArgumentParser()
ap.add_argument("--evidence", required=True)
ap.add_argument("--manifest", required=True)
ap.add_argument("--out", required=True)
args = ap.parse_args()

evidence = pathlib.Path(args.evidence)
out = pathlib.Path(args.out)
rows = []
for line in pathlib.Path(args.manifest).read_text().splitlines():
    line = line.strip()
    if not line or line.startswith("#"):
        continue
    name, rel = line.split()
    src = evidence / rel
    cap = json.loads(src.read_text())
    if cap.get("body_credentials_redacted"):
        sys.exit(f"{rel}: credential-redacted capture; refusing")
    status = cap["exchange"]["status"]
    fixture = {
        "probe": cap.get("probe"),
        "status": status,
        "request": cap["request"],
        "response": cap["response_json"],
    }
    (out / f"{name}.json").write_text(json.dumps(fixture, indent=2, sort_keys=True, ensure_ascii=False) + "\n")
    model = (cap.get("response_json") or {}).get("model") if isinstance(cap.get("response_json"), dict) else None
    when = datetime.datetime.fromtimestamp(cap["started_unix_ms"] / 1000, datetime.timezone.utc).date().isoformat()
    rows.append((name, cap.get("probe"), status, model or "", cap["harness_blake3"][:8], when))

readme = ["# Fixture provenance", "",
  "Real exchanges with the TypeSafe API, captured by the tidepool research",
  "harness on the dates below. Requests asked for `jev-latest`; every success",
  "resolved to `jev-1.13.0`. The public OpenAPI document at capture time was",
  "version 0.2.0 (SHA-256 `72452d6951dbaadd1030af76434917ef103e470bf0cd6ac035b02b111bfd4d24`).",
  "Fixtures keep only `probe`, `status`, `request`, and `response`; no capture",
  "had a credential echo. Regenerate with:", "",
  "    python3 scripts/curate-fixtures.py --evidence <captures> --manifest test/fixtures/MANIFEST --out test/fixtures", "",
  "| fixture | probe | status | model | harness | captured |", "|---|---|---|---|---|---|"]
for r in rows:
    readme.append("| " + " | ".join(str(x) for x in r) + " |")
(out / "README.md").write_text("\n".join(readme) + "\n")
print(f"{len(rows)} fixtures written to {out}")
