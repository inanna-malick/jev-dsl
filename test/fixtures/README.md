# Fixture provenance

Real exchanges with the TypeSafe API, captured by the tidepool research
harness on the dates below. Requests asked for `jev-latest`; every success
resolved to `jev-1.13.0`. The public OpenAPI document at capture time was
version 0.2.0 (SHA-256 `72452d6951dbaadd1030af76434917ef103e470bf0cd6ac035b02b111bfd4d24`).
Fixtures keep only `probe`, `status`, `request`, and `response`; no capture
had a credential echo. Regenerate with:

    python3 scripts/curate-fixtures.py --evidence <captures> --manifest test/fixtures/MANIFEST --out test/fixtures

| fixture | probe | status | model | harness | captured |
|---|---|---|---|---|---|
| structured | structured | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| escaped-keys | escaped-keys | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| state-array | state-array | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| state-empty-string | state-empty-string | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| state-empty-object | state-empty-object | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| state-empty-array | state-empty-array | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| score-one | score-one | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| score-two | score-two | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| score-ten | score-ten | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| score-array-level | score-array-level | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| score-empty-string-level | score-empty-string-level | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| score-empty-object-level | score-empty-object-level | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| choice-one | choice-one | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| choice255 | choice255 | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| choice-null-description | choice-null-description | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| choice-array-description | choice-array-description | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| choice-deep-description | choice-deep-description | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| choice-empty-string-description | choice-empty-string-description | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| choice-empty-object-description | choice-empty-object-description | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| choice-empty-key | choice-empty-key | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| questions255 | questions255 | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| questions256 | questions256 | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| noul-criteria-omitted | noul-criteria-omitted | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| noul-criteria-null | noul-criteria-null | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| noul-criteria-empty | noul-criteria-empty | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| noul-true-only | noul-true-only | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| noul-false-only | noul-false-only | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| noul-outcomes-null | noul-outcomes-null | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| noul-unknown-criterion | noul-unknown-criterion | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| instructions-omitted | instructions-omitted | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| instructions-null | instructions-null | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| instructions-array | instructions-array | 200 | jev-1.13.0 | 6e3f42c3 | 2026-09-16 |
| instructions-empty-string | instructions-empty-string | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| instructions-empty-object | instructions-empty-object | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| instructions-empty-array | instructions-empty-array | 200 | jev-1.13.0 | 41258346 | 2026-09-16 |
| shoal-route | shoal-route | 200 | jev-1.13.0 | 0217261c | 2026-09-16 |
| shoal-evidence | shoal-evidence | 200 | jev-1.13.0 | 0217261c | 2026-09-16 |
| shoal-experiment | shoal-experiment | 200 | jev-1.13.0 | 0217261c | 2026-09-16 |
| shoal-repair | shoal-repair | 200 | jev-1.13.0 | 0217261c | 2026-09-16 |
| shoal-question-distinct | shoal-question-distinct | 200 | jev-1.13.0 | 0217261c | 2026-09-16 |
| shoal-attention-urgent | shoal-attention-urgent | 200 | jev-1.13.0 | 0217261c | 2026-09-16 |
| shoal-expand | shoal-expand | 200 | jev-1.13.0 | 0217261c | 2026-09-16 |
| world-conflict | world-conflict | 200 | jev-1.13.0 | 34320014 | 2026-09-16 |
| frontier-fanout-32 | frontier-fanout-32 | 200 | jev-1.13.0 | 0667ca7e | 2026-09-16 |
| choice256 | choice256 | 400 |  | 6e3f42c3 | 2026-09-16 |
| choice-zero | choice-zero | 400 |  | 6e3f42c3 | 2026-09-16 |
| score-eleven | score-eleven | 400 |  | 6e3f42c3 | 2026-09-16 |
| question-empty-key | question-empty-key | 400 |  | 41258346 | 2026-09-16 |
| mixed-valid-invalid-questions | mixed-valid-invalid-questions | 400 |  | 41258346 | 2026-09-16 |
| model-unknown | model-unknown | 400 |  | 41258346 | 2026-09-16 |
| bounding-box-empty | bounding-box-empty | 400 |  | 5c651803 | 2026-09-16 |
| empty-questions | empty-questions | 422 |  | 6e3f42c3 | 2026-09-16 |
| questions-null | questions-null | 422 |  | 41258346 | 2026-09-16 |
| questions-array | questions-array | 422 |  | 41258346 | 2026-09-16 |
| questions-omitted | questions-omitted | 422 |  | 41258346 | 2026-09-16 |
| model-null | model-null | 422 |  | 41258346 | 2026-09-16 |
| model-omitted | model-omitted | 422 |  | 41258346 | 2026-09-16 |
| state-null | state-null | 422 |  | 6e3f42c3 | 2026-09-16 |
| state-omitted | state-omitted | 422 |  | 41258346 | 2026-09-16 |
| state-boolean | state-boolean | 422 |  | 6e3f42c3 | 2026-09-16 |
| state-number | state-number | 422 |  | 6e3f42c3 | 2026-09-16 |
| question-type-omitted | question-type-omitted | 422 |  | 41258346 | 2026-09-16 |
| question-type-unknown | question-type-unknown | 422 |  | 41258346 | 2026-09-16 |
| question-type-uppercase | question-type-uppercase | 422 |  | 41258346 | 2026-09-16 |
| score-zero | score-zero | 422 |  | 6e3f42c3 | 2026-09-16 |
| score-null-level | score-null-level | 422 |  | 6e3f42c3 | 2026-09-16 |
| instructions-boolean | instructions-boolean | 422 |  | 6e3f42c3 | 2026-09-16 |
| instructions-number | instructions-number | 422 |  | 6e3f42c3 | 2026-09-16 |
| choice-boolean-description | choice-boolean-description | 422 |  | 6e3f42c3 | 2026-09-16 |
| choice-number-description | choice-number-description | 422 |  | 6e3f42c3 | 2026-09-16 |
| max-tokens-exceeded | frontier-fanout-1024 | 400 |  | 1824c872 | 2026-09-16 |
