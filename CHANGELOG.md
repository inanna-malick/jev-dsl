# Changelog

## 0.1.0.0 (unreleased, early alpha)

First cut. The surface is `Jev.Operators`; `Jev.Core` is the same library
polymorphic over the JSON type. Expect the interface to change.

Breaking, since the first cut:

- `level` takes a result as well as a label and wording, exactly as `alt`
  takes a payload: `level #l wording result`.
- `grade` takes two arguments, not three: `grade floor answer`. The
  separate result-list argument is gone; it returns the result written
  beside the level the score landed on. A missing, extra, or misordered
  result is no longer checked — it is now unwritable.
- The score endpoint is `Score p levels`, payload first, and its answer
  gains a `results` field: every level's result, in level order.
- `each` takes a key, a question, and the rows, exactly as `many` does:
  `each key q rows`. The endpoint is `Each a e`. Answers come back as
  `[(row, answer)]`, the row beside its answer, so there is nothing to
  look up afterward — the same shape `many` already had.
- `Response` has no record fields. `answers`, `usage`, `resolvedModel` and
  `diagnostics` are plain functions, and a response reads directly by its
  packet's labels (`r.next`) without projecting out `answers r` first.
- `Handles hs alts` is a new exported constraint synonym for
  `(Alternatives alts, Match hs alts, hs ~ alts)`, for a helper's own
  signature over a handler list. It replaces `Alternatives` on the
  authoring surface, which is no longer exported from `Jev.Operators`.
- A selection is abstract. `Selected` and `Ranked` no longer export their
  constructors, so the only way to reach the alternative that won is
  `settle`, `handle` or `contenders`, each of which takes a handler for
  every alternative.
- An optional question is a battery of none or one: an empty `each`
  renders to nothing on the wire and decodes back to `[]`.
