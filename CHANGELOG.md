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
- The score endpoint is `Score p levels`, payload first. Its answer
  carries every level's result, which is what `grade` reads.
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
- Answers are abstract. The answer to a choice is a `Chosen alts`, to a
  Noul a `Yes`, to a score a `Scored p levels`, each exporting its fields
  and nothing else: an answer cannot be built or matched, the alternative
  that won is reached only through `settle`, `handle` or `contenders`,
  and a level's result only through `grade`. `A` is no longer on the
  authoring surface; a helper's signature reads `Chosen Routes -> Text`
  where it read `A Value (Choice Routes) -> Text`, and `explain` takes
  any answer a policy can weigh.
- An optional question is a battery of none or one: an empty `each`
  renders to nothing on the wire and decodes back to `[]`.
