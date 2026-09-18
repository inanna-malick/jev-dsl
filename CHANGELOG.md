# Changelog

## 0.1.0.0 (unreleased, early alpha)

Authoring improvements:

- `holds policy answer` is a Noul judged to a `Bool`: `True` only for a
  settled yes. It replaces `judge p n == Right (Settled True)`, which was
  the commonest way to read a Noul here and silently counted every doubt
  as a no.
- `graded floor answer` is `grade` with the label of the level it landed
  on. A ledger line that names the level no longer needs the label written
  into the result as well.
- `State` keeps no partial function. A state sent as given carries no
  packet, and its index says so, so reading a field off one is the compile
  error it always was with nothing undefined behind it. `state`,
  `rawState` and `State` itself are unchanged on the surface.
- `branches` lists a `Uniform` chain as key, wording and payload, and
  `withUniform` opens one to build a question from it. `Uniform r` on the
  authoring surface no longer names the JSON type.
- `takenUnder policy answer` consumes uniform choice payloads under the
  same policy checks as `settle`, without identity handlers. Together with
  `taken`, `handle`, and `settle`, this completes policy/no-policy and
  payload/handler consumption. It is not the old handler-based `accept`.
- `Carries alts r` now determines `r` from `alts`, so reading a field off
  a uniform result needs no extra payload annotation.
- `optional` takes a `Maybe` question or packet and reads back as a
  `Maybe` answer. Absence sends no questions; presence uses the containing
  path directly. It shares a name with `Control.Applicative.optional`.
- `field` accepts typed paths: `field (#gate :/ #posters) st`. Every
  segment is checked; intermediate fields must contain nested packets.
  A bare `#label` still works, but `field` now takes a `FieldPath` rather
  than a `Label`. State references have their own renderer; their effect
  on model answers is unmeasured beyond the existing top-level evidence.
- The Guard uses `optional` and uniform tripwire payloads. Its tripwire
  wire key changes from `stop.now` to `stop`; its poster wording now names
  `gate.posters`. Old transcripts remain historical evidence, not a
  baseline for these requests. The worked example's request is unchanged.

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
- `Packet` has no terminator. A cell is a packet of one and two packets
  join with `:&`, so `Nil` is gone: `#a := x :& #b := y` is a whole packet,
  and a nested cell is `#a := (#b := x)`. Packet *types* are written the
  same way, a chain rather than a list: `Packet ("a" ::= Noul :& "b" ::=
  Choice Alts)`.
- `many` takes a label, exactly as `alt` does: `many #edges key wording
  rows`, of type `"edges" ::* Edge`. `onMany` is gone; a runtime group is
  handled through its own label like any other alternative.
- Wording is `Text` in `alt`, `many`, and `level`: the `String`/`String .`
  wrapper these needed is gone. (`noul`/`choice`/`score` question text was
  always `Text`.)
- Handlers are matched by label, not by position. A handler list written
  inline may be in any order, and an alternative added in the middle of a
  chain breaks nothing that already handles the others by label. A
  missing, extra, or duplicated handler is still a compile error naming
  the label.
- The policies are renamed: `routing` is `lenient`, `spawning` is
  `careful`, `merging` is `strict`.
- `settle` and `judge` return `Either Doubt (Settled p r)`, not `Either
  Doubt r`. A verdict now carries the policy that reached it, so a
  function can demand one in its own signature: `Settled Strict r`.
- `Doubt` is a record, `Doubt { cause :: Cause, why :: Text }`, not a bare
  `Cause`. `why` is the same line `explain` prints for a doubt, so a doubt
  branch no longer needs to call `explain` itself. A pattern match on the
  old constructors is now `Left Doubt { cause = NearTie .. }`.
- `state` takes a field packet, not a `Value` built by hand: `state
  (#failure := f :& #checks := [...])`. Fields keep their Haskell types —
  a nested state, a list of `(Text, a)`, or any type with a `Field Value a`
  instance are all admitted. `rawState` is the escape hatch for a state
  shape the surface leaves out.
- Wording that names a state field is `field #name state`, checked
  against the state's own fields at compile time, not a string typed by
  hand.
- `ask` and `ask1` take a `Session`, not a transport and a model
  separately: `session transport jevLatest` builds it once. `request` and
  `decode` are unchanged, taking a `Model` directly, as the offline split.
- Structured, non-`Text` wording is off the authoring surface. `Jev.Core`
  still carries it, and `test/Replay.hs` renders it for the recorded
  exchanges that need it.
