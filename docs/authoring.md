# Authoring Jev packets with `Jev.Operators`

This is the compositional-operator front of `jev-dsl`, written for a model
working in a stateful Haskell session and for reviewers of such code. It
needs no declarations: a packet is an expression, its type is inferred from
the questions it holds, and the answers come back under the same labels.
The declared-record front for humans is designed in
[records-dsl.md](records-dsl.md).

Import `Jev.Operators` with `DataKinds`, `OverloadedLabels`,
`OverloadedRecordDot`, `OverloadedStrings`, `TypeApplications`, and
`TypeOperators`. The JSON type is aeson's `Value`, but it appears in user
code only when a description or a state is built by hand.

## One question

```haskell
answer <- jev1 transport jevLatest (stateText inquiry)
  (choice "Which line begins the retry-timeout branch?"
     (#no_match () .| many [(key, String line, (lineNo, revision)) | ...]))
case answer of
  Right a -> caseOf a (#no_match (\() -> handBack) .| onMany (\e -> editAt (elementPayload e)))
  Left err -> ...
```

`jev1` is a whole packet with one question under the label `value`.
`transport :: Value -> m (Either Text Value)` is anything that posts JSON
and hands the body back. The alternative type was inferred from the offer:
`"no_match" ::> () :|: Many (Int, Revision)`.

## Packets

A packet is a list of labelled questions:

```haskell
packet =
     #next     := choice @Routes "Which continuation advances the inquiry?" offers
  :& #enough   := noul "Does the supplied evidence answer the inquiry?"
  :& #urgency  := score @Urgency "What is the consequence of waiting?"
  :& #children := each [ (name, #useful := noul ("Is " <> name <> " relevant?") :& Nil) | name <- names ]
  :& #evidence := group (#gap := noul "Is source missing?" :& Nil)
  :& Nil
```

Its type is `Packet '[ "next" ::= Choice Routes, "enough" ::= Noul, … ] Questions`,
inferred. Two packets join with `++.`. Labels are the wire keys; nested
packets flatten to dotted paths with dots in keys escaped, so a label may be
anything. A duplicate label, at construction or on `++.`, is a compile error
naming it.

`roundTrip transport model state packet` returns `Response` whose `answers`
is the same packet under `Answers`; read it with the labels:
`a.next`, `a.enough`, `a.children` (a list of `(key, sub-packet)`),
`a.evidence.gap`. Accessing a label the packet lacks is a compile error that
lists the labels it has.

`prepare` builds the request without sending it (`requestValue`, `preview`
for pretty JSON); `decodeResponse` decodes a body against a prepared packet.
`roundTrip` is the two together.

## Alternatives

A `Choice` ranges over a disjunction written in the type:

```haskell
type Routes = "use_witness" ::> Witness :? "The current span already answers the inquiry"
          :|: "ask_model"   ::> Handoff :? "Choosing needs a design preference beyond the evidence"
          :|: Many Edge
```

- `label ::> payload` is one alternative. The label is the wire key; the
  payload is a local value the model never sees and the answer returns.
- `:? "description"` puts the model-facing description in the type. Without
  it the description is supplied at the offer.
- `Many payload` is a runtime group: elements with keys and descriptions at
  the value level, sharing one payload type. Use it for candidates computed
  at runtime (edges, lines, hypotheses). Keys must not collide with labels;
  that is a preparation error.
- `Sum t` offers the constructors of an ordinary Haskell sum; see below.

Offers follow declaration order and are checked against it:

```haskell
offers = #use_witness (Witness "complete_request:41")
      .| #ask_model (Handoff "preference")
      .| many [(edgeKey e, String (edgeText e), e) | e <- edges]
```

A described alternative takes just its payload; a bare one takes
`(description, payload)`. `describe value` on a described alternative
overrides the type-level wording at the value level, with structured or
null content. A label out of order, a missing alternative, a label where
`Many` stands, or parentheses inside a chain each produce a compile error
that says which label was expected.

Elimination is a handler list in the same order:

```haskell
caseOf a.next
  (  #use_witness (\(Witness w) -> ...)
  .| #ask_model (\(Handoff h) -> ...)
  .| onMany (\e -> ... elementKey e ... elementPayload e ...) )
```

A reusable handler list is a value of type `Handlers r Routes`; an offer is
an `Offers Routes`. `handle selection handlers` eliminates one `Selected`
value, so the same handlers serve the winner and any contender from
`ranked`. `selectedKey` gives a selection's wire key.

Results carry the evidence: `chosen`, `ranked` (mass and selection, best
first), `confidence`, `alternatives` (every wire key with its mass).
`accept policy answer` returns the selection or a `Doubt` (`NearTie`,
`Underweight`, `Unconfident`) under a `Policy {minMass, minMargin,
minConfidence}`; `acceptOr onDoubt policy answer handlers` chains into the
handlers. `lenient` accepts whatever won. Thresholds are yours; take them
from data, not from a guess.

### Ordinary sums

When a Haskell sum already exists, `Sum t` uses it. Constructor names become
snake-case wire keys through `ConName` (a `Generic` default); descriptions are
offered per value, and repeated constructors get explicit keys:

```haskell
data Next = Rerun Check | ReadSource Span | AskModel Handoff deriving (Generic, Show)
instance ConName Next

choice @(Sum Next) "What next?"
  (sumOffer [("rerun the focused check", Rerun c), ("read the span", ReadSource s), ("ask", AskModel h)])
-- answer: case chosen a of SelSum key value -> case value of ...
```

Coverage is a `case`; exhaustiveness is GHC's `-Wincomplete-patterns`.

## Rubrics

A `Score` ranges over a type-level list of levels, in order:

```haskell
type Urgency = '[ "background"   :? "No current action depends on this"
                , "checkpoint"   :? "Useful at the next ordinary checkpoint"
                , "blocked"      :? "A worker cannot take its next action"
                , "invalidating" :? "Continuing would invalidate ongoing work" ]
```

`score @Urgency "..."` needs no value. Bare labels (`Lvl "l"`) take their
descriptions at the value level with `scoreWith`, and the two must
correspond exactly or preparation fails. One to ten levels and unique labels
are checked at compile time. Answers give `expectation`, `masses` by label,
`scoreConfidence`, `legend` (the returned wording, by label),
`massAtOrAbove #blocked`, and `levelOf` (the level nearest the expectation).
`scale` is the same question with a fully runtime rubric (`Levels`).

## Pools

When several questions range over the same alternatives, declare them once
and reference them:

```haskell
probes = pool #probes [("run_retry", "Retries m42 and counts callbacks", Command "just test-target actor retry"), ...]

pooledPacket =
     #probes := probes
  :& #best   := choice "Which probe first?" (manyFrom probes .| #none ())
  :& #per    := eachIn probes (\r -> #useful := askAbout r "Does this probe help?" :& Nil)
  :& Nil
```

A pool is named at its binding and placed under the same label at the top
level of the packet (a different label is a compile error). Its descriptions
serialize once into the state, which must be wrapped with `pooled`; the wire
carries `{"context": yourState, "pools": {...}}`. References send null
descriptions and keep the pool's wording locally. `manyFrom` is a `Many`
offer; `eachIn` asks a sub-packet per entry; `askAbout` addresses an entry
by structured fields. A reference to an undeclared pool, a use whose
contents differ from the declaration, a pool declared inside a nested
packet, or pools without a pooled state are preparation errors.

## Wording

`noul`, `choice`, `score` take a question string. `noulWith`, `choiceWith`,
`scoreWith`, `scale` take `Instructions`: `question`, `structured [...]`,
`about q extras`, `Instructions anyValue`, or `NoInstructions`. Nouls take
`Criteria` as well: `noCriteria`, `yesOnly`, `noOnly`, `bothSides`, or an
explicit `Presence`. `noulAbout q value` attaches a value under `about`.

`given premise question` prefixes a runtime premise; the wire carries
`{"premise": ..., "instructions": ...}`. Use it to ask the dependent question
under each likely premise in the same packet and consume the one whose
premise won.

`dynamic [(key, someQ q)]` is a runtime-shaped map of heterogeneous
questions; `exact` is the same at the root with verbatim ids;
`rawUnchecked` sends any value and is outside every guarantee.

## What is checked where

At compile time: label uniqueness and presence, alternative labels and
their order in offers and handlers, the 255 bound on static alternatives,
rubric bounds and uniqueness, pool naming.

At `prepare`, with a named `PrepError`: empty offers, duplicate or colliding
runtime keys, descriptions and instructions of shapes the provider rejects,
rubric correspondence, runtime level counts, pool correspondence, state
shape, empty question maps.

At `decodeResponse`, with a named `DecodeError`: provider rejections (parsed
into `Rejection`), missing, unexpected, or malformed answers, selections
outside the offered set, masses outside it, legends that differ from what
was sent, values out of range. A rounded probability sum is a diagnostic on
the `Response`, not a rejection.

## Habits that pay

- Ask one packet per semantic boundary; put every question the current
  evidence can answer into it, speculative ones under `given`.
- Give every choice that may have no good answer an exit alternative, and a
  `ask_model` alternative when deciding may need judgment beyond the state.
- Read `ranked`, not just `chosen`; near ties are a typed outcome.
- Keys are model-facing; name them by what choosing them means.
- Payloads are the only thing an action should run; never a key or a
  description.
- Bind a packet in the session and keep it: `answers` are ordinary values,
  and the labels make them legible in a later turn.
