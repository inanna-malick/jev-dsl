# Authoring Jev packets with `Jev.Operators`

Written for a model working in a stateful Haskell session, and for anyone
reviewing such code. A packet is an expression. Its type is inferred from
the questions it holds, and the answers come back under the same labels.
Nothing needs declaring; a type alias is optional and only ever names what
was already inferred.

Import `Jev.Operators` with `DataKinds`, `OverloadedLabels`,
`OverloadedRecordDot`, `OverloadedStrings`, and `TypeOperators`. The JSON
type is aeson's `Value`; it appears in your code only where you build a
state or structured wording by hand.

## One question

```haskell
answer <- jev1 transport jevLatest (state source)
  (choice "Which line begins the retry-timeout branch?"
     (alt #not_here "The branch is not in this file" () .| many [(key, String line, (lineNo, revision)) | ...]))
case answer of
  Right a -> handle (chosen a) (#not_here (\() -> handBack) .| onMany (\_ (n, rev) -> editAt n rev))
  Left err -> ...
```

`jev1` is a whole packet with one question under the label `value`.
`transport :: Value -> m (Either Text Value)` is anything that posts JSON
and hands the body back. The alternative type was inferred from the offer:
`"not_here" ::> () :|: Many (Int, Revision)`. The payload is what the
program acts on; the model only ever sees the wording.

## Packets

```haskell
packet =
     #next     := choice "Which continuation advances the inquiry?" offers
  :& #enough   := noul "Does the supplied evidence answer the inquiry?"
  :& #urgency  := score "What is the consequence of waiting?" urgency
  :& #children := each [ (name, #useful := noul ("Is " <> name <> " relevant?") :& Nil) | name <- names ]
  :& #evidence := (#gap := noul "Is source missing?" :& Nil)
  :& Nil
```

A cell holds a question or a nested packet. Two packets join with `++.`.
Labels are the wire keys; nested packets flatten to dotted paths with dots
in keys escaped, so a label may be anything. A duplicate label is a compile
error naming it.

`roundTrip transport model state packet` returns a `Response`; `answers` is
the same packet under `Answers`, read with the labels: `a.next`,
`a.enough`, `a.children` (a list of `(key, sub-packet)`), `a.evidence.gap`.
A label the packet lacks is a compile error listing the labels it has.
`request model state packet` builds the body without sending it; `decode
packet body` decodes a response against the packet. `roundTrip` is both.
`usage` on the response is a `Usage { inputTokens, outputTokens }`; `resolvedModel` is the model the request actually resolved to.

## Alternatives

An offer is a chain of alternatives, each with its label, its wording for
the provider, and its payload for the program:

```haskell
offers = alt #use_witness "The current span already answers the inquiry" (Witness "complete_request:41")
      .| alt #ask_model "Choosing needs a design preference beyond the evidence" (Handoff "preference")
      .| many [(edgeKey e, String (edgeText e), e) | e <- edges]
```

`many` is a runtime group: keys and wording per element, one payload type.
Use it for candidates computed at runtime: lines, edges, hypotheses. Wording
is any JSON the provider admits: a string, an object, or `Null`.

The chain's type is `"use_witness" ::> Witness :|: "ask_model" ::> Handoff :|: Many Edge`.
Give it a name when a helper wants to mention it in a signature; never for
the compiler's sake.

Elimination is a handler list in declaration order:

```haskell
handle (chosen a.next)
  (  #use_witness (\(Witness w) -> ...)
  .| #ask_model (\(Handoff h) -> ...)
  .| onMany (\key e -> ...) )
```

A handler list is an ordinary value: bind it once and use it on the winner
and on every contender. A label out of order, a handler missing or extra, a
label where `Many` stands, or parentheses inside a chain each produce a
compile error that says which label was expected.

Answers carry the evidence. `chosen` is the winner as a typed selection;
`contenders floor answer` is every alternative at or above a mass floor,
best first, as selections; `confidence` and `masses` are the provider's
numbers; `selectedKey` is a selection's wire key. `accept policy answer`
returns the selection or a `Doubt` (`NearTie`, `Underweight`, `Unconfident`)
under a `Policy {minMass, minMargin, minConfidence}`. Thresholds are yours;
take them from data. `explain policy answer` gives the same verdict as one
line of prose, naming the check order and the numbers behind it.

Three named policies cover common cases: `routing` for read-only choices
(which file, which skill), `spawning` for starting a worker or choosing an
approach, and `merging` for merging, stopping, or anything with a receipt.

## Rubrics

A score ranges over a chain of levels, in order:

```haskell
urgency = level #background "No current action depends on this"
       .| level #checkpoint "Useful at the next ordinary checkpoint"
       .| level #blocked "A worker cannot take its next action"
       .| level #invalidating "Continuing would invalidate ongoing work"
```

Its type is `"background" :|: "checkpoint" :|: "blocked" :|: "invalidating"`.
Duplicate labels are a compile error; one to ten levels is checked when the
request is built. Answers give `expectation`, `masses` by label,
`confidence`, `massAtOrAbove #blocked`, and `levelOf`, the level nearest
the expectation.

## Pools

When several questions range over the same alternatives, declare them once
and draw on them:

```haskell
probes = pool #probes [("run_retry", "Retries m42 and counts callbacks", Command "just test-target actor retry"), ...]

pooledPacket =
     #probes := probes
  :& #best   := choice "Which probe first?" (manyFrom probes .| alt #none "No probe helps" ())
  :& #per    := eachIn probes (\r -> #useful := askAbout r "Does this probe help?" :& Nil)
  :& Nil
```

A pool is named at its binding and placed under the same label at the top
level of the packet; a different label is a compile error. Its wording is
sent once, in the state, which the request wraps as
`{"context": yourState, "pools": {...}}` whenever a packet declares a pool.
A choice that draws on a pool sends null wording for its keys and names the
pool beside its question, so two pools may reuse keys freely; a choice may
draw on one pool. `eachIn` asks a sub-packet per entry; `askAbout` asks
about one entry by pool and key; `refKey` and `refPayload` read the entry.
An undeclared pool, a use whose contents differ from the declaration, a
pool declared inside a nested packet, duplicate keys in a pool, or two
pools in one choice are errors when the request is built.

## Wording

`noul`, `choice`, and `score` take the question as text. `about [(key,
value)] question` adds structured members beside it, rendered as
`{"question": ..., key: value}`. `given premise question` prefixes a
runtime premise, rendered as `{"premise": ..., "instructions": ...}`; ask
the dependent question under each likely premise in one packet and consume
the one whose premise won. A duplicate member is an error when the request
is built, through any number of premises.

## What is checked where

At compile time: label uniqueness and presence, handler lists against
alternatives, rubric label uniqueness, pool naming, cell contents.

When the request is built, with a named `PrepError` inside `JevError`:
empty offers, duplicate or colliding runtime keys, wording and state shapes
the provider rejects, level counts, pool correspondence, duplicate
structured members, empty question maps.

When the response is decoded, with a named `DecodeError`: provider
rejections (parsed), missing, unexpected, or malformed answers, selections
outside the offered set, masses outside it, legends that differ from what
was sent, values out of range.

## Deliberately unsupported

The library covers what a program wants to express, not every request the
provider accepts. These shapes are left out on purpose; the test tree's
replay module renders them so recorded exchanges still round-trip.

- Wording for what yes and no mean on a Noul. Put it in the question.
- A choice drawing on two pools. Build one `many` with explicit wording.
- A rubric whose levels exist only at runtime. Levels are authored.
- A question sent verbatim, or a question id the flattening would not
  produce.
- The provider's distinction between an omitted and a null criteria block
  or instruction.

Each returns when a program in `test/Corpus.hs` needs it.

## Habits that pay

- Ask one packet per semantic boundary; put every question the current
  evidence can answer into it, speculative ones under `given`.
- Give every choice that may have no good answer an exit, and an
  `ask_model` alternative when deciding may need judgment beyond the state.
- Read `contenders`, not just `chosen`; a near tie is a typed outcome.
- Keys are model-facing; name them by what choosing them means.
- Payloads are the only thing an action should run; never a key or wording.
- Bind a packet in the session and keep it: answers are ordinary values,
  and the labels make them legible in a later turn.
