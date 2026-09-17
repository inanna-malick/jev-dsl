# Authoring Jev packets with `Jev.Operators`

Written for a model working in a stateful Haskell session, and for anyone
reviewing such code. A packet is an expression. Its type is inferred from
the questions it holds, and the answers come back under the same labels.
Nothing needs declaring; a type alias is optional and only ever names what
was already inferred.

Import `Jev.Operators` with `DataKinds`, `OverloadedLabels`,
`OverloadedRecordDot`, `OverloadedStrings`, and `TypeOperators`. The JSON
type is aeson's `Value`; it appears in your code only where you build a
state or structured wording by hand. A bare string literal in wording
position is wording: `alt #rerun "Rerun the focused check" c` sends the
string. Structured wording is any `Value` the provider admits — an object,
an array, or `Null`.

`Jev.Transport` has `request`, `decode`, and the same round trip under its
older names; `Jev.Operators` has `ask` and `ask1`, which are what authoring
code writes.

## One question

```haskell
answer <- ask1 transport jevLatest (state source)
  (choice "Which line begins the retry-timeout branch?"
     (alt #not_here "The branch is not in this file" () .| many [(key, String line, (lineNo, revision)) | ...]))
case answer of
  Right a -> handle a.chosen (#not_here (\() -> handBack) .| onMany (\_ (n, rev) -> editAt n rev))
  Left err -> ...
```

`ask1` is a whole packet with one question under the label `value`.
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
Under a `let` in a session, keep `:&` at the start of each continuation
line and indent every line past the first, as above; a `:&` left at the end
of a line, or a continuation line starting in the same column as the
binding, ends the expression early.
Labels are the wire keys; nested packets flatten to dotted paths with dots
in keys escaped, so a label may be anything. A duplicate label is a compile
error naming it.

`ask transport model state packet` returns a `Response`; `answers` is
the same packet under `Answers`, read with the labels: `a.next`,
`a.enough`, `a.children` (a list of `(key, sub-packet)`), `a.evidence.gap`.
A label the packet lacks is a compile error listing the labels it has.
`request model state packet` (from `Jev.Transport`) builds the body without
sending it; `decode packet body` decodes a response against the packet.
`ask` is both.
`usage` on the response is a `Usage { inputTokens, outputTokens }`; `resolvedModel` is the model the request actually resolved to.

## Reading answers

An answer is a plain record. It displays, and every number it carries is a
field:

```
> a.next
Choice {key = "rerun", mass = 0.82, margin = 0.65, confidence = 0.72, masses = ["rerun" 0.82, "ask_model" 0.17]}
> a.next.key
"rerun"
> a.next.margin
0.65
```

| Question | Fields |
|---|---|
| `choice` | `key`, `mass`, `margin`, `confidence`, `masses` (best first), `chosen` |
| `noul` | `yes` |
| `score` | `nearest`, `expectation`, `confidence`, `masses` (by level, in order) |

`margin` is the winner's mass less the runner-up's, and equals the mass
when nothing competes. A margin at or near 1.0 means no other option was
in play — usually a sign the alternatives were not really rivals.

Record dot needs the field selectors in scope, so importing `Jev.Operators`
unqualified takes some short names for itself. The fields: `key`, `mass`,
`margin`, `confidence`, `masses`, `chosen`, `yes`, `nearest`, `expectation`,
`ranked`. The verbs: `ask`, `ask1`, `alt`, `many`, `level`, `pool`, `each`,
`state`, `given`, `about`, `handle`, `accept`, `explain`. Under
`-Wall -Werror` a local binding with any of these names is a shadowing
error, and a local `ask` or `key` is the usual way to hit it; name your own
`tag`, `weight`, `askLine`, or import qualified. Reach the winner as
`a.chosen` (the field), not `chosen a`; `handle a.chosen handlers` is the one
idiom the docs use.

`accept policy answer` weighs those fields and returns either the selection
or a `Doubt`: `NearTie`, `Underweight`, or `Unconfident`. Three named
policies cover the usual cases — `routing` for a read-only choice (which
file, which skill), `spawning` for starting a worker or choosing an
approach, `merging` for merging, stopping, or anything with a receipt.
`explain policy answer` says in one line which check passed or failed and
the numbers behind it. A whole `Response` displays as its answers under
their labels, and `toJSON` on an answer or on an answers packet is a ledger
row.

```haskell
case accept merging a.next of
  Right s -> handle s (#rerun (\c -> run c) .| #ask_model (\h -> handBack h) .| onMany (\_ e -> follow e))
  Left doubt -> stop (explain merging a.next) doubt
```

`handle` is the one thing fields cannot do: it runs the payload the winning
alternative carried. `chosen` is the winner as a typed selection,
`contenders floor answer` is every alternative at or above a mass floor as
selections, best first, and `s.key` (or `selectedKey s`) is a selection's
wire key. A handler list is an ordinary value: bind it once and use it on
the winner and on every contender. `massAtOrAbove #blocked a.urgency` sums
a rubric from a level up.

## Writing questions

The measured difference between a packet a model answers well and one it
answers at 0.5 everywhere is in the wording, not the types.

- **An option describes the condition that makes it apply**, in terms of
  the state's own fields: `"The state shows every one of build, test and
  lint"`, not `"all present"`. A vague question — "is this sufficient?" —
  scores near 0.5 on everything; the same judgment written as three
  conditions over named fields scores 0.95 at 0.92 confidence.
- **A checklist is an ordinary `choice`** with one option per outcome:
  everything present, something missing, something contradictory. Write
  each option as the condition, and name the items in the wording.
- **A judgment Noul carries its criteria in the question**: what makes it
  true and what makes it false, in the question text or as structured
  members with `about`. There is no separate place to put them.
- **Rivals come from evidence, not from symmetry.** Offer an alternative
  because the state could support it. An option that argues for itself
  steers the answer; an option that merely describes its condition does
  not.
- **Give every choice an exit** — `#not_here`, `#none`, `#ask_model` — so
  "none of these" is an answer rather than a forced pick.
- **Keys are model-facing**: name them by what choosing them means.

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
handle a.next.chosen
  (  #use_witness (\(Witness w) -> ...)
  .| #ask_model (\(Handoff h) -> ...)
  .| onMany (\key e -> ...) )
```

A label out of order, a handler missing or extra, a label where `Many`
stands, or parentheses inside a chain each produce a compile error that
says which label was expected. Elimination through handlers is for the
payload; everything else about an answer is a field, as in "Reading
answers" above.

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
request is built. The answer gives `expectation`, `nearest`, `confidence`,
and `masses` by label, plus `massAtOrAbove #blocked`.

`nearest` is the level nearest the expectation, which is the right reading
of an ordinal scale with a threshold. It is not the likeliest level: masses
of 12%, 26%, 62% have their expectation at the middle level. To act on the
likeliest, take the maximum of `masses`.

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

## Patterns

Each of these is used in `examples/Guard.hs`, a dialogue tree folded by a
catamorphism whose algebra is Jev.

- **The continuation is the payload.** When the program's next step depends
  on the branch, offer the branches with their continuations as payloads
  and let the handler run the winner: `many [(label, wording, next) | …]`
  then `handle a.chosen (onMany (\_ next -> next))`. No dispatch table.
- **Rules in Haskell, judgments in Jev.** Decide eligibility before the
  call and offer only what is legal now; do not ask a Noul whether an
  alternative should be on offer. What the state cannot decide, a question
  does.
- **Two questions, one call, reconciled in code.** A branch choice and a
  Noul such as "does this reply admit to something the rules forbid" go in
  the same packet; the program takes the Noul's route when `yes` clears a
  floor and the chosen branch otherwise. Give the Noul a route only where
  there is somewhere to send the case; a tripwire with nowhere to go steals
  branches that mean something.
- **Frequency is a rule, not a wording.** "Which of these fits this moment,
  or none" with the same events fired never under "most moments, nothing
  does" and every time under neutral wording. Gate how often a question is
  asked in code; keep the wording about the judgment.
- **Carry the conversation.** Every call's state holds the whole exchange
  so far and whatever changed the world in between. A reply judged with
  its history is judged better than the same reply alone; the cost is a few
  thousand input tokens per call.
- **Retry at the transport.** The provider returns 529 under load. A
  transport that retries 5xx and 529 with backoff and passes every other
  body back lets the library decode real rejections.

## Habits that pay

- Ask one packet per semantic boundary; put every question the current
  evidence can answer into it, speculative ones under `given`.
- Offer an `ask_model` alternative when deciding may need judgment beyond
  the state.
- Read `margin` and `contenders`, not just `key`; a near tie is a typed
  outcome, and so is a 1.0 that means nothing competed.
- Payloads are the only thing an action should run; never a key or wording.
- Bind a packet in the session and keep it: answers are ordinary values,
  and the labels make them legible in a later turn.
