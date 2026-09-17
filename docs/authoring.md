# Authoring Jev packets

Written for a model working in a stateful Haskell session, and for anyone
reviewing such code. A packet is an expression. Its type is inferred from
the questions it holds, and the answers come back under the same labels.
Nothing needs declaring; a type alias is optional and only ever names what
was already inferred.

Import `Jev.Operators` with `DataKinds`, `OverloadedLabels`,
`OverloadedRecordDot`, `OverloadedStrings`, and `TypeOperators`. That is the
whole surface; `Jev.Core` exists only to build a facade over another JSON
type. The JSON type here is aeson's `Value`, and it appears in your code
only where you build a state or structured wording by hand. A bare string
literal in wording position is wording: `alt #rerun "Rerun the focused
check" c` sends the string. Structured wording is any `Value` the provider
admits: an object, an array, or `Null`.

## The three questions

A **Noul** is a proposition. The answer is one probability, `yes`.

A **choice** is a set of labelled alternatives, one of which comes back,
carrying the payload your program offered it with.

A **score** grades something on an ordered rubric of one to ten levels. The
answer is a distribution over the levels and its expectation.

## One question

```haskell
answer <- ask1 transport jevLatest (state source)
  (choice "Which line begins the retry-timeout branch?"
     (alt #not_here "The branch is not in this file" () .| many (T.pack . show . (.lineNo)) (String . (.lineText)) lines))
case answer of
  Right a -> settle routing a (#not_here (\() -> handBack) .| onMany (\_ l -> editAt l.lineNo l.revision))
  Left err -> ...
```

`ask1` is a whole packet with one question under the label `value`.
`transport :: Value -> m (Either Text Value)` is anything that posts JSON
and hands the body back. The alternative type was inferred from the offer:
`"not_here" ::> () :|: Many Line`. The payload is what the program acts on;
the model only ever sees the wording.

## Packets

```haskell
packet =
     #next     := choice "Which continuation advances the inquiry?" offers
  :& #enough   := noul "Does the supplied evidence answer the inquiry?"
  :& #urgency  := score "What is the consequence of waiting?" urgency
  :& #children := each (.key) (\e -> #useful := noul ("Is " <> e.key <> " (" <> e.text <> ") relevant?") :& Nil) edges
  :& #evidence := (#gap := noul "Is source missing?" :& Nil)
  :& Nil
```

A cell holds a question or a nested packet. Under a `let` in a session,
keep `:&` at the start of each continuation line and indent every line past
the first, as above; a `:&` left at the end of a line, or a continuation
line starting in the same column as the binding, ends the expression early.
Labels are the wire keys; nested packets flatten to dotted paths with dots
in keys escaped, so a label may be anything. A duplicate label is a compile
error naming it.

`ask transport model state packet` returns a `Response`. It has no record
fields of its own: `answers`, `usage`, `resolvedModel` and `diagnostics` are
plain functions over it, and a response reads by its packet's own labels
directly, with no need to project out the packet first: `r.next`,
`r.enough`, `r.children` (a list of `(item, sub-answer)`, the row beside
what it answered), `r.evidence.gap`. `answers r` is still there for handing
the whole packet to a function that wants it as one value rather than a
label at a time. A label the packet lacks is a compile error listing the
labels it has. `usage` is a `Usage { inputTokens, outputTokens }`;
`resolvedModel` is the model the request actually resolved to;
`diagnostics` is a list of log lines, such as a distribution that did not
sum to one.

## Acting on answers

Every answer is consumed under a policy. A policy is three floors, and
three are named for how bad it is to be wrong:

| Policy | For | mass | margin | confidence |
|---|---|---|---|---|
| `routing` | read-only choices: which file, which skill | 0.40 | 0.08 | 0.50 |
| `spawning` | starting a worker, choosing an approach | 0.55 | 0.20 | 0.70 |
| `merging` | merging, stopping, anything with a receipt | 0.70 | 0.40 | 0.85 |

`settle policy answer handlers` is the only way to consume a choice. It
returns either the result of the handler for the alternative that won, or a
`Doubt`: `NearTie`, `Underweight`, or `Unconfident`.

```haskell
case settle spawning a.next
       (  #rerun (\c -> run c)
       .| #ask_model (\h -> handBack h)
       .| onMany (\_ e -> follow e) ) of
  Right action -> action
  Left doubt -> stop (explain spawning a.next) doubt
```

There is no way to reach a result without a handler for every alternative.
That matters more than it looks: a confident answer that means "none of
these" or "the evidence is missing" runs its own handler, and cannot be
mistaken for approval by a caller that only checked for success.

`judge policy answer` does the same for a Noul, returning `Right True`,
`Right False`, or a `Doubt`. A Noul weighs yes against no, so the same
three policies apply; there is no confidence on the wire for a Noul, so
that floor is not consulted.

`explain policy answer` says in one line which check settled or doubted the
answer and the numbers behind it. It works on a choice or a Noul. This is
the line a log, a notification, or a planner reads, and it is usually worth
recording next to whatever the program did.

```
settled on rerun: confidence 0.72 ≥ 0.70, mass 0.82 ≥ 0.55, margin 0.65 ≥ 0.20
doubted ask_model (NearTie): margin 0.06 < 0.20 by 0.14; confidence 0.70, mass 0.45
```

`handle answer handlers` is `settle` without a policy, for when the program
follows the winner regardless: a dialogue branch, a sorting where every
outcome is a legal next step. `contenders floor answer handlers` is every
alternative at or above a mass floor, best first, each already through those
handlers. All three take the answer and the branches, so there is nothing to
thread between them, and a handler list is an ordinary value you bind once
and use on the winner and on every contender.

The rest of an answer is fields, read with record dot:

| Question | Fields |
|---|---|
| `choice` | `key`, `mass`, `margin`, `confidence`, `masses` (best first) |
| `noul` | `yes` |
| `score` | `expectation`, `confidence`, `masses`, `results` (by level, in order) |

Each kind has one typed consumer: `settle` for a choice, `judge` for a Noul,
`grade` for a score. `settle` and `handle` take a handler list as a value and
so cannot hand back a result the program did not write a case for. `grade`
needs no such list: the result it returns was written beside the level's own
wording when the rubric was built.

`key` is for logs and ledgers, never for dispatch: a `case` on it is
unchecked, and the compiler cannot tell you when the alternatives change.
`margin` is the winner's mass less the runner-up's, and equals the mass
when nothing competes. A margin at or near 1.0 means no other option was in
play, usually a sign the alternatives were not really rivals.

Record dot needs the field selectors in scope, so importing `Jev.Operators`
unqualified takes some short names for itself. The fields: `key`, `mass`,
`margin`, `confidence`, `masses`, `yes`, `expectation`, `results`. The
verbs: `ask`, `ask1`, `alt`, `many`, `level`, `each`, `state`, `settle`,
`judge`, `grade`, `handle`, `explain`. Under `-Wall` a local binding with any of
these names shadows; name your own `tag`, `weight`, `askLine`, or import
qualified.

## Writing questions

The measured difference between a packet a model answers well and one it
answers at 0.5 everywhere is in the wording, not the types.

- **An option describes the condition that makes it apply**, in terms of
  the state's own fields: `"The state shows every one of build, test and
  lint"`, not `"all present"`. A vague question, "is this sufficient?",
  scores near 0.5 on everything; the same judgment written as three
  conditions over named fields scores 0.95 at 0.92 confidence.
- **A checklist is an ordinary `choice`** with one option per outcome:
  everything present, something missing, something contradictory. Write
  each option as the condition, and name the items in the wording.
- **A judgment Noul carries its criteria in the question**: what makes it
  true and what makes it false. There is no separate place to put them.
- **Rivals come from evidence, not from symmetry.** Offer an alternative
  because the state could support it. An option that argues for itself
  steers the answer; an option that merely describes its condition does
  not.
- **Give every choice an exit**: `#not_here`, `#none`, `#ask_model`, so
  "none of these" is an answer rather than a forced pick. An exit is worth
  more than a policy floor, because it is a branch you wrote.
- **Keys are model-facing**: name them by what choosing them means.

## Alternatives

An offer is a chain of alternatives, each with its label, its wording for
the provider, and its payload for the program:

```haskell
offers = alt #use_witness "The current span already answers the inquiry" (Witness "complete_request:41")
      .| alt #ask_model "Choosing needs a design preference beyond the evidence" (Handoff "preference")
      .| many (.edgeKey) (String . (.edgeText)) edges
```

`many key wording rows` is a runtime group: a wire key and a wording per
row, and the row itself is the payload the handler receives. Use it for
candidates computed at runtime: lines, edges, hypotheses, table rows. There
is nothing to dereference afterwards, because the handler already has the
row.

The chain's type is `"use_witness" ::> Witness :|: "ask_model" ::> Handoff :|: Many Edge`.
Give it a name when a helper wants to mention it in a signature; never for
the compiler's sake. `Handles hs alts` bundles the constraints `settle`,
`handle` and `contenders` need between a handler list and the alternatives
it answers, for a helper that wants to take either as a parameter.

Handlers follow declaration order. A label out of order, a handler missing
or extra, a label where `Many` stands, or parentheses inside a chain each
produce a compile error that says which label was expected.

## Rubrics

A score ranges over a chain of levels, in order, each with its wording for
the provider and the result `grade` returns when the score lands on it —
the same two things an alternative carries, beside its label:

```haskell
score "What is the consequence of waiting?"
  (  level #background   "No current action depends on this"        keepGoing
  .| level #checkpoint   "Useful at the next ordinary checkpoint"    noteIt
  .| level #blocked      "A worker cannot take its next action"     wakeSomeone
  .| level #invalidating "Continuing would invalidate ongoing work" stopEverything )
```

The levels' type is `"background" :|: "checkpoint" :|: "blocked" :|: "invalidating"`,
and the question's is `Score p levels` for whatever type the results have.
Duplicate labels are a compile error; one to ten levels is checked when the
request is built.

`grade floor answer` is how you act on one: the result written beside the
level the score landed on, the highest level whose mass at or above it
clears the floor, or the lowest when none does. At a floor of `0.5` that is
the median.

```haskell
grade 0.5 a.urgency
```

There is no list of results to keep in step with the rubric, so a result
cannot go missing, arrive twice, or land on the wrong level: it is the same
expression as the level's wording. That is the point. A rubric's labels are
known at compile time, so nothing should ever dispatch on them as strings,
and the way not to is to leave no string to dispatch on.

There is no `Doubt` here. An ordinal scale has a median even when the
distribution is flat, so `grade` always answers; read `confidence` yourself if
you want to gate on it. The answer also gives `expectation`, `masses` by
label, and `results` (every level's result, in level order) for the ledger,
plus `massAtOrAbove #blocked`, which sums the rubric from a level up when you
want the raw number rather than a branch.

A score is right only for a genuinely ordered, mutually exclusive
situation. Most judgments are not: reach for a Noul or a choice first.

## The per-item battery

When the question is "for each of these N things, ...", `each` asks a
question per item, keyed at runtime, in one call. It takes a key, a
question, and the rows, exactly as `many` does:

```haskell
#clauses := each (.key) (\c -> noul ("Does the draft satisfy " <> c.key <> "? " <> c.text)) clauses
```

`each`'s second argument is a question or a nested packet, exactly as a
cell does, so one question per item needs nothing around it and several per
item is the same call with a packet in it. The answers come back as
`[(row, answer)]`, the row itself beside what it answered — there is
nothing to look up afterward, the same property `many` already has. Each
item's wording is written where the question is, so a battery is an
ordinary fold over whatever list the program has. This is the highest-value
shape in the corpus: per-item questions catch things a single summary
question waves through.

## What is checked where

At compile time: label uniqueness and presence, handler lists against
alternatives, rubric label uniqueness, cell contents.

When the request is built, with a named `PrepError` inside `JevError`:
empty offers, duplicate or colliding runtime keys, wording and state shapes
the provider rejects, level counts, empty question maps.

When the response is decoded, with a named `DecodeError`: provider
rejections (parsed), missing, unexpected, or malformed answers, selections
outside the offered set, masses outside it, legends that differ from what
was sent, values out of range.

## Deliberately unsupported

The library covers what a program wants to express, not every request the
provider accepts. These shapes are left out on purpose; the test tree's
replay module renders them so recorded exchanges still round-trip.

- Wording for what yes and no mean on a Noul. Put it in the question.
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
  and let the handler run the winner. No dispatch table.
- **A choice picks one; a Noul each says how many.** A choice's
  distribution is uncertainty about which single alternative fits, not
  evidence that several apply. When things can be true at the same time,
  ask a Noul per thing with `each`, in the same packet, and judge each one.
  Reading a choice's runner-up mass as "this also applies" conflates
  doubt with multiplicity.
- **An optional question is a battery of none or one.** A question a node
  may or may not have does not need a separate packet shape: ask it with
  `each` over `maybe [] pure` of the optional thing. An empty `each`
  renders to nothing on the wire and decodes back to `[]`. `examples/Guard.hs`
  asks this way wherever a question only sometimes applies.
- **Rules in Haskell, judgments in Jev.** Decide eligibility before the
  call and offer only what is legal now; do not ask a Noul whether an
  alternative should be on offer. What the state cannot decide, a question
  does.
- **Two questions, one call, reconciled in code.** A branch choice and a
  Noul such as "does this reply admit to something the rules forbid" go in
  the same packet; the program takes the Noul's route when `judge merging`
  says yes and the chosen branch otherwise. Give the Noul a route only
  where there is somewhere to send the case; a tripwire with nowhere to go
  steals branches that mean something.
- **Wording moves the numbers more than anything else.** Rewriting one
  alternative in `examples/Guard.hs` from "cannot be squared with what the
  traveller said earlier" to a sentence naming the state field and the
  three concrete ways it could conflict moved that answer from mass 0.56 at
  0.34 confidence to mass 0.81 at 0.72, which was the difference between
  the program acting and the program doubting. Nothing about the types
  changed.
- **Ambiguity and absent evidence are different failures.** A `Doubt` means
  the provider was not clear. It does not mean the evidence was missing: a
  model can be confident and wrong because the state never carried what it
  needed. Ask that as its own question.
- **Frequency is a rule, not a wording.** "Which of these fits this moment,
  or none" with the same events fired never under "most moments, nothing
  does" and every time under neutral wording. Gate how often a question is
  asked in code; keep the wording about the judgment.
- **Carry the conversation.** Every call's state holds the whole exchange
  so far and whatever changed the world in between. A reply judged with its
  history is judged better than the same reply alone; the cost is a few
  thousand input tokens per call.
- **Retry at the transport.** The provider returns 529 under load. A
  transport that retries 5xx and 529 with backoff and passes every other
  body back lets the library decode real rejections.

## Habits that pay

- Ask one packet per semantic boundary; put every question the current
  evidence can answer into it.
- Offer an `ask_model` alternative when deciding may need judgment beyond
  the state.
- Record `explain` next to whatever the program did. It is the line a
  person reads when they ask why.
- Read `margin` and `contenders`, not just the winner; a near tie is a
  typed outcome, and so is a 1.0 that means nothing competed.
- Payloads are the only thing an action should run; never a key or wording.
- Bind a packet in the session and keep it: answers are ordinary values,
  and the labels make them legible in a later turn.
