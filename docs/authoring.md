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
only where you write a `Field Value a` instance for a state field's own
type, or reach for `rawState`. Wording is plain `Text` everywhere it is
written — `alt`, `many`, `level`, `noul`, `choice`, `score` all take it with
no wrapper: `alt #rerun "Rerun the focused check" c` sends the string.

## The three questions

A **Noul** is a proposition. The answer is one probability, `yes`.

A **choice** is a set of labelled alternatives, one of which comes back,
carrying the payload your program offered it with.

A **score** grades something on an ordered rubric of one to ten levels. The
answer is a distribution over the levels and its expectation.

## One question

```haskell
sess = session transport jevLatest
answer <- ask1 sess (state (#source := source))
  (choice "Which line begins the retry-timeout branch?"
     (alt #not_here "The branch is not in this file" () .| many #lines (T.pack . show . (.lineNo)) (.lineText) lines))
case answer of
  Right a -> case settle lenient a (#not_here (\() -> handBack) .| #lines (\_ l -> editAt l.lineNo l.revision)) of
    Right (Settled result) -> result
    Left d -> stop d.why
  Left err -> ...
```

`ask1` is a whole packet with one question under the label `value`.
`session transport jevLatest` is the session `ask1` takes; `transport ::
Value -> m (Either Text Value)` is anything that posts JSON and hands the
body back. The alternative type was inferred from the offer: `"not_here"
::> () :|: "lines" ::* Line`. The payload is what the program acts on; the
model only ever sees the wording.

## Packets

```haskell
packet =
     #next     := choice "Which continuation advances the inquiry?" offers
  :& #enough   := noul "Does the supplied evidence answer the inquiry?"
  :& #urgency  := score "What is the consequence of waiting?" urgency
  :& #children := each (.key) (\e -> noul ("Is " <> e.key <> " (" <> e.text <> ") relevant?")) edges
  :& #evidence := (#gap := noul "Is source missing?")
```

A cell holds a question or a nested packet, and a cell is already a packet
of one, so two of them join with the same `:&` and nothing terminates the
list — there is no `Nil`. That also makes a shared set of questions an
ordinary value: `common :& #next := choice …` sends both. A packet's *type*
is written the same way its value is, a chain rather than a list:
`Packet ("next" ::= Choice Alts :& "enough" ::= Noul)`; nobody writes this
by hand, but a helper's signature can name it. To read the type back off an
expression, ask for it at a mode — `packet :: Packet _ Questions` — since a
packet on its own stays polymorphic in whether it holds questions, answers
or state fields, and the unpinned type is unreadable. Under a `let` in a session,
keep `:&` at the start of each continuation line and indent every line past
the first, as above; a `:&` left at the end of a line, or a continuation
line starting in the same column as the binding, ends the expression early.
Labels are the wire keys; nested packets flatten to dotted paths with dots
in keys escaped, so a label may be anything. A duplicate label is a compile
error naming it.

`ask sess state packet` returns a `Response`. It has no record
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

## State

`state` takes a field packet, written the same way a question packet is,
and keeps each field's own Haskell type:

```haskell
world = state
  (  #failure := inputs.failure
  :& #diagnostics := [Diagnostic k t | (k, t) <- inputs.diagnosticLines]
  :& #checks := [Check k t | (k, t) <- inputs.checks] )
```

A field may be `Text`, `Bool`, `Int`, `Double`, a list, a list of
`(Text, a)` (renders as an object), a `Maybe`, a nested state packet, a raw
`Value`, or any type with a `Field Value a` instance you write. A list of a
row type of your own needs the instance on the list itself:

```haskell
instance Field Value [Diagnostic] where
  toField ds = object [Key.fromText d.diagnosticKey .= d.diagnosticText | d <- ds]
```

Read a field back with record dot — `world.checks :: [Check]`, nested as
`world.gate.posters` — and build the rows a `many` or `each` offers from
the state itself, so the rows the program acts on are the rows the
provider was shown:

```haskell
many #checks (.checkKey) (.checkText) world.checks
```

Wording that names a field goes through `field`, not a string that happens
to match:

```haskell
noul ("Do " <> field #diagnostics world <> " alone establish the mechanism of " <> field #failure world <> "?")
```

`field #k st` renders the key in backticks — `` `diagnostics` `` — and a
name the state does not have is a compile error listing the names it does.
Nested states read back with record dot (`world.gate.posters`); name one
in wording with a path:

```haskell
field (#gate :/ #posters) world
```

`:/` associates right, so deeper paths read `#gate :/ #watch :/ #captain`.
Every segment is checked. Intermediate fields must be nested state packets;
the final field may have any type. The result above is `` `gate.posters` ``.
Dots and backslashes inside individual labels are escaped with a backslash.
These are references in model-facing wording, distinct from question wire
keys even though the escaping convention is the same.

The model's interpretation of nested references is **not yet validated**.
The measured evidence that field references improve answers was collected
on top-level names; compile-time path checking makes no claim about that
semantic effect.

For a state shape the surface leaves out — a bare string, say — `Jev.Core`
still has `rawState :: v -> State v ()`, whose fields cannot be named by
`field` or record dot. Reach for it only when nothing above fits.

## Acting on answers

A choice or Noul can be consumed under a policy. A policy is three floors, and
three are named for how bad it is to be wrong:

| Policy | For | mass | margin | confidence |
|---|---|---|---|---|
| `lenient` | read-only choices: which file, which skill | 0.40 | 0.08 | 0.50 |
| `careful` | starting a worker, or choosing an approach | 0.55 | 0.20 | 0.70 |
| `strict` | merging, stopping, anything with a receipt | 0.70 | 0.40 | 0.85 |

`settle policy answer handlers` consumes a choice, returning
`Either Doubt (Settled p r)`: `Right (Settled r)` through the handler for
the alternative that won, or `Left` a `Doubt`.

```haskell
case settle careful a.next
       (  #rerun (\c -> run c)
       .| #ask_model (\h -> handBack h)
       .| #edges (\_ e -> follow e) ) of
  Right (Settled action) -> action
  Left d -> stop d.why
```

`settle` requires a handler for every alternative, including each runtime
group. A confident "none of these" runs its own handler. `Right` means the
selected alternative cleared the policy, not that the answer was approval;
inspect the result your handler returned.

The verdict carries the policy that reached it, so a function that must not
be handed a lightly-settled answer can demand one in its own signature:

```haskell
merge :: Settled Strict Patch -> IO ()     -- a careful verdict will not typecheck here
```

`Doubt` is a record, `Doubt { cause :: Cause, why :: Text }`, where `Cause`
is `NearTie`, `Underweight`, or `Unconfident`. `why` is the same line
`explain` prints for a doubt, so a doubt branch does not need to call
`explain` again — read it straight off: `Left d -> stop d.why`. Matching on
the reason itself reads `Left Doubt { cause = NearTie {} } -> ...`.

`judge policy answer` does the same for a Noul: `Either Doubt (Settled p
Bool)`, so `Right (Settled True)`, `Right (Settled False)`, or `Left`
a `Doubt`. A Noul weighs yes against no, so the same three policies apply;
there is no confidence on the wire for a Noul, so that floor is not
consulted.

`holds policy answer` is the same question asked as a `Bool`, for the
common case of a Noul in a guard or a list comprehension:

```haskell
[e.edgeKey | (e, n) <- a.relevant, holds lenient n]
```

It is `True` only for a settled yes. A doubt is not a no, and both read as
`False` here, which is the whole reason it is a separate verb: comparing a
verdict for equality (`judge p n == Right (Settled True)`) silently turns
every doubt into a no. Use `judge` wherever the doubt is worth acting on or
worth a line in the log, and `holds` where the answer is a filter.

`explain policy answer` says in one line which check settled or doubted the
answer and the numbers behind it, on a *settled* answer — a `Chosen` or a
`Yes`, not a verdict. It works on a choice or a Noul, and it is usually
worth recording next to whatever the program did.

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

When every alternative carries the same type — usually what to do next —
consume its payload without repeating the branches:

| | Labelled handlers | Uniform payloads |
|---|---|---|
| Without a policy | `handle answer handlers` | `taken answer` |
| Under a policy | `settle policy answer handlers` | `takenUnder policy answer` |

`takenUnder policy answer` returns `Either Doubt (Settled p r)`, with the
same checks and exact doubt explanations as `settle`. A uniform chain is
built by giving every alternative a payload of one type, including the
exit: `alt #none "No matching row" Nothing .| mapCarried Just (many ...)`. The selected payload
is the one authored beside its wording. Every alternative has a payload;
the author defines its meaning. For example, `Nothing` can explicitly mean
no diversion, as in the Guard's tripwire. A settled `Nothing` is not
approval of some other action.

`Carries alts r` lets the compiler infer `r` from `alts`; the new consumer
needs no result annotation merely to read a field off its payload.

The rest of an answer is fields, read with record dot:

| Question | Fields |
|---|---|
| `choice` | `key`, `mass`, `margin`, `confidence`, `masses` (best first) |
| `noul` | `yes` |
| `score` | `expectation`, `confidence`, `masses` (by level, in order) |

Those are all there is to read. An answer cannot be built or matched, and
its type is what a helper's signature names: `Chosen alts` for a choice,
`Yes` for a Noul, `Scored p levels` for a score.

Choices use the four consumers above; `judge` consumes a Noul and `grade`
a score. Labelled handlers cover every alternative; uniform choices carry
a result beside every alternative instead. `grade`
needs no such list: the result it returns was written beside the level's own
wording when the rubric was built.

`key` is for logs and ledgers, never for dispatch: a `case` on it is
unchecked, and the compiler cannot tell you when the alternatives change.
`margin` is the winner's mass less the runner-up's, and equals the mass
when nothing competes. A margin at or near 1.0 means no other option was in
play, usually a sign the alternatives were not really rivals.

Record dot needs the field selectors in scope, so importing `Jev.Operators`
unqualified takes some short names for itself. The fields: `key`, `mass`,
`margin`, `confidence`, `masses`, `yes`, `expectation`, `cause`, `why`. The
verbs: `ask`, `ask1`, `alt`, `many`, `level`, `each`, `optional`, `state`, `field`,
`session`, `settle`, `takenUnder`, `judge`, `holds`, `grade`, `graded`, `handle`,
`explain`, `taken`, `offered`, `branches`, `uniform`, `withUniform`,
`lenient`, `careful`, `strict`. Under `-Wall` a local
binding with any of these names shadows; name your own `tag`, `weight`,
`askLine`, or import qualified. `field` and `taken` are the likeliest
collisions — a record of your own is likely to want either name.
`optional` also conflicts with `Control.Applicative.optional`; use an
explicit import list or qualify one of the modules when using both.

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
      .| many #edges (.edgeKey) (.edgeText) edges
```

`many label key wording rows` is a runtime group: a label, then a wire key
and a wording per row, and the row itself is the payload the handler
receives. Use it for candidates computed at runtime: lines, edges,
hypotheses, table rows. There is nothing to dereference afterwards, because
the handler already has the row.

The chain's type is `"use_witness" ::> Witness :|: "ask_model" ::> Handoff
:|: "edges" ::* Edge`. Give it a name when a helper wants to mention it in
a signature; never for the compiler's sake. `Handles hs alts` bundles the
constraints `settle`, `handle` and `contenders` need between a handler list
and the alternatives it answers, for a helper that wants to take either as
a parameter.

Handlers are found by their label, not by position, so they may be written
in any order and a group is handled through its label exactly as any other
alternative is — `onMany` is gone:

```haskell
settle careful a.next
  (  #edges       (\k _ -> "follow " <> k)
  .| #use_witness (\(Witness w) -> "located at " <> w)
  .| #ask_model   (\(Handoff h) -> "hand back: " <> h) )
```

Adding an alternative in the middle of a chain breaks nothing that already
handles the others by label. A missing handler, an extra one, or a
duplicate is a compile error naming the label.

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

`graded floor answer` returns the same result with the label of the level
it landed on, `(Text, p)`, for a ledger line that names the level. Without
it a program that wants both writes the label a second time into the
result, and the two copies can drift.

```haskell
let (landed, next) = graded 0.5 a
```

There is no list of results to keep in step with the rubric, so a result
cannot go missing, arrive twice, or land on the wrong level: it is the same
expression as the level's wording. That is the point. A rubric's labels are
known at compile time, so nothing should ever dispatch on them as strings,
and the way not to is to leave no string to dispatch on.

There is no `Doubt` here. An ordinal scale has a median even when the
distribution is flat, so `grade` always answers; read `confidence` yourself if
you want to gate on it. The answer also gives `expectation` and `masses` by
label for the ledger, plus `massAtOrAbove #blocked`, which sums the rubric from a level up when you
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
alternatives, rubric label uniqueness, cell contents, state field names.

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
- Structured, non-`Text` wording: an object, array, or `Null` sent where a
  question or an alternative's wording goes. `Jev.Core` still carries it,
  and `test/Replay.hs` renders it for the recorded exchanges that need it.

Each returns when a program in `test/Corpus.hs` needs it.

## Patterns

Each of these is used in `examples/Guard.hs`, a dialogue tree folded by a
catamorphism whose algebra is Jev.

- **The continuation is the payload.** When every branch carries the same
  type — usually the next step to take — offer them with their
  continuations as payloads and read the winner with `taken`, or
  `takenUnder policy` when a floor is needed. Neither needs a handler list.
- **`Uniform` is for a node type that is itself a functor over such
  payloads.** `examples/Guard.hs`'s `GuardF` holds its branches in
  `Uniform r`, which hides the chain's own type so the node type stays a
  plain functor. `uniform` builds one, `mapUniform` is its `fmap`, and
  `branches` lists every alternative as key, wording and payload for a
  fold that prints or inspects the tree. To *ask* one, open it with
  `withUniform u $ \o -> ... choice "..." o ...`: the alternatives are
  existential, so they are named only inside, and the question built there
  gets every check a written-out chain gets.
- **A choice picks one; a Noul each says how many.** A choice's
  distribution is uncertainty about which single alternative fits, not
  evidence that several apply. When things can be true at the same time,
  ask a Noul per thing with `each`, in the same packet, and judge each one.
  Reading a choice's runner-up mass as "this also applies" conflates
  doubt with multiplicity.
- **An optional question is a `Maybe`.** Write
  `#stop := optional (stopping <$> trip)`. `optional` takes a `Maybe` question
  or nested packet and returns a `Maybe` answer at the same cell label.
  `Nothing` sends no questions; `Just` uses the cell's path directly, with
  no synthetic `now` segment. A present answer must still pass decoding.
  Missing questions do not count as answers: a response containing a key
  for an absent question is rejected. A request with no questions at all
  still fails with `EmptyQuestionMap`. Presence comes from the retained
  question, so a present packet with zero leaves still returns `Just` its
  empty answers.
- **Rules in Haskell, judgments in Jev.** Decide eligibility before the
  call and offer only what is legal now; do not ask a Noul whether an
  alternative should be on offer. What the state cannot decide, a question
  does.
- **Two questions, one call, reconciled in code.** A branch choice and a
  Noul such as "does this reply admit to something the rules forbid" go in
  the same packet; the program takes the Noul's route when `holds careful`
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
