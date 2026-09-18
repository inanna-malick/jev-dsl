# jev-dsl

A Haskell DSL for agents that would rather have a question judged than
guessed. A packet of labelled questions is written once as an expression;
its type is inferred, it renders to the exact request JSON for
[TypeSafe's Jev](https://docs.typesafe.ai), and the answers come back under
the same labels as records read by field. A choice is consumed through
exhaustive labelled handlers or its carried payload, with a policy when
needed. The branch that runs is one the program wrote and carries the
payload it was offered with. No schema, no instance, no codec, no network.

**Early alpha.** The interface is still moving, and this repository is the
only place it has been used. See [Status](#status) for what is unsettled.

`Jev.Operators` is the whole surface, and the guide written for a model is
[docs/authoring.md](docs/authoring.md). `Jev.Core` is the same library
polymorphic over the JSON type, for an environment that cannot load aeson.
Every example below is compiled, by `test/Readme.hs` or as part of
`examples/Guard.hs`.

## What Jev is

[Jev](https://docs.typesafe.ai) is a hosted judgment model from TypeSafe.
It is not a chat model and there is no prompt. You send it a **state** —
whatever the situation is, as JSON — and a **map of labelled questions**
about that state, and it answers all of them in one call, as probabilities
rather than prose.

There are three kinds of question and nothing else:

| Kind | Asks | Comes back as |
|---|---|---|
| **Noul** | a proposition | one probability that it holds |
| **choice** | which one of these labelled alternatives applies | the winner, a mass for every alternative, and a confidence |
| **score** | where this falls on an ordered rubric of one to ten levels | a mass per level and an expectation |

That is the whole vocabulary. It is a system-1 shape: fast judgment over a
situation you supply, with no generation step. Three things follow, and
they are why a typed DSL is worth building over it rather than over a chat
completion.

**Nothing comes back to parse.** The answer to a choice is one of the keys
you sent, and no answer is free text, so there is no format to coax and no
retry loop for malformed output. It does not make the state trustworthy —
text in the state can still steer which alternative wins — but it bounds
the damage: a hostile state can move the answer among the branches you
wrote, never introduce one you did not.

**The numbers are the product, not a by-product.** A choice at 0.78 with a
runner-up at 0.17 is a different situation from the same winner at 0.78
with a runner-up at 0.74, and a program can act on that difference. This is
what the policies in this library are: three named floors over mass,
margin and confidence, so "the model said yes" becomes "the model said yes
strongly enough for a step this expensive".

**One call answers many questions about one situation.** The state is sent
once and every question sees it, so asking twenty per-item questions beside
a summary question costs one round trip, not twenty-one. That changes which
questions are worth asking: per-item batteries stop being expensive, and
they are where most of the value turns out to be.

What Jev will not do is also the point. It will not write your code,
summarize a document, plan a task, or produce anything you would have to
read and trust. It judges what you put in front of it, against options you
wrote. Everything else stays in your program.

Requests here go to `jev-latest`, which resolved to `jev-1.13.0` when this
was built; see [Contract pin](#contract-pin) for exactly what was observed
and when.

## Who this is shaped for

The expected author is a language model, and that is the reason for the
type-level machinery rather than an accident of taste. Three things follow
from it, and the rest of the design is downstream of them.

**A model knows Haskell's type vocabulary better than it knows your
library.** Type families, `DataKinds`, overloaded labels and record dot are
in its weights already; your combinator names are not. So the DSL spends
its complexity on inference and spends none on API surface to memorize.
There is one import, no class to instantiate, no schema to declare, and no
codec to write. A packet is an expression, and its type is whatever the
questions in it imply:

```haskell
type Routes = "use_witness" ::> Witness :|: "ask_model" ::> Handoff :|: "edges" ::* Edge
type Inspection = Packet
  (  "next"     ::= Choice Routes
  :& "enough"   ::= Noul
  :& "children" ::= Each Edge Noul
  :& "evidence" ::= Group (Packet ("gap" ::= Noul)) )
```

Nobody wrote that. It is what the compiler inferred from the packet in
[Route](#route) below, and it is printable, so a model that has lost track
of a value's shape can ask the compiler instead of guessing. Ask for it at
a mode — `:t (packet :: Packet _ Questions)` — because a packet written on
its own is still polymorphic in whether it holds questions, answers or
state fields, and that is not the shape anyone wants to read.

**The compile error is the feedback loop, so it is the most-read text in
the library.** A person reads an error once and remembers; a model reads
one, edits, and compiles again, so the error has to say what to write. Each
is phrased in the author's own vocabulary, not the type checker's:

```
no handler for #edges
#nope has no alternative; the alternatives are #use_witness, #ask_model, #edges
duplicate handler #use_witness
Jev: this packet has no #urgency; it has #enough, #more
Jev: this state has no #failure; it has #source
Jev: a cell holds a question or a nested packet; this is Int
```

`test/reject/` is a directory of programs that must fail to compile, each
naming the sentence it expects. The messages are a tested interface, not a
courtesy.

**Tokens spent on structure are tokens not spent on wording, and wording is
what moves the answers.** The measured difference between a packet answered
well and one answered at 0.5 everywhere is in how the questions are
written, never in the types. So the surface is arranged to make the
structural part nearly free: a cell is a packet of one and two packets
join, so there is no terminator and no separate builder; handlers are found
by label, so they can be written in any order and an alternative added in
the middle breaks nothing; the state keeps its Haskell types, so the rows a
question offers are the rows the provider was shown, written once.

What the model may emit is bounded by construction. It picks among
alternatives the program authored, and each alternative hands back a
payload the program chose — a continuation, a row, a handback — never a
string the program then interprets.

## Locate

One question, one call, and either a payload the program offered or a
handback.

```haskell
locate :: Session IO -> Text -> [Line] -> IO (Either Text Int)
locate sess source numbered = do
  answer <- ask1 sess (state (#source := source))
    (choice "Which line begins the retry-timeout branch?"
       (alt #not_here "No line in this file begins that branch" ()
        .| many #lines (T.pack . show . (.lineNo)) (.lineText) numbered))
  pure $ case answer of
    Left err -> Left (T.pack (show err))
    Right a -> case settle lenient a (#not_here (\() -> Nothing) .| #lines (\_ l -> Just l.lineNo)) of
      Right (Settled (Just n)) -> Right n
      Right (Settled Nothing) -> Left "not in this file"
      Left d -> Left d.why
```

`settle` consumes a choice, and it gives no result without a handler for
every alternative. The handler receives the row the program offered, so
there is nothing to look up afterwards, and a confident "not here" runs its
own branch instead of reading as a pass. A verdict carries the policy that
reached it, so a step that must not be taken lightly can demand one:

```haskell
merge :: Settled Strict Patch -> IO ()     -- a lenient verdict will not typecheck here
```

When the policy is not met there is no verdict, only the reason, already
written out in the line a log or a planner reads:

```
> a
Choice {key = "142", mass = 0.78, margin = 0.61, confidence = 0.80, masses = ["142" 0.78, "137" 0.17, "not_here" 0.05]}
> d.why
"doubted 142 (Unconfident): confidence 0.80 < 0.85 by 0.05; mass 0.78, margin 0.61"
```

Three policies are named for how bad it is to be wrong: `lenient` for a
read-only choice, `careful` for starting work, `strict` for anything with a
receipt. The same three apply to a Noul through `judge`, which returns yes,
no, or the same structured doubt.

## Route

Several questions about one situation go in one packet and one call. The
packet's type is inferred from the questions.

```haskell
inspection edges =
     #next     := choice "Which available continuation advances the inquiry?"
                    (  alt #use_witness "The current span already answers the inquiry" (Witness "complete_request:41")
                    .| alt #ask_model "Choosing needs a design preference beyond the supplied evidence" (Handoff "preference")
                    .| many #edges (.edgeKey) (.edgeText) edges )
  :& #enough   := noul "Does the supplied evidence answer the inquiry?"
  :& #children := each (.edgeKey) (\e -> noul ("Is " <> e.edgeKey <> " (" <> e.edgeText <> ") relevant to the inquiry?")) edges
  :& #evidence := (#gap := noul "Does answering require source that was not supplied?")
```

A cell holds a question or a nested packet, and a cell is already a packet,
so two of them join with the same operator and nothing terminates the list.
That also means a shared set of questions is an ordinary value:
`common :& #next := choice …` sends both, and a label written twice is a
compile error naming it. Nesting flattens to dotted wire keys and reads
back through the labels, so `a.evidence.gap` is the answer to the question
written above.

`each` is the per-item battery, written like `many`: a key, a question per
row, and the rows. Its second argument is a question or a nested packet,
exactly as a cell is, so a single question needs no packet around it. The
answers come back paired with the row that produced them, so there is
nothing to look up afterward — the same property `many` already has.
Per-item questions catch what a single summary question waves through.

Answers come back under the same labels, ready to consume under a policy.
Handlers are found by their label, so they need not follow the order the
alternatives were written in:

```haskell
act :: Inspection Answers -> Text
act a =
  case settle careful a.next
         (  #edges       (\k _ -> "follow " <> k)
         .| #use_witness (\(Witness w) -> "located at " <> w)
         .| #ask_model   (\(Handoff h) -> "hand back: " <> h) ) of
    Right (Settled step) -> step <> (if holds lenient a.enough then "; evidence suffices" else "")
    Left d -> "stopped: " <> d.why
```

A choice answers with `key`, `mass`, `margin`, `confidence` and `masses`; a
Noul with `yes`; a score with `expectation`, `confidence` and `masses`.
Those are for logs and thresholds. Dispatch goes through the branches
instead: `settle` or `takenUnder` for a choice under a policy, `judge` for
a Noul, `grade` for a score. A missing, extra, duplicated or misspelled
handler is a compile error naming the label. A score has no handler list to
get wrong: the result is written right beside its wording when the rubric
was asked.

Where a Noul is a filter rather than a decision, `holds policy answer` is
the same judgment as a `Bool`. It is `True` only for a settled yes, because
a doubt is not a no — which is exactly what comparing a verdict for
equality would quietly make it.

```haskell
(score "What is the consequence of waiting?"
   (  level #background "No current action depends on this" Background
   .| level #checkpoint "Useful at the next ordinary checkpoint" AtCheckpoint
   .| level #blocked "A worker cannot take its next action" Now ))
```

```haskell
grade 0.5 a
```

`grade` returns the result written beside the level the score landed on:
the highest level whose mass at or above it clears the floor, or the lowest
when none does. `graded` returns that result with the level's own label, so
a ledger line that names the level does not need the label written into the
result a second time.

When every alternative carries the same kind of thing — usually what to
do next — consume that payload directly. Adding a policy need not add a
handler list:

```haskell
takenUnder :: Carries alts r => Policy p -> Chosen alts -> Either Doubt (Settled p r)
```

| Choice consumption | Labelled handlers | Uniform payloads |
|---|---|---|
| Without a policy | `handle answer handlers` | `taken answer` |
| Under a policy | `settle policy answer handlers` | `takenUnder policy answer` |

Every alternative has a payload, including the exit. The author assigns
its meaning: `Nothing` may mean no action, but a successful policy check
only supports the selected alternative, not permission to proceed. In the
Guard, the stop alternatives carry `Just continuation` or `Nothing`, and
`takenUnder careful` returns that decision without repeating the branches.

## State, written once

A state is written the way a packet is, and keeps its Haskell types:

```haskell
world = state
  (  #failure := inputs.failure
  :& #diagnostics := [Diagnostic k t | (k, t) <- inputs.diagnosticLines]
  :& #checks := [Check k t | (k, t) <- inputs.checks] )
```

The rows a question offers then come from the state itself, so what the
program acts on and what the provider was shown cannot drift apart:

```haskell
many #checks (.checkKey) (.checkText) world.checks
```

and wording that names a field names it through the compiler, not through a
string that happens to match:

```haskell
noul ("Do " <> field #diagnostics world <> " alone establish the mechanism of " <> field #failure world <> "?")
```

`field` renders the key in backticks the way the provider reads it, and a
name the state does not have is a compile error listing the names it does.
Nested states read back through record dot: `world.gate.posters`. Name the
same nested field in wording with a checked path:

```haskell
field (#gate :/ #posters) st
```

Each segment is checked against its packet. This renders `gate.posters`
in backticks; its effect on model answers has not yet been measured.

## What it is for

The work an agent system does badly is the work where a rule cannot decide
and a free-form model answer cannot be trusted. Four shapes cover most of
it, and each is one packet.

### Gate the step that cannot be taken back

Merging, deploying, closing a ticket, spending money. The failure mode is
not a wrong answer, it is a *confident-sounding* answer accepted because
nobody asked how confident. Give the gate a policy and make the function
that acts demand a verdict from it:

```haskell
merge :: Settled Strict Patch -> IO Receipt
```

`settle strict` or `takenUnder strict` gives that result its policy tag.
A `lenient` result does not directly fit the signature. The tag documents
the chosen policy; it is not a provenance or authorization boundary. When
the floor is not met there is no verdict at all, only `d.why`, which is the
line to put in the log next to whatever the program did instead.

### Ask whether the evidence is actually sufficient

The highest-value question in an agent system is rarely "what should I do";
it is "does what I have in hand support doing anything yet". A Noul with
its criteria in the wording answers that, and the answer is calibrated
enough to be worth gating on.

```haskell
#sufficient := noul ("Do " <> field #diagnostics world <> " alone establish the mechanism of " <> field #failure world <> "?")
```

In this repository's own dogfooding, a sufficiency gate over three code
reviews passed none of them when it was shown compact summaries: two came
back as doubts at 0.26 and 0.34 confidence, and the third said plainly that
more was needed, at 0.69 mass. Shown the actual declarations and diffs the
reviewers had read, the same gate passed all three, at 0.78 to 0.92 mass
and 0.67 to 0.89 confidence. The gate was right both times — the summaries
really had dropped the evidence — and the operator did not override it.
That is the argument for this library in one measurement: the model was not
asked to be smarter, it was asked a question whose answer the program could
act on.

### One question per item, not one question about the list

A single summary question ("are these checks relevant?") is answered at 0.5
and waves through exactly the item you cared about. `each` asks one
question per row, in the same call, and hands each answer back beside the
row that produced it:

```haskell
#relevant := each (.checkKey) (\c -> noul ("Does the check `" <> c.checkKey <> "` exercise the code path " <> field #failure world <> " names?")) world.checks
```

This is the highest-value shape in the corpus. Clause-by-clause review of a
draft, file-by-file blast radius, precondition-by-precondition readiness:
all the same call. A choice would be wrong here — a choice's distribution
is uncertainty about which single row fits, not evidence that several do.

### Sort free-form input into branches the program wrote

A reply, a ticket, a log line, a user's message. The branches are authored,
and each one carries the thing to do next, so the answer *is* the
dispatch — there is no table from a key to a handler and no string the
program interprets:

```haskell
choice "Which branch does this reply take?"
  (  alt #refund "Asks for money back, in any words" refundFlow
  .| alt #status "Asks where an existing order is" statusFlow
  .| alt #other  "Anything the two above do not cover" handBack
  .| many #accounts (.accountId) (.accountSummary) knownAccounts )
```

Give every such choice an exit (`#other`, `#none`, `#ask_model`). An exit is
worth more than a policy floor, because it is a branch you wrote rather
than a failure you have to handle.

### Rules in Haskell, judgments in Jev

The line to hold: decide eligibility in code and offer only what is legal
now; never ask whether an option should be on the menu. What the state
cannot decide, a question decides. Frequency is a rule too — gate how often
you ask in code, and keep the wording about the judgment.

## The gate at Greyhaven

`examples/Guard.hs` is the stress test rather than the pitch: a city guard
at a gate, played against a person typing, as a catamorphism whose algebra
is Jev. It is here because a dialogue tree exercises every shape at once,
and because an adversarial human is a cheap source of inputs nobody
designed for. Prompt injection, role reversal, flattery, bribery,
gibberish, one-word answers, and contradictions across turns were each
tried and sorted where a person would put them.

The script is a hand-written tree, a fixed point of `GuardF`, and four of
its constructors are Jev questions: sort a reply into one of the authored
branches, hold the story against each wanted poster (one Noul per poster,
one call), grade the story on a three-level rubric, and pick which of six
authored events fits the moment, or none. Two folds run over it. One prints
the script; the other builds a program in which every node says its line,
makes one call, and continues into the child Jev chose.

A hub is written as its branches, each beside the node it leads to, and
that is the whole of the routing:

```haskell
askLine "The gate's closed to you tonight. Unless you've something to add." Nothing $ uniform
  (  alt #explain "Adds to their story, gives a reason, or names someone who can vouch for them"
       (say "Go on, then. All of it, from the start." weigh)
  .| alt #bribe "Offers money, a favour, or anything of value to the guard"
       (say "Did you just offer the watch coin? At its own gate?" (verdict SendForCaptain))
  .| alt #leave "Gives up, says goodbye, or turns to go" (say "Then go. The road's that way." end)
  ... )
```

There is no table from a key to a node and no case on a string: the
alternative the provider picks hands the program the node written beside
it. The branches that come from the world are the same chain with a runtime
group in it, and the tree is rational, so the hubs after each verdict tie
back into themselves and the conversation runs until the player leaves,
runs, or the captain arrives.

Every hub is one packet and one call. The branch choice, a Noul per topic
the reply may also raise, and the tripwire that would stop the traveller
where they stand go in one request. `optional` omits an absent tripwire
from the wire and reads it back as `Nothing`, with no second packet shape:

```haskell
r <- must =<< ask sess st
  (  #branch := choice "Which branch does the traveller's reply take?" o
  :& #also   := alsoQ
  :& #stop   := optional (stopping <$> trip) )
```

The state every call sends is one typed packet, and the node that hears a
reply sends that packet plus the reply, because packets join:

```haskell
situation t = state (theGate t)
exchange t line reply = state (theGate t :& #question := line :& #reply := reply)
```

Rules stay in Haskell — contraband is turned away without any weighing, a
held traveller can talk their way down to the road but never through the
gate, and something can happen at most every other exchange. Jev decides
everything that needs judgment, and nothing is generated at run time.

```sh
scripts/guard.sh --script                 # print the tree, no network
TYPESAFE_API_KEY=... scripts/guard.sh     # play it
```

A historical session on 2026-09-17, sixteen calls, twenty thousand input
tokens. This predates `optional` and nested state references; it is not a
capture of the current Guard request:

```
guard: Halt. Where do you hail from, traveller?
you:   The farmlands
       [heard farmlands 100%]
guard: And what brings you to Greyhaven?
you:   Turnips for the market
       [heard market 99%]
guard: Anything to declare? Weapons, goods, anything the customs officer should see?
you:   Nothing, just the cart
       [heard nothing 100%]
       [weighed sound  (sound 99%, thin 1%, false 0%; thin or worse 1%)]
guard: Right. You're in.
guard: Anything else before you go through?
you:   Which way to the temple, and when does the bell go?
       [heard temple 97%]
       [also asked curfew 67%]
guard: Indoors by the second bell. The watch won't ask twice tonight, not after the counting house.
guard: Left at the well, follow the bells. The infirmary's round the back.
guard: Anything else?
you:   Good thing you did not check under the turnips, there is a cask of brandy the customs man never saw
       [settled on admits: confidence 0.96 ≥ 0.70, mass 0.98 ≥ 0.55, margin 0.96 ≥ 0.20]
guard: Wait. Say that again.
       [weighed false  (sound 8%, thin 42%, false 50%; thin or worse 92%)]
guard: Hm. That doesn't quite hang together. Once more, plainly: what brings you in, and what have you got with you?
you:   Turnips. Just turnips, and the brandy is my own, for the cold
       [heard changes_story 70%  (also straight 25%)]
       [weighed false  (sound 2%, thin 27%, false 71%; thin or worse 98%)]
guard: Not tonight. Move along, and don't let me see you at this gate again.
guard: The gate's closed to you tonight. Unless you've something to add.
```

The bracketed lines are the program's own ledger. `settled on admits` is
the policy that let the guard interrupt, with the three floors it cleared.
`thin or worse` is `massAtOrAbove` summing the rubric from a level up,
which is how the guard decides whether a story is merely vague or actually
broken. An ordinary chatty visit runs about eight exchanges, fourteen
calls, and fifteen thousand input tokens. The transport is
`scripts/transport.sh`, a curl call that keeps the key out of every Haskell
process and retries overloads.

## What is checked, and where

See [docs/authoring.md#what-is-checked-where](docs/authoring.md#what-is-checked-where)
for the full breakdown of compile-time, request-build, and decode-time
checks.

Seventy-one real exchanges ship in `test/fixtures`, curated from research
captures; `test/fixtures/README.md` records their provenance. Every
captured success re-renders as structurally identical JSON and decodes
against the retained request through the core, and every captured rejection
is either inexpressible, rejected with a named error before the request is
built, or a decision only the provider can make. Shapes the authoring
surface leaves out (runtime rubrics, verbatim ids, raw questions,
omitted-versus-null criteria) are rendered by the replay module in the test
tree, not by the library. Setting `JEV_EVIDENCE_DIR` to a directory of
research captures runs the same generic pass over the full private set, 253
accepted exchanges at last count, of which 251 render identically and two
carry request-level members the research harness added on purpose.

## Bring your own JSON type

`jev-core` is the library polymorphic over a small `JsonValue` class. It is
a separate library that depends on `base` and `text` and nothing else, so it
cannot reach aeson and a program that depends on it never pays for one. That
is the build system's guarantee, not a comment's.

`jev-dsl` is `jev-core` plus one `JsonValue` instance for aeson's `Value`
and `Jev.Operators`, a facade that pins the value type so wording literals
and inference behave. Depend on `jev-dsl` if aeson is what you have.

For another JSON type, depend on `jev-dsl:jev-core` and supply two
instances. `JsonValue` is six constructors and a view, and `jEqual` has a
structural default you can leave alone unless your numbers are exact.
`IsString` is the other one: wording is written as a bare literal, so
without it the authoring surface does not read the way it does here.

A facade like `src/Jev/Operators.hs` is optional. It buys pinned inference
and hides the core's internals, and it is worth writing for a type you
author against daily, but the core's own verbs take the value type as a
parameter and can be driven directly. `test/Mini.hs` is a second value type,
structurally unlike aeson's, exercised end to end through the core with no
facade at all: it builds a request, decodes a response, and runs `handle`,
`judge` and `grade` over the answers.

## Contract pin

Built against the behavior observed on 2026-09-16. Requests to `jev-latest`
resolved to `jev-1.13.0`, and the public OpenAPI was 0.2.0. A Choice takes
1 to 255 alternatives and a Score 1 to 10 levels. The question map is
bounded only by token limits. Undocumented question kinds are excluded.
Structured content is admitted wherever the provider admits it.

## A worked example, end to end

`jev-dsl-example` triages a failing check in one packet: a runtime group
over diagnostics, a static disjunction with a handback, a per-check
battery, a Noul, and a rubric. Every choice is settled under a policy and
every Noul judged, with `explain` printed beside each. `request` prints the
request JSON. `decode` reads the response JSON on stdin and prints what the
typed answers say. `scripts/example.sh` puts curl between them:

```sh
TYPESAFE_API_KEY=... ./scripts/example.sh
```

## Status

What is settled: the packet, the three question kinds, payload-carrying
alternatives, handlers found by label, and a state whose fields are
checked. What is not:

- **The policy floors are plausible, not calibrated.** Three named policies
  cover the cases seen so far. The numbers come from judgment about the
  cost of being wrong, not from measurement.
- **Nested field references are not model-validated yet.** The compiler
  checks `field (#gate :/ #posters) st`, but the measured wording improvements
  behind this design came from top-level references. Nested references need
  their own live evaluation.
- **The operators have to be read, even though they are never written.**
  `::=`, `:&`, `::>`, `::*`, `:|:`, `:/` and `:-` appear in inferred types
  and in error messages, so an author who prints a type meets all of them.
  Nothing on the authoring surface requires writing one; the cost is
  reading, and it is real.
- **Two checks still report in the type checker's voice.** Offering
  alternatives that carry different types, or asking `taken` for a type the
  chain does not carry, gives GHC's own `No instance for Carries ...`;
  handling a runtime group with a one-argument function reports a mismatch
  between the key and the row. Both name the label and the types, but
  neither is a sentence in the author's vocabulary, unlike every other
  check here. Both are decided before the library's own messages can run.
- **Scores are rarely the right shape.** Most judgments are not ordered and
  exclusive. Reach for a Noul or a choice first.

Issues and observations are welcome at
[the tracker](https://github.com/inanna-malick/jev-dsl/issues), especially
from anything that has actually written a packet.

## Building

`nix develop` or `nix-shell` gives GHC 9.12 with every dependency, cabal,
and curl; both read the same pinned nixpkgs from `flake.lock`. Then:

```sh
cabal build all --enable-tests && cabal test
./check.sh
```

`check.sh` builds with `-Werror` (the `strict` flag, off by default so a
future GHC's new warnings cannot break a downstream build), compiles
`examples/` and `test/Readme.hs`, and builds both executables. It then
requires every `test/reject/*.hs` to fail to compile at its own site with
the diagnostic phrase the file names.

MIT.
