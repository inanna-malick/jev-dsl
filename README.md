# jev-dsl

A Haskell DSL for agents that would rather have a question judged than
guessed. A packet of labelled questions is written once as an expression;
its type is inferred, it renders to the exact request JSON for
[TypeSafe's Jev](https://docs.typesafe.ai), and the answers come back under
the same labels as records read by field. Each answer is consumed under a
policy, through a handler for every alternative, so the branch that runs is
always one the program wrote and always carries the payload it was offered
with. No schema, no instance, no codec, no network.

**Early alpha.** The interface is still moving, and this repository is the
only place it has been used. See [Status](#status) for what is unsettled.

`Jev.Operators` is the whole surface, and the guide written for a model is
[docs/authoring.md](docs/authoring.md). `Jev.Core` is the same library
polymorphic over the JSON type, for an environment that cannot load aeson.
Every example below is compiled by `test/Readme.hs`.

Jev answers three kinds of question. A **Noul** is a proposition, answered
with one probability. A **choice** picks one of a set of labelled
alternatives. A **score** grades something on an ordered rubric. Nothing
else is generated: the model never writes a string your program runs.

## Locate

One question, one call, and either a payload the program offered or a
handback.

```haskell
locate :: Transport -> Text -> [Line] -> IO (Either Text Int)
locate transport source numbered = do
  answer <- ask1 transport jevLatest (state (String source))
    (choice "Which line begins the retry-timeout branch?"
       (alt #not_here "No line in this file begins that branch" () .| many (T.pack . show . (.lineNo)) (String . (.lineText)) numbered))
  pure $ case answer of
    Left err -> Left (T.pack (show err))
    Right a -> case settle routing a (#not_here (\() -> Nothing) .| onMany (\_ l -> Just l.lineNo)) of
      Right (Just n) -> Right n
      Right Nothing -> Left "not in this file"
      Left _ -> Left (explain routing a)

-- with, at top level:
type Transport = Value -> IO (Either Text Value)
```

`settle` is the only way to consume a choice, and it gives no result
without a handler for every alternative. The handler receives the row the
program offered, so there is nothing to look up afterwards, and a confident
"not here" runs its own branch instead of reading as a pass. When the
policy is not met, `explain` says why in the line a log or a planner reads:

```
> a
Choice {key = "142", mass = 0.78, margin = 0.61, confidence = 0.80, masses = ["142" 0.78, "137" 0.17, "not_here" 0.05]}
> explain merging a
"doubted 142 (Unconfident): confidence 0.80 < 0.85 by 0.05; mass 0.78, margin 0.61"
```

Three policies are named for how bad it is to be wrong: `routing` for a
read-only choice, `spawning` for starting work, `merging` for anything with
a receipt. The same three apply to a Noul through `judge`, which returns
yes, no, or the same structured doubt.

## Route

Several questions about one situation go in one packet and one call. The
packet's type is inferred from the questions.

```haskell
inspection edges =
     #next     := choice "Which available continuation advances the inquiry?"
                    (  alt #use_witness "The current span already answers the inquiry" (Witness "complete_request:41")
                    .| alt #ask_model "Choosing needs a design preference beyond the supplied evidence" (Handoff "preference")
                    .| many (.edgeKey) (String . (.edgeText)) edges )
  :& #enough   := noul "Does the supplied evidence answer the inquiry?"
  :& #children := each [ (e.edgeKey, #useful := noul ("Is " <> e.edgeKey <> " (" <> e.edgeText <> ") relevant to the inquiry?") :& Nil) | e <- edges ]
  :& #evidence := (#gap := noul "Does answering require source that was not supplied?" :& Nil)
  :& Nil
```

A cell holds a question or a nested packet. Nesting flattens to dotted wire
keys and reads back through the labels, so `a.evidence.gap` is the answer to
the question written above.

`each` is the per-item battery: one sub-packet per item, keyed at runtime,
in the same call. Per-item questions catch what a single summary question
waves through, and the list is whatever the program already has.

Answers come back under the same labels, and every one is consumed under a
policy:

```haskell
act :: Inspection Answers -> Text
act a =
  case settle spawning a.next
         (  #use_witness (\(Witness w) -> "located at " <> w)
         .| #ask_model   (\(Handoff h) -> "hand back: " <> h)
         .| onMany       (\k _ -> "follow " <> k) ) of
    Right step -> step <> (if judge routing a.enough == Right True then "; evidence suffices" else "")
    Left _ -> "stopped: " <> explain spawning a.next
```

A choice answers with `key`, `mass`, `margin`, `confidence` and `masses`; a
Noul with `yes`; a score with `expectation`, `confidence` and `masses`.
Those are for logs and thresholds. Dispatch goes through the branches
instead, one typed consumer per question kind: `settle` for a choice,
`judge` for a Noul, `grade` for a score. Each takes the branches in
declaration order, and the compiler rejects a misordered, missing, extra, or
mislabelled one with a message naming what it expected.

```haskell
grade 0.5 a.urgency
  (level #background keepGoing .| level #checkpoint noteIt .| level #blocked wakeSomeone)
```

`grade` runs the result for the level the score landed on, the highest whose
mass at or above it clears the floor. A rubric's labels are known at compile
time, so nothing has to dispatch on them as strings.

A handler list is a value. The same list eliminates the winner or every
contender above a floor:

```haskell
routes :: Handlers Text Routes
routes = #use_witness (const "witness") .| #ask_model (const "model") .| onMany (\k _ -> k)

alive :: Inspection Answers -> [Text]
alive a = [handle s routes | (_, s) <- contenders 0.25 a.next]
```

Signatures are optional. This one is what the compiler inferred for
`inspection`:

```haskell
type Routes = "use_witness" ::> Witness :|: "ask_model" ::> Handoff :|: Many Edge
type Inspection = Packet
  '[ "next" ::= Choice Routes
   , "enough" ::= Noul
   , "children" ::= Each (Packet '[ "useful" ::= Noul ])
   , "evidence" ::= Group (Packet '[ "gap" ::= Noul ]) ]
```

## The gate at Greyhaven

`examples/Guard.hs` is what the library can carry: a city guard at a gate,
played against a person typing, as a catamorphism whose algebra is Jev.

The script is a tree written by hand, a fixed point of `GuardF`. Four of
its constructors are Jev questions. `Ask` sorts a free-form reply into one
of the branches the author wrote, and in the same packet asks a second
question: whether the reply admits to something, contradicts the story so
far, or raises nothing new. Those three alternatives carry different
continuations, so the handler that runs is the one the author wrote for
that outcome. `Check` holds the story against each wanted poster, one Noul
per poster in one call. `Weigh` grades the story on a three-level rubric.
`Happen` lets Jev pick which of six authored events fits the moment, or
none. The branches come from a small world value; the
guard's lines, the posters, and the events are data. The tree is rational:
the hubs after each verdict are tied back into themselves, so the
conversation runs until the player leaves, runs, or the captain arrives.

Two folds run over the same tree. `render` prints it, naming each hub once.
`interpret` builds a program: every node becomes a `Play` that says its
line, makes one call, and continues into the child Jev chose. The child's
continuation rides inside the Jev alternative as its payload, so there is
no routing code, and the fold is productive over the infinite tree because
no child is forced until the player takes that branch.

```haskell
putStr (snd (cata render (gate world) []))
outcome <- cata (interpret transport) (gate world) (Traveller [] [] [] world)
```

Rules stay in Haskell: smuggled goods are turned away without any weighing,
a held traveller can talk their way down to the road but never through the
gate, and something can happen at most every other exchange. Jev decides
everything that needs judgment. Nothing is generated at run time.

The guard also shows where a policy belongs and where it does not. Sorting
a reply into a branch uses `handle` with no floor, because every branch is
a legal next line and the guard follows the winner. Everything with a cost
goes through `settle` or `judge` under a named policy, and the policy's own
`explain` line is what the transcript records. Stopping a traveller
mid-sentence and holding one against a poster both use `spawning`, since
each starts something and neither is final. Firing an event uses `routing`.
When a policy doubts an answer the guard lets it pass, which is why a
`Doubt` reads as "let it pass" rather than as a stop.

```sh
scripts/guard.sh --script                 # print the tree, no network
TYPESAFE_API_KEY=... scripts/guard.sh     # play it
```

A session on 2026-09-17, sixteen calls, twenty thousand input tokens:

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
`explain` printing the policy that let the guard interrupt, with the three
floors it cleared. `thin or worse` is `massAtOrAbove` summing the rubric
from a level up, which is how the guard decides whether a story is merely
vague or actually broken.

Every question also takes the ways people talk at a guard instead of
answering: nonsense or play-acting, a question back, flattery, dropping the
captain's name, slurring drunk, a hard-luck story before anything was
asked, or not understanding at all. Each is a branch with its own retort
and the question again, twice at most, then it counts as evasive. A threat
closes the gate; a bribe fetches the captain. The hubs take the same.
Prompt injection, role reversal, gibberish, one-word answers, and
contradictions across turns were each tried and sorted where a person would
put them.

A reply that asks two things gets two answers. A choice cannot say so on
its own: its distribution is uncertainty about which single branch fits,
not evidence that several do. Things that can be true at the same time are
a Noul each, so the hub packet carries one per topic beside the branch
choice, all in the same call, and every topic that clears the read-only
policy gets its line. The fold's carrier is a program plus the line its
subtree opens with, which is what makes that answer available to the
parent, and the guard remembers what it has already said so no line comes
out twice.

The guard errs on the forgiving side. A story that does not hold up gets
one plain re-ask, and after it a thin story is let through under a warning;
a sword is bonded at the post and the talk goes on; only contraband, a
poster match, a bribe, or a story that contradicts itself twice ends the
night badly. A sellsword off the north road, "looking for work, anything
really", was re-asked once and went through with "I've got my eye on you".

In other sessions a traveller off the north road with "only my satchel,
heavy" matched the thief poster at 68% and was held; "Look, there is a
silver piece in it for you" tripped the wire at 81% from the turned-away
hub; "Work. I heard the watch is hiring" was heard as the barracks at 99%;
and a traveller who answered the first question with "Why do you need to
know" was asked once more, then moved on with the story marked. An ordinary
chatty visit runs about eight exchanges, fourteen calls, and fifteen
thousand input tokens. The transport is `scripts/transport.sh`, a curl call
that keeps the key out of every Haskell process and retries overloads.

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

`Jev.Operators` fixes the JSON type to aeson's `Value`. `Jev.Core` is the
same library polymorphic over a small `JsonValue` class and imports only
`base`, `containers`, and `text`. It can be copied into an environment that
cannot load aeson. Write an instance for your value type and a facade like
`src/Jev/Operators.hs`.

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
alternatives, and exhaustive handlers. What is not:

- **Handlers are positional.** Inserting or reordering an alternative
  breaks its handler lists, with a compile error that names the label.
  Appending is safe. Label-keyed handlers would fix it and cost machinery;
  no measured program has needed them yet.
- **The policy floors are plausible, not calibrated.** Three named policies
  cover the cases seen so far. The numbers come from judgment about the
  cost of being wrong, not from measurement.
- **State is untyped.** A packet builds its state from one `Value` and
  names the state's fields again inside wording, and nothing checks that
  the two agree. This is the most error-prone part of authoring. A checked
  field reference is the likely fix.
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
