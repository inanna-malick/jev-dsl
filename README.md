# jev-dsl

A Haskell DSL for models working in a stateful session. A packet is an
expression: a few labelled questions written once, whose type is inferred
from the questions themselves. Sent, it becomes the exact request JSON for
[TypeSafe's Jev](https://docs.typesafe.ai). Answered, it comes back under
the same labels as plain records you read by field — `a.next.key`,
`a.next.margin`, `a.enough.yes` — with every selection still carrying the
local payload it was offered with. Declarations are a liability in a
session, so there are none to write: no schema type, no instance, no
codec. The library does no networking; any transport that posts JSON and
hands the body back will do.

Written for an agent that has a Haskell session and a question it would
rather have judged than guessed, and for the person reviewing what that
agent wrote. The library covers what a program wants to express with Jev,
not every request the provider accepts. It is an early alpha, shaped by
having other models write with it and say what got in the way.

| Module | Audience | Status |
|---|---|---|
| `Jev.Operators` | Agent use and review: anonymous packets, inferred alternatives and rubrics, handler lists | implemented |
| `Jev.Records` | Human use and review: declared records, ordinary sums and enums, `case` | [designed](docs/records-dsl.md) |

`Jev.Transport` holds `request` and `decode` for a program that carries the
JSON itself; `Jev.Core` is the shared core, polymorphic over the JSON type.
The guide written for a model is [docs/authoring.md](docs/authoring.md).
Every example below is compiled by `test/Readme.hs`; the acceptance suite is
the five microprograms in `test/Corpus.hs`.

## The tiny use

One question, one answer, and either a retained payload or a handback.

```haskell
locate :: Transport -> Text -> [(Int, Text)] -> IO (Maybe Int)
locate transport source numbered = do
  answer <- ask1 transport jevLatest (state (String source))
    (choice "Which line begins the retry-timeout branch?"
       (alt #not_here "No line in this file begins that branch" () .| many [(T.pack (show n), String l, n) | (n, l) <- numbered]))
  pure $ case answer of
    Left _ -> Nothing
    Right a -> handle a.chosen (#not_here (\() -> Nothing) .| onMany (\_ n -> Just n))

-- with, at top level:
type Transport = Value -> IO (Either Text Value)
```

Everything is inferred from the offer. The payload is a line number, never
a string the model produced.

The answer is a record, and it displays:

```
> a
Choice {key = "142", mass = 0.78, margin = 0.61, confidence = 0.80, masses = ["142" 0.78, "137" 0.17, "not_here" 0.05]}
> a.key
"142"
> accept merging a
Left (Unconfident 0.8)
```

`key`, `mass`, `margin`, `confidence` and `masses` are fields, not
accessors to look up. `accept` weighs them against a policy and gives back
the selection or a named doubt; `handle` is for the branch that must run
the payload.

## A packet

```haskell
inspection edges =
     #next     := choice "Which available continuation advances the inquiry?"
                    (  alt #use_witness "The current span already answers the inquiry" (Witness "complete_request:41")
                    .| alt #ask_model "Choosing needs a design preference beyond the supplied evidence" (Handoff "preference")
                    .| many edges )
  :& #urgency  := score "What is the consequence of waiting?"
                    (  level #background "No current action depends on this"
                    .| level #checkpoint "Useful at the next ordinary checkpoint"
                    .| level #blocked "A worker cannot take its next action" )
  :& #children := each [ (k, #useful := noul ("Is " <> k <> " relevant to the inquiry?") :& Nil) | (k, _, _) <- edges ]
  :& #evidence := (#enough := noul "Does the supplied evidence answer the inquiry?" :& Nil)
  :& Nil
```

The packet's type is inferred. Signatures are optional; one shows what was
inferred:

```haskell
type Routes = "use_witness" ::> Witness :|: "ask_model" ::> Handoff :|: Many Edge
type Inspection = Packet
  '[ "next" ::= Choice Routes
   , "urgency" ::= Score ("background" :|: "checkpoint" :|: "blocked")
   , "children" ::= Each (Packet '[ "useful" ::= Noul ])
   , "evidence" ::= Group (Packet '[ "enough" ::= Noul ]) ]
```

Answers come back under the same labels, as records:

```haskell
report :: Inspection Answers -> Text
report a =
  a.next.key <> " by " <> pct a.next.margin
    <> ", urgency " <> a.urgency.nearest
    <> (if a.evidence.enough.yes > 0.8 then ", evidence suffices" else "")
  where pct x = T.pack (show (round (x * 100) :: Int)) <> "%"
```

A choice answers with `key`, `mass`, `margin`, `confidence` and `masses`;
a Noul with `yes`; a score with `nearest`, `expectation`, `confidence` and
`masses`. `toJSON` on any of them, or on a whole answers packet, is a
ledger row.

Handlers are for the other path: when the branch must run the payload the
alternative carried.

```haskell
act :: Inspection Answers -> Text
act a =
  handle a.next.chosen
    (  #use_witness (\(Witness w) -> "located at " <> w)
    .| #ask_model   (\(Handoff h) -> "hand back: " <> h)
    .| onMany       (\k _ -> "follow " <> k) )
  <> (if massAtOrAbove #blocked a.urgency > 0.5 then " now" else " later")
```

Handlers follow declaration order. The compiler rejects a misordered,
missing, extra, or mislabelled handler, and its message names the label it
expected. A handler list is a value, so the same list eliminates the winner
and every contender above a floor, or the winner under a policy:

```haskell
routes :: Handlers Text Routes
routes = #use_witness (const "witness") .| #ask_model (const "model") .| onMany (\k _ -> k)

alive :: Inspection Answers -> [Text]
alive a = [handle s routes | (_, s) <- contenders 0.25 a.next]

decide :: Inspection Answers -> Either Doubt Text
decide a = fmap (`handle` routes) (accept spawning a.next)
```

Three named policies cover the usual cases: `routing` for a read-only
choice, `spawning` for starting work, `merging` for anything with a
receipt.

## Pools and premises

When several questions range over the same alternatives, declare them once
under the pool's own name:

```haskell
probing probes =
     #probes   := probes
  :& #best     := choice "Which probe discriminates best?" (manyFrom probes .| alt #none "No probe discriminates" ())
  :& #per      := eachIn probes (\r -> #useful := askAbout r "Does this probe help answer the inquiry?" :& Nil)
  :& #if_retry := given "the mechanism is retry redelivery" (choice "Which probe confirms it?" (manyFrom probes))
  :& Nil

retryProbes = pool #probes [("run_retry", "Retries m42 and counts callbacks", Command "just test-target actor retry")]
```

The wire carries the wording once, under `state.pools.probes`, with null
wording at each use and the pool named beside each question that draws on
it. Two pools may reuse keys; a choice draws on one pool. `given` prefixes a
runtime premise.

## What is checked, and where

See [docs/authoring.md#what-is-checked-where](docs/authoring.md#what-is-checked-where)
for the full breakdown of compile-time, request-build, and decode-time checks.

Of 253 real accepted exchanges, 251 render as structurally identical JSON
and decode against the retained request through the core, the other two
carrying request-level members the research harness added on purpose.
Shapes the authoring surface leaves out (runtime rubrics, verbatim ids, raw
questions, omitted-versus-null criteria) are rendered by the replay module
in the test tree, not by the library. Every captured rejection is either
inexpressible, rejected with a named error before the request is built, or
a decision only the provider can make. See `test/fixtures/README.md` for
provenance.

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

`jev-dsl-example` triages a failing check in one packet. The packet
declares a pool of checks and draws on it from a choice, a per-check Noul,
and a premise-prefixed question. It also has a runtime group of
diagnostics, a static disjunction with a handback, and a rubric. `request`
prints the request JSON. `decode` reads the response JSON on stdin and
prints what the typed answers say. `scripts/example.sh` puts curl between
them:

```sh
TYPESAFE_API_KEY=... ./scripts/example.sh
```

## The gate at Greyhaven

`jev-dsl-guard` is a city guard at the gate, written as a catamorphism with
Jev for its algebra. The script is a dialogue tree written by hand in
`examples/Guard.hs`: what the guard asks, which kinds of reply it tells
apart, when it holds the traveller's story against the wanted posters, and
how it weighs the story at the end. Its three branching constructors are
Jev's three question kinds. A free-form reply is sorted into a branch by a
choice; the story is held against each poster by a Noul per poster over a
pool; the story is graded by a score on a three-level rubric. The branches
come from a small world value, so changing the roads into the city changes
what the guard asks.

Two folds run over the same tree. One is pure and prints the script. The
other builds a program: each node becomes a `Play` that says its line,
reads a reply, makes one Jev call, and continues into whichever child Jev
chose. The child's continuation rides inside the Jev alternative as its
payload, so there is no routing code:

```haskell
putStr (cata render (gate world))
outcome <- cata (interpret world transport) (gate world) (Traveller [])
```

Nothing is generated at run time. The author wrote every line and every
branch; Jev only decides which branch a reply takes, which poster matches,
and whether the story holds up. Rules stay in Haskell: an unbonded weapon
turns a traveller away without any weighing, and only travellers from the
north road or bound for the taverns are checked against the posters.

```sh
scripts/guard.sh --script                 # print the tree, no network
TYPESAFE_API_KEY=... scripts/guard.sh     # play it, one call per node visited
```

A conversation on 2026-09-17, four calls and about three thousand input
tokens:

```
guard: Halt. Where do you hail from, traveller?
you:   The north road
       [heard north_road 100%]
guard: And what brings you to Greyhaven?
you:   Looking for a room at the Broken Wheel, then I move on at first light
       [heard tavern 99%]
guard: Anything to declare? Weapons, goods, anything the customs officer should see?
you:   Only my satchel. Personal things. Heavy, I know, I have been walking a long way
       [heard nothing 96%]
       [posters thief 68%, deserter 20%]

guard: Guards! Hold this one. Someone fetch the captain.
```

A pilgrim on the same road, "bound for the temple, my daughter is in the
infirmary there", matched the thief poster at 14% and was weighed sound at
73%. "Work. I heard the watch is hiring since the robbery" was heard as
barracks at 99%. "My sword. I am not handing it over to anyone" was heard
as an unbonded weapon and turned away by rule. "That is my own affair" was
heard as evasive at 100%, and the weighing then called the story thin. The
transport is `scripts/transport.sh`, a curl call that keeps the key out of
every Haskell process.

## Building

`nix develop` or `nix-shell` gives GHC 9.12 with every dependency, cabal,
and curl; both read the same pinned nixpkgs from `flake.lock`. Then:

```sh
cabal build all --enable-tests && cabal test
./check.sh
```

`check.sh` also compiles `examples/` and `test/Readme.hs`, and builds both executables. It then requires
every `test/reject/*.hs` to fail to compile at its own site with the
diagnostic phrase the file names. Set `JEV_EVIDENCE_DIR` to a directory of
research captures to run the full generic pass over every recorded success.

MIT.
