# jev-dsl

Typed Haskell packets for [TypeSafe's Jev](https://docs.typesafe.ai): write a
packet of labelled questions once, get the exact request JSON out of it, and
get the same packet back with typed answers under the same labels, every
selection carrying the local payload it was offered with. The library does
no networking. Any transport that can post JSON and hand the body back will
do.

The library covers what a program wants to express with Jev, not every
request the provider accepts. It is an early alpha, shaped by having other
models write with it and say what got in the way.

| Module | Audience | Status |
|---|---|---|
| `Jev.Operators` | Agent use and review: anonymous packets, inferred alternatives and rubrics, handler lists | implemented |
| `Jev.Records` | Human use and review: declared records, ordinary sums and enums, `case` | [designed](docs/records-dsl.md) |

`Jev.Core` is the shared core, polymorphic over the JSON type. The guide
written for a model is [docs/authoring.md](docs/authoring.md). Every example
below is compiled by `test/Readme.hs`; the acceptance suite is the five
microprograms in `test/Corpus.hs`.

## The tiny use

One question, one answer, and either a retained payload or a handback.

```haskell
locate :: Transport -> Text -> [(Int, Text)] -> IO (Maybe Int)
locate transport source numbered = do
  answer <- jev1 transport jevLatest (state (String source))
    (choice "Which line begins the retry-timeout branch?"
       (alt #not_here "The branch is not in this file" () .| many [(T.pack (show n), String l, n) | (n, l) <- numbered]))
  pure $ case answer of
    Left _ -> Nothing
    Right a -> handle (chosen a) (#not_here (\() -> Nothing) .| onMany (\_ n -> Just n))

-- with, at top level:
type Transport = Value -> IO (Either Text Value)
```

Everything is inferred from the offer. The payload is a line number, never
a string the model produced.

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

Answers come back under the same labels:

```haskell
act :: Inspection Answers -> Text
act a =
  handle (chosen a.next)
    (  #use_witness (\(Witness w) -> "located at " <> w)
    .| #ask_model   (\(Handoff h) -> "hand back: " <> h)
    .| onMany       (\key _ -> "follow " <> key) )
  <> (if massAtOrAbove #blocked a.urgency > 0.5 then " now" else " later")
  <> (if yes a.evidence.enough > 0.8 then ", evidence suffices" else "")
```

Handlers follow declaration order. The compiler rejects a misordered,
missing, extra, or mislabelled handler, and its message names the label it
expected. A handler list is a value, so the same list eliminates the winner
and every contender above a floor, or the winner under a policy:

```haskell
routes :: Handlers Text Routes
routes = #use_witness (const "witness") .| #ask_model (const "model") .| onMany (\key _ -> key)

alive :: Inspection Answers -> [Text]
alive a = [handle s routes | (_, s) <- contenders 0.25 a.next]

decide :: Inspection Answers -> Either Doubt Text
decide a = fmap (`handle` routes) (accept (Policy 0.4 0.15 0.5) a.next)
```

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

**At compile time.** Label uniqueness and presence, handler lists against
their alternatives, unique rubric levels, pools placed under their own
name, cell contents.

**When the request is built.** Every builder is total, so shape checks on
runtime values happen here, each a named `PrepError` carrying the question
key: empty offers; duplicate runtime keys, or runtime keys colliding with
labels; wording, level, and state shapes the provider rejects; one to ten
levels; undeclared, conflicting, nested, or duplicate-keyed pools, and two
pools in one choice; duplicate structured members, through any premise;
empty question maps and ids, and duplicate flattened ids.

**When the response is decoded.** Each failure is a named `DecodeError`: a
selection or a probability key outside the submitted set, values or
confidence outside [0, 1], a legend that differs from the submitted levels,
wrong answer kinds, missing or unexpected answers. A provider rejection
body is returned parsed. A rounded probability sum is a diagnostic, not a
rejection.

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

## Building

```sh
cabal build all --enable-tests && cabal test
./check.sh
```

`check.sh` also compiles `examples/` and `test/Readme.hs`. It then requires
every `test/reject/*.hs` to fail to compile at its own site with the
diagnostic phrase the file names. Set `JEV_EVIDENCE_DIR` to a directory of
research captures to run the full generic pass over every recorded success.

MIT.
