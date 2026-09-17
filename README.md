# jev-dsl

Typed Haskell packets for [TypeSafe's Jev](https://docs.typesafe.ai): write a
packet of labelled questions once, get the exact request JSON out of it, and
get the same packet back with typed answers under the same labels, every
selection carrying the local payload it was offered with. The library does
no networking. Any transport that can post JSON and hand the body back will
do.

The library has two fronts over one core. Both produce the same wire.

| Module | Audience | Status |
|---|---|---|
| `Jev.Operators` | Agent use and review: anonymous packets, disjunctions and rubrics in the type, handler lists | implemented |
| `Jev.Records` | Human use and review: declared records, ordinary sums and enums, `case` | [designed](docs/records-dsl.md) |

`Jev.Core` is the shared core, polymorphic over the JSON type. `Jev`
re-exports the vocabulary common to both fronts. The model-facing guide
is [docs/authoring.md](docs/authoring.md). Every example below is compiled
by `test/Readme.hs`.

## The tiny use

One question, one answer, and either a retained payload or a handback.

```haskell
locate :: Transport -> Text -> [(Int, Text)] -> IO (Maybe Int)
locate transport source numbered = do
  answer <- jev1 transport jevLatest (stateText source)
    (choice @("not_here" ::> () :? "The branch is not in this file" :|: Many Int)
       "Which line begins the retry-timeout branch?"
       (#not_here () .| many [(T.pack (show n), String l, n) | (n, l) <- numbered]))
  pure $ case answer of
    Left _ -> Nothing
    Right a -> caseOf a (#not_here (\() -> Nothing) .| onMany (Just . elementPayload))

-- with, at top level:
type Transport = Value -> IO (Either Text Value)
```

The type application carries the description of the exit. Everything else
is inferred. The payload is a line number, never a string the model
produced.

## A packet

```haskell
type Routes = "use_witness" ::> Witness :? "The current span already answers the inquiry"
          :|: "ask_model"   ::> Handoff :? "Choosing needs a design preference beyond the supplied evidence"
          :|: Many Edge

type Urgency = '[ "background"   :? "No current action depends on this"
                , "checkpoint"   :? "Useful at the next ordinary checkpoint"
                , "blocked"      :? "A worker cannot take its next action"
                , "invalidating" :? "Continuing would invalidate ongoing work" ]

inspection edges =
     #next     := choice @Routes "Which available continuation advances the inquiry?"
                    (#use_witness (Witness "complete_request:41") .| #ask_model (Handoff "preference") .| many edges)
  :& #urgency  := score @Urgency "What is the consequence of waiting?"
  :& #children := each [ (k, #useful := noul ("Is " <> k <> " relevant to the inquiry?") :& Nil) | (k, _, _) <- edges ]
  :& #evidence := group (#enough := noul "Does the supplied evidence answer the inquiry?" :& Nil)
  :& Nil
```

The packet's type is inferred. A described disjunction is named once with a
type application, because its descriptions exist only in the type. Answers
come back under the same labels:

```haskell
act a =
  caseOf a.next
    (  #use_witness (\(Witness w) -> "located at " <> w)
    .| #ask_model   (\(Handoff h) -> "hand back: " <> h)
    .| onMany       (\e -> "follow " <> elementKey e) )
  <> (if massAtOrAbove #blocked a.urgency > 0.5 then " now" else " later")
  <> (if yesAbove 0.8 a.evidence.enough then ", evidence suffices" else "")
```

Offers and handlers follow declaration order. The compiler rejects a
misordered, missing, or mislabelled handler, and its message names the
label it expected. The same handler list eliminates any contender from
`ranked`, so keeping two explanations alive costs nothing extra:

```haskell
contenders a = [handle s routes | (mass, s) <- ranked a.next, mass > 0.25]
  where routes = #use_witness (const "witness") .| #ask_model (const "model") .| onMany elementKey
```

## Pools and premises

When several questions range over the same alternatives, declare them once
under the pool's own name:

```haskell
probing probes =
     #probes := probes
  :& #best   := choice @(Many Command :|: "none" ::> () :? "No probe discriminates")
                  "Which probe discriminates best?" (manyFrom probes .| #none ())
  :& #per    := eachIn probes (\r -> #useful := askAbout r "Does this probe help answer the inquiry?" :& Nil)
  :& #if_retry := given "the mechanism is retry redelivery"
                    (choice "Which probe confirms it?" (manyFrom probes))
  :& Nil
```

`probes = pool #probes [...]` is bound once. The state must be wrapped with
`pooled`. The wire then carries the descriptions once, under
`state.pools.probes`, with null descriptions at each use. `given` prefixes a
runtime premise to the instruction.

## What is checked, and where

**At compile time.** Label uniqueness and presence, alternative order in
offers and handlers, at most 255 static alternatives, 1 to 10 unique rubric
levels, and pools placed under their own name.

**At `prepare`.** Every builder is total, so shape checks on runtime values
happen here, each with a named `PrepError` carrying the question key. These
cover:

- empty offers,
- duplicate runtime keys, or runtime keys colliding with labels,
- description, instruction, level, and state shapes the provider rejects,
- runtime rubrics that do not match their labels,
- undeclared, conflicting, or nested pools, and pools without `pooled` state,
- empty question maps and ids, and duplicate flattened ids.

An empty runtime key is admitted, because the provider admits it.

**At `decodeResponse`.** Each failure is a named `DecodeError`. It rejects:

- a selection or a probability key outside the submitted set,
- values or confidence outside [0, 1],
- a legend that differs from the submitted levels,
- wrong answer kinds,
- missing or unexpected answers.

A provider rejection body is returned parsed. A rounded probability sum is
reported as a diagnostic on the response.

`rawUnchecked` is the one escape hatch, outside every guarantee.

Of 253 real accepted exchanges, 251 render as structurally identical JSON
through this library and decode against the retained request. The other two
carried request-level members the research harness added on purpose, which
the closed request spine does not express. Every captured rejection is
either inexpressible, rejected by `prepare` with a named error, or a
decision only the provider can make. See `test/fixtures/README.md` for
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
declares a pool of checks and references it from a choice, a per-check
Noul, and a premise-prefixed question. It also has a runtime group of
diagnostics, a described static disjunction with a handback, and a typed
rubric. `request` prints the request JSON. `decode` reads the response JSON
on stdin and prints what the typed answers say. `scripts/example.sh` puts
curl between them:

```sh
TYPESAFE_API_KEY=... ./scripts/example.sh
```

A live run on 2026-09-16 printed:

```
model: jev-1.13.0
explains: d2  "bookmark identity assertion failed after prefix insertion; observed old numerical offset"
  ranked: d2=1.0, d1=0.0, d3=0.0, no_match=0.0
next: read the implicated source  confidence 0.28
verify: prefix_insert
relevant: prefix_insert=0.94, bookmark.fuzz/quick=0.46, lookup_benchmark=8.0e-2, path_normalization=9.0e-2
sufficient: 0.37  (unsure)
breadth: 0.86  nearest adjacent, mass at or above adjacent 0.69
if flaky: bookmark.fuzz/quick
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
