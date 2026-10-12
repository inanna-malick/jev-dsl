# jev-dsl

Typed semantic queries for Haskell, using [TypeSafe’s Jev](https://docs.typesafe.ai).
Ask whether evidence supports a claim, select a candidate, or route a reply into
an authored continuation. The answers retain the Haskell values you supplied:
rows, domain values, functions, or actions.

Compose queries with `Applicative` and `Traversable`, adapt assessments with
`Star`, edit payloads with optics, and choose evaluation strategies with
`Selective`. The library builds and validates requests, groups questions that
share a state, and decodes responses against the original alternatives. You
supply the transport and run any selected actions.

**Early alpha, 0.2.0.0.** Used by the examples here and Exomonad’s hosted Haskell
interface. Start with `Jev.Operators` for Aeson, or `Jev.Core` for your own JSON
type. See the [authoring guide](docs/authoring.md) for the API and the
[composition guide](docs/composition.md) for evaluation and replay contracts.

## Compose assessments over your data

An assessment combines a projection into evidence with a query over that
evidence. Here, `reportState` exposes a report’s claim and supporting material:

```haskell
assessReport = J.assessment reportState $ \world ->
  J.judge J.careful <$> J.question world #supported
    (J.noul ("Does " <> J.field #evidence world <> " support " <> J.field #claim world <> "?"))

reviewPending = traverseOf (traversed . _2)
  (runStar (lmap (view helperReport) assessReport))
```

`assessReport` is a standard `Star` arrow from a report to a query result.
`lmap` lets it accept a helper containing a report; the traversal applies it to
the second element of each pair, preserving the identifiers. The result is one
composed query whose answers keep that structure. Complete definitions and
imports are in [Reflex.hs](examples/Reflex.hs).

`runQuery` prepares the whole query before sending anything. Questions with
structurally equal JSON states share a request; different states produce
separate requests, sent sequentially. Repeated local labels such as `#supported`
receive distinct occurrence paths. `pure` and empty traversals make no calls.
Use `prepareQuery` and `queryGroups` to inspect the requests first.

This works with ordinary `traverse` too. Use optics when they help reach the
data, and `do` notation in the host monad when an answer determines what to
read or do next.

## Return the value you offered

Each choice alternative pairs its wording with a local payload. This example
selects a source line and returns its number, or `Nothing` when no line fits:

```haskell
selectLine sess world numbered = do
  answer <- ask1 sess world (choice "Which line begins the branch?"
    (alt #not_here "No line begins it" Nothing
     .| mapCarried (Just . (.lineNo)) (many #lines (T.pack . show . (.lineNo)) (.lineText) numbered)))
  pure (fmap (takenUnder lenient . answers) answer)
```

`many` supplies candidates from runtime data. `mapCarried` changes their local
payloads while preserving the keys and descriptions sent to Jev. `takenUnder`
returns the selected payload if the answer clears the policy, or a `Doubt`.
Transport, preparation, and decoding errors remain in the outer `Either`.
The complete example is in [Readme.hs](test/Readme.hs).

Payloads need no JSON instance. A choice can carry an actor instruction, a
continuation, or an `IO` action; decoding selects the original value without
executing it. If alternatives carry different types, supply exhaustive handlers
matched by label:

| | Labelled handlers | Payloads of one type |
|---|---|---|
| Select the winner | `handle answer handlers` | `taken answer` |
| Require a policy | `settle policy answer handlers` | `takenUnder policy answer` |

Adding an alternative requires a corresponding handler. Missing, extra, and
duplicate handlers are compile errors. Dynamic candidates share the handler for
their `many` group, which receives the selected key and original row.

## Ask several questions about the same state

Jev accepts a JSON state and a map of questions. The DSL supports three kinds:

| Question | Use | Answer |
|---|---|---|
| `noul` | Test a proposition | Probability that it holds |
| `choice` | Select one of the offered alternatives | Selected alternative, weights, confidence |
| `score` | Judge an ordered rubric | Weights by level, expectation, confidence |

A packet combines questions and nested packets. Its type is inferred from the
expression; the response uses the same field labels:

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

Here `next` selects one continuation, `enough` tests the evidence, and `children`
checks each edge independently. `each` returns each original row beside its
answer. This is useful when several items may apply: a choice selects one,
while a Noul per item can identify several.

Send a packet with `ask`, or embed it in a larger query with `packet world`.
Read answers as `a.next`, `a.enough`, and `a.evidence.gap`. Shared question sets
compose with `:&`; duplicate labels are rejected. `optional` accepts a `Maybe`
question or packet and omits absent questions from the request.

State fields are typed too. `field #evidence world` checks that the field exists
and renders its name for use in question wording. Nested paths use
`field (#gate :/ #posters) st`. See [Readme.hs](test/Readme.hs) for the packet’s
types, state definitions, and handlers.

## Choose which questions to run

Some questions are useful only after another answer. A `Program` expresses that
with the standard `Selective` interface. This example asks whether the evidence
is complete and, if needed, selects a source to expand:

```haskell
inspectOrContinue :: J.State ReportState -> [(Text, Text, IO ())] -> J.Program (IO ())
inspectOrContinue world sources = ifS
  (J.liftQuery (J.holds J.careful <$> J.question world #complete (J.noul "Does the evidence suffice?")))
  (pure (pure ()))
  (J.liftQuery (J.withUniform repertoire $ \offers ->
    J.taken <$> J.question world #expand (J.choice "Which source would resolve what is missing?" offers)))
  where
    repertoire = J.uniform (J.alt #pause "No source resolves the gap" (pure ()) J..|
      over (J.payloads #source) (\(_,_,action) -> action)
        (J.many #source (\(k,_,_) -> k) (\(_,w,_) -> w) sources))
```

`payloads #source` is a type-changing traversal over the runtime group. Here it
keeps the candidate keys and wording but replaces each tuple with its action.
It also composes with lenses into your own payload records. `Uniform` supports
`Functor`, `Foldable`, and `Traversable` when you want to transform or inspect
an entire set of alternatives.

Choose the interpreter’s strategy with `runProgram`:

- **`SkipUnused`** asks the condition first and only asks for an expansion if
  needed. Unselected branches are neither prepared nor sent.
- **`BatchKnown`** prepares all statically known questions and groups those with
  equal states. In this example, both questions fit in one call, at the cost of
  asking about expansion even when the evidence is complete.

Both return the selected action as a value. The caller decides when to execute
it. Acquisition that depends on an answer stays in the host’s `IO`, effect
program, or actor. The full example is in [Reflex.hs](examples/Reflex.hs).

## Compare policies without another call

`lenient`, `careful`, and `strict` check mass, margin, and confidence where
applicable. `customPolicy` constructs checked custom thresholds. `judge` applies
a policy to a Noul and distinguishes yes, no, and doubt; `holds` reduces that to
a filter that is true only for a settled yes. `explain` reports the checks and
numbers. These thresholds are presets, not calibrated error guarantees.

For decisions with explicit costs, retain all alternatives with `choiceWeights`
or `carriedWeights`. `noulWeights` and `scoreWeights` do the same for the other
question kinds. Normalization is explicit:

```haskell
comparePolicies :: J.Weights Hypothesis
  -> Either J.DistributionError (Next, [(Double, Hypothesis)])
comparePolicies raw = do
  distribution <- J.normalize raw
  explore <- J.covering 0.9 distribution
  let loss ReadMore MissingEvidence = 1
      loss ReadMore Contradiction = 8
      loss Recheck MissingEvidence = 4
      loss Recheck Contradiction = 1
      chosen = if J.expected (loss ReadMore) distribution <= J.expected (loss Recheck) distribution
        then ReadMore else Recheck
  pure (chosen, explore)
```

This selects the lower expected-loss action and a set of hypotheses covering
90% of normalized mass. Both use the same observation. `normalize` checks finite,
nonnegative weights and a positive finite total; it preserves the raw weights.
`covering` returns the smallest prefix in descending weight order, breaking
ties in authored order. Domain types are defined in [Reflex.hs](examples/Reflex.hs).

For ordered rubrics, `grade` selects the payload of a level by cumulative mass;
`graded` also returns its label. Use a rubric when the outcomes have a meaningful
order, such as urgency or severity.

## Retain observations and replay them

`runQuery` and `runProgram` return an `Evaluation`: a typed result or failure,
plus observations from the calls actually attempted. Each observation retains
the request, occurrence mapping, raw transport outcome, and decoded metadata
when available. A later failure preserves the successful prefix and failed
attempt. Previews do not evaluate captured functions or actions.

| Change | What to do |
|---|---|
| Thresholds or loss function | Apply another policy to the retained typed answer |
| Local payloads or continuations | Prepare the same request with new payloads and use `replayQuery` |
| State, wording, model, or offered alternatives | Make a new request |

Replay checks complete structural equality of requests before decoding the
recorded responses through the new plan. It rejects missing, extra, or changed
requests. `replayProgram` applies the same rule under its selected strategy.
See the [composition guide](docs/composition.md#replay-precisely) for details.

## Types and validation

The library is designed for LLM-authored Haskell as well as hand-written code.
It uses standard Haskell abstractions so an author can combine assessments with
existing functions and optics. Packets infer their types; explicit signatures
are useful for reusable interfaces. Compiler errors name the relevant labels:

```text
no handler for #edges
#nope has no alternative; the alternatives are #use_witness, #ask_model, #edges
duplicate handler #use_witness
Jev: this state has no #failure; it has #source
```

Compile-time checks cover static labels, field references, and handler coverage.
Preparation checks runtime keys, wording, state shapes, and provider limits.
Decoding checks the response against the prepared request, including unknown
selections and malformed answers. See
[what is checked where](docs/authoring.md#what-is-checked-where).

These checks establish structural validity. They do not establish that a
judgment is correct or that an action is authorized. Evidence and wording affect
the result; adding questions to a batch can also change live judgments.

## Use the library

The examples use GHC2021 with `DataKinds`, `OverloadedLabels`,
`OverloadedStrings`, and, where needed, `OverloadedRecordDot` and `TypeOperators`.
Import `Jev.Operators` for the Aeson interface; qualified imports are useful
alongside libraries that export names such as `field`, `optional`, or `result`.

A session takes a transport of type `Value -> m (Either err Value)` and a model.
Construct it with `session transport jevLatest`, then use `ask`, `runQuery`, or
`runProgram`. The library has no HTTP dependency. The example transport lives in
[scripts/transport.sh](scripts/transport.sh).

For a different JSON representation, depend on `jev-dsl:jev-core` and implement
`JsonValue`. Core depends on `base`, `text`, `profunctors`, and `selective`;
optics use the van Laarhoven representation without a lens package dependency.
The examples use `lens`. [Mini.hs](test/Mini.hs) exercises a second JSON
implementation. Hosted integrations and the breaking rename from
`Contract.question` to `textInstructions` are covered in the
[migration notes](docs/composition.md#migration-from-01).

## Run the examples

[Reflex.hs](examples/Reflex.hs) demonstrates query composition, payload optics,
selective evaluation, and expected-loss decisions.
[Readme.hs](test/Readme.hs) contains the packet and selection examples.
Both are compiled by `check.sh`.

[Guard.hs](examples/Guard.hs) is an interactive dialogue tree. Its instruction
functor carries typed continuations; one fold prints the dialogue and another
asks Jev which continuation to take. Nodes combine branch choices, per-item
checks, rubrics, and optional tripwires. The same representation supports
inspection and execution.

```sh
nix develop                         # or nix-shell
./scripts/guard.sh --script          # inspect the dialogue without network calls
./scripts/guard.sh                   # play it
./scripts/example.sh                 # triage a failing check in one request
```

Live examples read a nonempty `TYPESAFE_API_KEY`, otherwise
`$XDG_CONFIG_HOME/typesafe/api-key` (default `~/.config/typesafe/api-key`). The
file contains the raw key on one line. The transport passes the authorization
header through stdin rather than curl’s command-line arguments.

## Build and check

The Nix shell pins GHC 9.12 and includes Cabal and the dependencies. Inside it:

```sh
cabal build all --enable-tests && cabal test
./check.sh
```

`check.sh` builds with `-Werror`, runs the tests, compiles the examples and
`test/Readme.hs`, and requires each negative fixture to fail with its expected
diagnostic. Tests cover typed decoding, batching, replay, optics, selective
execution, and recorded provider exchanges. See
[test/fixtures/README.md](test/fixtures/README.md) for capture provenance.

The provider contract was checked against `jev-1.13.0` and OpenAPI 0.2.0;
recorded dates and fixtures describe that evidence, not a guarantee about
future versions of `jev-latest`. Policy calibration and the effect of nested
field references on live judgments remain open evaluation work.

[Changelog](CHANGELOG.md) · [Issues](https://github.com/inanna-malick/jev-dsl/issues) · [MIT](LICENSE)
