# Compose and revise a reflex

A reusable assessment owns its evidence and returns an ordinary `Query a`.
`Applicative` combines independent observations; `traverse` lifts an assessment
across runtime data; `Star Query` gives it standard profunctor operations. Use
a `do` block when a result determines what to read or do next. There is no
benefit in hiding that dependency inside an applicative expression.

The complete, compiled example is [Reflex.hs](../examples/Reflex.hs). It defines
the domain records and lens behind this composition:

```haskell
assessReport = J.assessment reportState $ \world ->
  J.judge J.careful <$> J.question world #supported
    (J.noul ("Does " <> J.field #evidence world <> " support " <> J.field #claim world <> "?"))

reviewPending = traverseOf (traversed . _2)
  (runStar (lmap (view helperReport) assessReport))
```

`lmap` adapts the input; `fmap` adapts the result. An assessment is a standard
`Star (Query Value)`, so existing profunctor and optic combinators work without
a second vocabulary. In the Aeson facade, use `J.Query a` for signatures;
use `Star (Core.Query Value)` when the type constructor must be partially applied.

## Evidence, identity, and calls

`question world #label q` is one typed question; `packet world p` lifts an
existing typed packet. They capture their original state, schema, and payloads.
`scope "report"` and `at #report` add readable path segments.

Composing assessments never unions their states. Preparation groups leaves
only when `JsonValue.jEqual` says their serialized states are equal, under the
selected model. Groups run sequentially in first-occurrence order through the
supplied session. This library owns no scheduler. Different states require
different requests; keeping shared task evidence in one state and per-item
text in questions can preserve one-call batching when that is the intended
meaning. A field reference always refers to its own assessment's state.

Occurrence numbers are assigned in leaf order. Applicative reassociation and
pure mapping preserve them. `traverse assess reports` works when every copy
uses `#supported`. Reusing a query twice asks twice; mapping one answer to a pair
reuses one observation. Packet-internal duplicate labels remain compile errors.
Runtime duplicate question paths and offer keys remain preparation errors.

`prepareQuery` validates the entire independent computation before transport.
Inspect `queryGroups` to see each actual request and its logical occurrences.
Empty traversals and pure queries complete without sending an empty request.
The provider's 255-alternative limit applies to each individual choice; it is
not a question-count limit. No alternatives are silently split across choices.

Adding sibling questions can change live judgments even when state is unchanged.
Exact-state batching guarantees evidence isolation, not statistical invariance
under a changed question set. Use actual prepared requests when comparing runs.

## Keep observations, change policy

`Response a` remains one provider response. `Evaluation err a` holds a result
or typed failure and the actual observations of zero or many calls. It retains
the successful prefix and failed attempt if transport or decoding fails. Each
observation includes the actual request, raw transport outcome, occurrence map,
and decoded response metadata when decoding succeeded. Unknown usage remains
unknown; a measured zero remains zero.

Mapping either container changes only its typed result. Request and answer
previews inspect original evidence without demanding captured functions or
host actions. Constructing an action, carrying it in a choice, observing which
branch wins, and executing the action are separate operations.

`choiceWeights answer handlers` projects every alternative through exhaustive
handlers. `carriedWeights` does the same for uniform payloads; `noulWeights`
and `scoreWeights` retain their respective alternatives. These return raw
`Weights`, preserving reported scale and authored order. `normalize` explicitly
validates finite nonnegative weights and positive finite total mass, returning
a `Distribution`. It never edits the original observation.

`expected loss distribution` computes expected loss. `covering mass distribution`
selects the smallest descending-weight prefix reaching a valid target in [0,1],
with authored order breaking ties. A target of zero selects nothing. Mapping
payloads does not merge alternatives even when their new values compare equal.
These are ordinary data and functions: apply several policies to one observation
without another provider call.

## Edit the repertoire, not its wire identity

`payloads #label` is a type-changing van Laarhoven traversal. Compose it with
`over`, `traverseOf`, other traversals, or lenses into your own payload records.
A static alternative has one focus; a runtime `many` group has every row.
Keys, labels, and wording remain unchanged. An absent label is a compile error.

`Uniform` is `Functor`, `Foldable`, and `Traversable`. A domain instruction
functor containing a uniform repertoire can derive those operations instead
of hand-maintaining a mapping instance. The Guard example now derives its
`Functor`; its existing catamorphism still selects original typed continuations.

## Decide which observations to speculate

`Program` implements the standard `Selective` interface. Lift a query with
`liftQuery`, then use `select`, `ifS`, or other `Control.Selective` combinators.
The interpreter has two explicit strategies:

- `SkipUnused` asks an active independent frontier, then uses its answers to
  decide which frontier is needed next. Unselected branches are not prepared
  or sent. A malformed later active frontier retains earlier observations.
- `BatchKnown` prepares all statically known questions, including questions in
  unselected branches, and groups them by exact state. An invalid speculative
  branch therefore refuses the independent batch before any call.

Speculation observes judgments; it does not execute carried effects. A branch
can carry an `IO` action, an Exomonad actor instruction, or an `Eff` continuation.
The caller explicitly runs the selected value. Answer-dependent acquisition
belongs in ordinary `IO`/`Eff` or a host actor, not a new scheduler here.

## Replay precisely

`recordings` extracts requests and raw transport outcomes from an evaluation.
`replayQuery` requires complete structural equality of every request and passes
the recorded body through the new prepared plan's original typed decoder.
Missing, extra, or changed requests are typed replay failures. Replaying a
transport/decode failure reproduces the failure and its observed prefix.
`replayProgram` applies the same rule to the frontiers actually selected by
the requested strategy.

There are three distinct ways to revise a reflex:

1. **Change local policy.** Keep a typed observation; apply a different threshold,
   loss function, or cumulative-mass rule. No replay or network is needed.
2. **Change local payloads.** Prepare the same wire request with different
   continuations and replay the retained recording through its new decoder.
3. **Change evidence, wording, model, or offered alternatives.** Prepare and
   inspect a new request, then obtain a fresh observation. Replay refuses it.

A notebook can save an assessment, inspect its requests, compare policies,
and publish the chosen context or execute a continuation through its existing
host API. Jev supplies observations; the host owns publication and effects.

## Migration from 0.1

Existing `ask`, `ask1`, typed packets, settlement, and prepared calls remain.
`Prepared` additionally supports `Functor`. The low-level Contract helper
`question :: Text -> Instructions v` is renamed `textInstructions`; `question`
now constructs an evidence-owning query. New module exports can shadow local
names such as `packet` or `result`; qualify the library import or hide those
names where appropriate.

`jev-core` now directly depends on `profunctors` and `selective`, while remaining
independent of Aeson. Payload traversals require no optics package in core;
the examples use `lens`. Hosted consumers compiling core sources directly must
register `Jev.Core.Query`, `Jev.Core.Distribution`, and `Jev.Core.Program`, and
include their declared packages in the actual compiler closure.
