# jev-dsl

Typed Haskell records for [TypeSafe's Jev](https://docs.typesafe.ai): write a
question record once, get the exact request JSON out of it, and get the same
record back under an answer mode with every selection carrying the payload you
declared it with. No network in the library. Any transport that can post JSON
and hand the body back will do, including one written in another language.

```haskell
data Inspect mode = Inspect
  { next     :: mode :- Choice Routes     -- static alternatives, each with its own payload type
  , probe    :: mode :- Choose Command    -- runtime candidates, one payload type
  , urgency  :: mode :- Score Urgency     -- ordered rubric as a record
  , children :: mode :- Each Relevance    -- runtime-sized keyed collection of a sub-record
  , evidence :: mode :- Group Sufficiency -- nested record
  } deriving Generic
instance Schema Inspect
```

`Inspect Questions` is what you write. `Inspect Answers` is what comes back.
The JSON type never appears in your code.

## The tiny use

One question, one answer, and either a retained command or a handback.

```haskell
lines <- numbered <$> readFile path
answer <- jev1 transport jevLatest world
  (choose "Which line begins the retry-timeout branch?" (candidates lines)
     [noMatch "Not in this file"])
case answer of
  Left err -> handleError err
  Right a  -> pickOr handBack a $ \(lineNo, revision) -> editAt lineNo revision
```

`candidates` takes `(key, description, payload)` triples and is total; every
shape check happens in `prepare`, so a cell reads straight through. The
payload here is a line number and the revision it was read at, never a string
the model produced. The file is never read into the model's context.

## A heterogeneous investigation

Alternatives are a record. Each field has its own payload type, an exit is an
ordinary field, and elimination is an exhaustive handler record that the
compiler checks.

```haskell
data Routes mode = Routes
  { followCaller :: mode :- Option Edge
  , useWitness   :: mode :- Option Witness
  , noUsefulPath :: mode :- Option ()
  , askModel     :: mode :- Option Handoff   -- the handback is just another alternative
  } deriving Generic

inspection = Inspect
  { next = choice "Which available continuation advances the inquiry?" Routes
      { followCaller = option "Inspect publish_if_active, which gates publication on cancellation" (Edge "publish_if_active")
      , useWitness   = option "The current span already answers the inquiry" (Witness "complete_request:41")
      , noUsefulPath = option "No supplied continuation is useful" ()
      , askModel     = option "Choosing needs a design preference beyond the supplied evidence" (Handoff "preference")
      }
  , probe    = choose "Which focused query best discriminates the remaining mechanisms?" probes
                 [deferToModel "Discriminating needs evidence outside the supplied state"]
  , urgency  = score "What is the consequence of waiting?" Urgency
      { background = level "No current action depends on this"
      , checkpoint = level "Useful at the next ordinary checkpoint"
      , blocked    = level "A worker cannot take its next action"
      , invalidating = level "Continuing would invalidate ongoing work"
      }
  , children = each edges $ \name -> Relevance
      { useful      = noul ("Is child " <> name <> " relevant to the inquiry?")
      , contradicts = noul ("Does child " <> name <> " contradict the premise?")
      }
  , evidence = group Sufficiency
      { enough     = noul "Does the supplied evidence answer the inquiry?"
      , gapRemains = noul "Does answering require source not supplied?"
      }
  }

resp <- either (fail . show) pure =<< roundTrip transport jevLatest world inspection
let a = answers resp
outcome <- match a.next Routes
  { followCaller = \(Edge e)    -> follow e
  , useWitness   = \(Witness w) -> pure (Located w)
  , noUsefulPath = \()          -> pure NeedOtherCandidates
  , askModel     = \(Handoff w) -> pure (HandBack w)
  }
when (yesAbove 0.8 (groupAnswer a.evidence).enough) ...
```

Under `-Werror=missing-fields` a forgotten handler is a compile error. A
handler record for a different alternatives record, or a handler with the
wrong payload type, is a type error naming the field.

## Keep competing explanations alive

Jev answers every question in a packet independently over one state. When a
Choice splits, do not commit: keep the live hypotheses, gather one
discriminating observation each, and ask again over the enriched state. A
premise-prefixed question lets one packet pre-decide the branch-specific
follow-up.

```haskell
data Investigation mode = Investigation
  { mechanism    :: mode :- Choice Mechanisms
  , checkIfRetry :: mode :- Choose Command
  , risk         :: mode :- Scale                -- a runtime-sized rubric
  , extras       :: mode :- Many                 -- a fully dynamic sub-map
  } deriving Generic

first <- roundTrip transport jevLatest world Investigation
  { mechanism    = choice "Which mechanism explains the second callback?" mechanisms
  , checkIfRetry = given "the mechanism is retry redelivery" $
                     choose "Which check is the focused verification?" checks []
  , risk         = scale (Present (object ["question" .= "How broad is the fix?", "focus" .= "changed callers"])) rubric
  , extras       = many [("wake_now", someQ (noul "Does the state satisfy the wake policy?"))]
  }

let ms   = masses a.mechanism
    live = [ h | (h, mass) <- [(retry, ms.retryRedelivery), (admission, ms.doubleAdmission)], mass > 0.3 ]
-- or, for a dynamic choice: take (top two of) `contenders a.probe` and run their retained payloads
observations <- traverse observe live
second <- jev1 transport jevLatest (stateObject [("observations", toJSON observations)])
            (choice "Which mechanism now?" mechanisms)
```

The returned bundle names one supported mechanism with the evidence that
separated it, or the survivors with the evidence that failed to. Both are
better inputs to a larger model than an early commitment.

## What you get back

- `match`, `withChoice`, `probabilityOf`, `masses`, `selectedKey`, `confidence`
  for static choices. A selection cannot be applied to another result's
  distribution; the types forbid it.
- `picked`, `pickOr`, `ranked`, and `contenders` for dynamic choices, with
  exits ranked among candidates so a winning handback is first and the top
  two carry their payloads. `select` applies a `Policy` (mass floor, margin,
  confidence floor) and returns the accepted candidate or a structured
  `Doubt` that keeps the cases apart: the provider chose an exit, the winner
  is underweight, the margin is too thin, or confidence is too low. The
  original answer stays in hand for inspection or resumption. A one-option
  choice has no runner-up and passes the margin check; an exit-only choice is
  valid. `selectOr` runs a continuation or hands the doubt back. Acting on an
  uncertain choice should not be the shortest path by accident.
- `probabilityYes`, `yesAbove`, `noBelow`, `unsure` for Nouls, so a tree of
  natural-language conditions reads like the sentence it encodes.
- `expectation`, `levelMasses`, `legend` for Scores; the legend is the
  submitted level value, structured or not.
- `usage` and `resolvedModel` verbatim from the envelope, and `diagnostics`
  for anything worth knowing that is not a rejection (a distribution whose
  rounded masses do not sum to one, for instance).

## What `prepare` and `decodeResponse` guarantee

Builders are total and produce drafts; a draft may be invalid. `prepare` is
where validity is established, and only a `Prepared` value can be rendered or
decoded against. That is a deliberate trade: every check happens in one
place with the question key attached, and no `Either` sits between an author
and a candidate list.

`prepare` rejects, with the question key: a dynamic choice with neither
candidates nor exits, duplicate candidate keys, an exit colliding with a
candidate, duplicate wire keys after overrides, more than 255 alternatives
or a runtime rubric outside 1 to 10, null levels, bare scalars where the
provider requires structure, a bare-scalar or null state, empty question
maps, empty question ids, and duplicate flattened ids. An empty *candidate*
key is admitted, because the provider admits it. Static records are checked
at compile time instead: an alternatives record with no `Option` fields or
more than 255, a level record with none or more than 10, or a field of the
wrong shape, each fail with a message naming the record and the rule.

`decodeResponse` rejects: a selection outside the submitted set, probability
keys that do not equal the submitted set exactly, values or confidence outside
[0, 1] or non-finite, a legend that differs from the submitted levels, wrong
answer kinds, missing, unexpected, or duplicate answer keys. A provider
rejection body is returned parsed, not as a decode failure of a different
kind.

`rawUnchecked` is the one escape hatch. It sends any value as the question
object and returns the answer as the original parsed JSON, and it is
explicitly outside the guarantee.

Of 253 real accepted exchanges, 251 render as structurally identical JSON
through this library and decode against the retained request; the other two
carried unknown request-level members the harness added deliberately, which
the closed request spine does not express. Every rejected shape is either
inexpressible, rejected by `prepare` with a named error, or a decision only
the provider can make. See `test/fixtures/README.md` for provenance.

## Bring your own JSON type

`Jev` fixes the JSON type to aeson's `Value`. `Jev.Core` is the same library
polymorphic over a seven-method `JsonValue` class and imports only `base`,
`containers`, and `text`, so it can be copied into an environment that cannot
load aeson. Write an instance for your value type and a facade of type
synonyms like `src/Jev.hs`; user code is unchanged.

## Contract pin

Built against the behavior observed on 2026-09-16: requests to `jev-latest`
resolved to `jev-1.13.0`, public OpenAPI 0.2.0. Choice 1 to 255 alternatives,
Score 1 to 10 levels, the question id map unbounded within token limits,
undocumented question kinds excluded. Structured content is admitted wherever
the provider admits it.

## A worked example, end to end

`jev-dsl-example` is one record (triage a failing check: which diagnostic
explains it, what to do next with typed payloads, which check verifies,
whether the evidence suffices, how broad the fix is) with two commands.
`request` prints the request JSON; `decode` reads the response JSON on stdin
and prints what the typed answers say. Nothing in between is the library's
business. `scripts/example.sh` puts curl there:

```sh
TYPESAFE_API_KEY=... ./scripts/example.sh
```

```
explains: d2  "bookmark identity assertion failed after prefix insertion; observed old numerical offset"
  ranked: d2=1.0, d1=0.0, d3=0.0, no_match=0.0
next: read the implicated source  confidence 0.4
verify: prefix_insert
sufficient: 0.61  (unsure)
breadth: 0.93  localized 0.31, adjacent 0.45, contract 0.24
```

## Building

```sh
cabal build all --enable-tests && cabal test
./check.sh     # also requires every test/reject/*.hs to fail to compile at its own site
```

Set `JEV_EVIDENCE_DIR` to a directory of research captures to run the full
generic pass over every recorded success.

Not yet here: shared candidate pools across questions in one packet. They
attach as an endpoint; `Prepared` already retains the schema and state where
their descriptions will serialize.

MIT.
