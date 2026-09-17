# jev-dsl

A Haskell DSL for agents that would rather have a question judged than
guessed. A packet of labelled questions is written once as an expression;
its type is inferred, it renders to the exact request JSON for
[TypeSafe's Jev](https://docs.typesafe.ai), and the answers come back under
the same labels as records read by field, each selection still carrying the
payload it was offered with. No schema, no instance, no codec, no network.

The guide written for a model is [docs/authoring.md](docs/authoring.md).
Every example below is compiled by `test/Readme.hs`. `Jev.Operators` is the
surface; `Jev.Transport` has `request` and `decode` for a program that
carries the JSON itself; `Jev.Core` is the same library polymorphic over
the JSON type. A declared-records front for human authors is
[designed](docs/records-dsl.md), not built.

## Locate

One question, one call, and either a payload the program offered or a
handback. Nothing is declared.

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

The payload is a line number the program supplied, never a string the
model produced. The answer is a record and displays as one:

```
> a
Choice {key = "142", mass = 0.78, margin = 0.61, confidence = 0.80, masses = ["142" 0.78, "137" 0.17, "not_here" 0.05]}
> accept merging a
Left (Unconfident 0.8)
```

## Route

Several questions about one situation go in one packet and one call. The
packet's type is inferred from the questions.

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

Answers come back under the same labels:

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
`masses`. When the branch must run the payload it was offered with,
handlers eliminate the selection. They follow declaration order, and the
compiler rejects a misordered, missing, extra, or mislabelled handler with
a message naming the label it expected.

```haskell
act :: Inspection Answers -> Text
act a =
  handle a.next.chosen
    (  #use_witness (\(Witness w) -> "located at " <> w)
    .| #ask_model   (\(Handoff h) -> "hand back: " <> h)
    .| onMany       (\k _ -> "follow " <> k) )
  <> (if massAtOrAbove #blocked a.urgency > 0.5 then " now" else " later")
```

A handler list is a value. The same list eliminates every contender above
a floor, or the winner under a named policy: `routing` for a read-only
choice, `spawning` for starting work, `merging` for anything with a
receipt.

```haskell
routes :: Handlers Text Routes
routes = #use_witness (const "witness") .| #ask_model (const "model") .| onMany (\k _ -> k)

alive :: Inspection Answers -> [Text]
alive a = [handle s routes | (_, s) <- contenders 0.25 a.next]

decide :: Inspection Answers -> Either Doubt Text
decide a = fmap (`handle` routes) (accept spawning a.next)
```

Signatures are optional. This one is what the compiler inferred for
`inspection`:

```haskell
type Routes = "use_witness" ::> Witness :|: "ask_model" ::> Handoff :|: Many Edge
type Inspection = Packet
  '[ "next" ::= Choice Routes
   , "urgency" ::= Score ("background" :|: "checkpoint" :|: "blocked")
   , "children" ::= Each (Packet '[ "useful" ::= Noul ])
   , "evidence" ::= Group (Packet '[ "enough" ::= Noul ]) ]
```

## Pool

When several questions range over the same alternatives, declare them once
under the pool's name and draw on it from a choice, a per-member Noul, and
a question under a premise.

```haskell
probing probes =
     #probes   := probes
  :& #best     := choice "Which probe discriminates best?" (manyFrom probes .| alt #none "No probe discriminates" ())
  :& #per      := eachIn probes (\r -> #useful := askAbout r "Does this probe help answer the inquiry?" :& Nil)
  :& #if_retry := given "the mechanism is retry redelivery" (choice "Which probe confirms it?" (manyFrom probes))
  :& Nil

retryProbes = pool #probes [("run_retry", "Retries m42 and counts callbacks", Command "just test-target actor retry")]
```

The wire carries each member's wording once, under `state.pools.probes`,
and names the pool beside every question that draws on it.

## The gate at Greyhaven

`examples/Guard.hs` is what the library can carry: a city guard at a gate,
played against a person typing, as a catamorphism whose algebra is Jev.

The script is a tree written by hand, a fixed point of `GuardF`. Four of
its constructors are Jev questions. `Ask` sorts a free-form reply into one
of the branches the author wrote, with a tripwire Noul in the same packet
for admissions and contradictions. `Check` holds the story against each
wanted poster, one Noul per poster over a pool. `Weigh` grades the story on
a three-level rubric. `Happen` lets Jev pick which of six authored events
fits the moment, or none. The branches come from a small world value; the
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

Rules stay in Haskell: an unbonded weapon is turned away without any
weighing, a held traveller can talk their way down to the road but never
through the gate, and something can happen at most every other exchange.
Jev decides everything that needs judgment. Nothing is generated at run
time.

```sh
scripts/guard.sh --script                 # print the tree, no network
TYPESAFE_API_KEY=... scripts/guard.sh     # play it
```

A session on 2026-09-17, thirteen calls, thirteen thousand input tokens:

```
guard: Halt. Where do you hail from, traveller?
you:   The farmlands
       [heard farmlands 99%]
guard: And what brings you to Greyhaven?
you:   Turnips for the market
       [heard market 96%]
guard: Anything to declare? Weapons, goods, anything the customs officer should see?
you:   Nothing, just the cart
       [heard nothing 99%]
       [weighed sound  sound 72%, thin 26%, false 2%]
guard: Go on through. Mind the curfew.
       [happening runner 46%]
       A boy in watch colours comes pelting down the wall road and mutters something to the guard.
guard: Seen at the harbour tonight, they say. The thief. So much for the north road.
guard: Anything else before you go through?
you:   Which way to the temple?
       [heard temple 100%]
guard: Left at the well, follow the bells. The infirmary's round the back.
guard: Anything else before you go through?
you:   Good thing you did not check under the turnips, there is a cask of brandy the customs man never saw
       [slip 97%, was heading for chat]
guard: Wait. Say that again.
       [weighed false  sound 12%, thin 26%, false 62%]
guard: Guards! Hold this one. Someone fetch the captain.
       [happening bell 34%]
       The curfew bell starts up over the rooftops, slow and heavy.
guard: There's the bell. Nobody's got long now.
guard: Stand there. The captain's on his way. Anything to say for yourself?
you:   It was a joke, I swear it
       [slip 79%, was heading for explain]
guard: Noted. The captain will want to hear that.
guard: Stand there. The captain's on his way. Anything to say for yourself?
you:   Fine. I will go
       [heard explain 40%  (also protest 22%)]
guard: Go on. Slowly.
       [weighed false  sound 9%, thin 27%, false 64%]
guard: Guards! Hold this one. Someone fetch the captain.
       [happening captain 84%]
       Boots on the wall walk. The captain, with two of the watch behind him, stops at the gate.
guard: Captain. This one's for you.
```

In other sessions a traveller off the north road with "only my satchel,
heavy" matched the thief poster at 68% and was held; "Look, there is a
silver piece in it for you" tripped the wire at 81% from the turned-away
hub; "Work. I heard the watch is hiring" was heard as the barracks at 99%;
and a traveller who answered the first question with "Why do you need to
know" was asked once more, then moved on with the story marked. An
ordinary chatty visit runs about eight exchanges, fourteen calls, and
fifteen thousand input tokens. The transport is `scripts/transport.sh`, a
curl call that keeps the key out of every Haskell process and retries
overloads.

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
