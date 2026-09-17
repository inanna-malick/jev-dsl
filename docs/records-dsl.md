# The declared-record front: `Jev.Records` (design)

No code in this repository implements this design; `Jev.Records` does not
exist yet.

Status: design only, recorded 2026-09-16. This front is for human use and review: code that a person
writes once, reads in a file, and maintains, where a declaration is a
feature. The compositional-operator front in `Jev.Operators` is for a model
working in a stateful session, where declarations are a liability; see
[authoring.md](authoring.md). Both are fronts over one core and produce the
same wire.

## Shape

A packet is a higher-kinded record with one field per question:

```haskell
data Inspect mode = Inspect
  { next     :: mode :- Choice Next
  , urgency  :: mode :- Score Urgency
  , children :: mode :- Each Relevance
  , evidence :: mode :- Group Sufficiency
  } deriving (Generic, Schema)
```

`Inspect Questions` is what you write; `Inspect Answers` is what comes
back; `a.next`, `a.evidence.enough`, `a.children` as a keyed list of
`Relevance Answers`. `Group` and `Each` answers are transparent. Field names
are wire keys after snake-casing, with an optional `keyed` override for
names Haskell cannot spell.

## Alternatives are sums

```haskell
data Next = Rerun Check | ReadSource Span | AskModel Handoff
  deriving (Generic, Show)
```

A `Choice Next` offers values of the sum with descriptions:

```haskell
next = choice "What next?"
  [ Rerun c      `is` "Rerun the focused check"
  , ReadSource s `is` "Read the implicated span"
  , AskModel h   `is` "Needs judgment beyond the evidence"
  ]
```

The wire key defaults to the snake-cased constructor name. Repeated
constructors, which arise whenever several runtime candidates share a
constructor, must be keyed: `Follow e1 `keyed` "e1" `is` d1`. A repeated
key without `keyed` is a preparation error naming it. The answer is
`a.next.chosen :: Next` and elimination is `case`. Coverage and
exhaustiveness are separate: `case` covers the constructors, and
`-Wincomplete-patterns` (the one warning worth making fatal) checks it;
whether the offered values covered the situation is what the exit
constructor is for. Runtime candidates have no distinct kind here: they are
values of the sum offered in a list, keyed.

## Rubrics are enums

```haskell
data Urgency = Background | Checkpoint | Blocked | Invalidating
  deriving (Eq, Ord, Enum, Bounded, Show, Generic)

urgency = score "What is the consequence of waiting?" $ \case
  Background   -> "No current action depends on this"
  Checkpoint   -> "Useful at the next ordinary checkpoint"
  Blocked      -> "A worker cannot take its next action"
  Invalidating -> "Continuing would invalidate ongoing work"
```

The description function is exhaustiveness-checked by GHC. Levels are the
constructors in `Enum` order; a nullary-only constraint and the 1 to 10
bound are compile-time errors naming the type. Answers give `expectation`,
`massAtOrAbove Blocked a.urgency`, `levelOf :: Urgency`.

## Pools are values

```haskell
edges = pool "edges" [(key, description, edge) | ...]

inspect = Inspect
  { next     = chooseFrom edges [NoUsefulEdge `is` "..."]
  , relevant = eachIn edges (\ref -> Relevance { useful = askAbout ref "..." })
  , ...
  }
```

The state is wrapped with `pooled`; correspondence between declaration and
uses is checked at `prepare`, exactly as in the operator front. A pool
declaration is a field of type `mode :- PoolDecl Edge` whose field name
is the pool name.

## What is shared

Everything below the record layer: endpoints, `Instructions`, `Criteria`,
`given`, `State`, pools, `prepare`, `decodeResponse`, `roundTrip`, errors,
policies, `Doubt`. A record front adds only `Generic` traversals that turn
the record into the core's compiled questions and back, and an `Alts`
interpretation for sums. The core no longer carries a sums-as-alternatives
seam; that interpretation belongs to this front alone, as a fourth shape of
the shared `Alts` chain that renders and decodes a constructor by its
snake-cased name.

## Why records are the human form and not the model form

In a stateful session a model writes a different packet each turn. A
record per packet means a declaration per turn, selector names that pollute
scope and shadow each other, and `-Wmissing-fields` as the only guard
against a forgotten handler. Those costs are invisible in a file that a
person edits, where the declaration documents the packet and `case` over a
sum reads naturally. Hence two labelled fronts rather than one compromise.

## Implementation sequence, when wanted

1. `Schema` via `Generic` over `mode :- e` fields, reusing `Endpoint`.
2. `is`/`keyed` as offers of a `Sum t` alternative shape added to the core's chain; a `ConName` class (Generic default) for default keys.
3. Enum rubrics as an `Alts`-free `Rubric` instance built from `Enum` and
   `Bounded` with the description function.
4. `HasField` access for `Answers`, transparent `Group`/`Each`.
5. Compile-fail fixtures for the nullary-only and bound checks; goldens
   equal to the operator front's for the same packets.
