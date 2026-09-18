{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- 'key', 'mass' and 'margin' are answer fields, and this module binds all
-- three as ordinary locals; the shadowing is deliberate and local.
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- A handler is written as a bare label applied to a function, so the
-- instance that builds one is headed by a function type and counts as an
-- orphan wherever it lives.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The agent-facing form: an anonymous, type-indexed packet of questions,
-- alternatives and rubric levels as type-level chains of labels.
-- Polymorphic over the JSON value through "Jev.Core.Json";
-- "Jev.Operators" fixes it.
--
-- The packet's type is inferred from the questions written, and mirrors
-- the way they were written: a cell is a packet of one, and two packets
-- join with the same operator, so packets compose and there is no empty
-- packet to write. Labels, handler completeness and state field names are
-- checked at compile time with messages in the author's vocabulary;
-- wording, runtime candidates and level counts are checked at preparation.
module Jev.Core.Schema
  ( -- * Modes
    Questions, Answers, Fields, type (:-)
    -- * Packets
  , type (::=), type (:&), Label (..), Cell (..), Packet (..)
  , QKind, QJson, ToQ (..), ToCell (..), CellOk, Nested, NestedQ
  , Unique, Get, PacketLabels
    -- * State
  , State, state, rawState, stateValue, field, FieldPath ((:/)), StatePath, StateHas
  , Field (..)
    -- * Endpoints
  , Noul, Choice, Score, Each, Optional, Group
  , Q (..), A (..), Yes (..), Chosen (key, mass, margin, confidence, masses), Scored (expectation, confidence, masses)
    -- * Alternatives and rubric levels
  , type (::>), type (::*), type (:->), type (:|:), Offer, HandlerT, Level, Interp
  , Alts (..), (.|), alt, many, level, offered
    -- * Handlers, by label
  , Alternatives, AltsOk, Handles, Covers, Fits, HandlersOk, Fetch (..), Dispatch (..)
  , Levels, RubricOk, Index, Selected
    -- * Uniform payloads
  , Carries (..), Retarget, Dict (..), taken, Uniform (..), uniform, mapUniform, withUniform, branches
    -- * Builders
  , noul, choice, score, each, optional
    -- * Results
  , Weighed (..), Weight (..), Doubt (..), Cause (..), Policy (..), Settled (..)
  , Lenient, Careful, Strict
  , settle, takenUnder, judge, holds, explain, contenders, handle
  , grade, graded, massAtOrAbove
    -- * The operation
  , Schema (..), Model (..), jevLatest, Session, session
  , request, decode, Response, answers, responseModel, usage, diagnostics
  , JevError (..), roundTrip, jev1
    -- * Internals for extension (capture replay lives outside the library)
  , Endpoint (..), Path (..), encodePath, extend, leaf, lookupAnswer
  , checkLegend, checkExpectation, labelText
  ) where

import Data.Kind (Constraint, Type)
import Data.List (nub, sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits
import Jev.Core.Contract
import Jev.Core.Json
import Numeric (showFFloat)

-- ---------------------------------------------------------------------------
-- Modes and the interpretation of a cell
-- ---------------------------------------------------------------------------

data Questions (v :: Type)
data Answers (v :: Type)
data Fields (v :: Type)

-- | How a cell of endpoint @e@ reads under a mode. Questions are always the
-- leaf; answers are transparent for nesting; a state field keeps the
-- Haskell type it was written with.
type family mode :- (e :: Type) :: Type where
  Questions v :- e = Q v e
  Answers v :- Noul = Yes
  Answers v :- Choice alts = Chosen alts
  Answers v :- Score p levels = Scored p levels
  Answers v :- Group s = s (Answers v)
  Answers v :- Each a e = [(a, Answers v :- e)]
  Answers v :- Optional e = Maybe (Answers v :- e)
  Answers v :- e = A v e
  Fields v :- a = a
infixr 0 :-

-- ---------------------------------------------------------------------------
-- Endpoints
-- ---------------------------------------------------------------------------

data Noul
data Choice (alts :: Type)
data Score (p :: Type) (levels :: k)
data Each (a :: Type) (e :: Type)
data Optional (e :: Type)
data Group (s :: Type -> Type)

data family Q (v :: Type) (e :: Type)
data family A (v :: Type) (e :: Type)

data Label (k :: Symbol) = Label
instance k ~ k' => IsLabel k (Label k') where fromLabel = Label

labelText :: forall k. KnownSymbol k => Text
labelText = T.pack (symbolVal (Proxy @k))

labelOf :: forall k. KnownSymbol k => Label k -> Text
labelOf _ = labelText @k

-- ---------------------------------------------------------------------------
-- Alternatives, rubric levels and handlers: one chain, four shapes
-- ---------------------------------------------------------------------------

-- | One alternative: a label and the payload it carries.
data (k :: Symbol) ::> (p :: Type)
-- | A runtime group: a label, and one row of type @p@ per candidate.
data (k :: Symbol) ::* (p :: Type)
-- | One handler: a label and the function written for it.
data (k :: Symbol) :-> (x :: Type)
-- | A chain. It associates to the right and a chain may stand where one
-- element does, so chains compose.
data (a :: ka) :|: (b :: kb)
infix 6 ::>, ::*, :->
infixr 4 :|:

-- | Interpretations of an alternative: what an offer supplies, what a
-- level carries.
data Offer (v :: Type)
data HandlerT
data Level (v :: Type) (p :: Type)

type family Interp (f :: Type) (x :: Type) :: Type where
  Interp (Offer v) (k ::> p) = (v, p)
  Interp (Offer v) (k ::* p) = [(Text, v, p)]

-- | Offers, handlers, or levels for a whole chain.
type Alts :: Type -> forall k. k -> Type
data Alts f alts where
  One :: KnownSymbol k => Interp f (k ::> p) -> Alts f (k ::> p)
  Grp :: KnownSymbol k => Interp f (k ::* p) -> Alts f (k ::* p)
  Hnd :: KnownSymbol k => x -> Alts HandlerT (k :-> x)
  Lvl :: KnownSymbol l => v -> p -> Alts (Level v p) (l :: Symbol)
  (:|) :: Alts f x -> Alts f rest -> Alts f (x :|: rest)
infixr 4 :|

-- | Join two chains. Either side may itself be a chain, so a program can
-- keep a set of alternatives as a value and compose it.
(.|) :: Alts f x -> Alts f rest -> Alts f (x :|: rest)
(.|) = (:|)
infixr 4 .|

-- | One alternative: its label, its wording for the provider, its payload
-- for the program. The alternative's type is inferred from this.
alt :: KnownSymbol k => Label k -> v -> p -> Alts (Offer v) (k ::> p)
alt _ d p = One (d, p)

-- | A runtime group: its label, then a wire key and a wording per row. The
-- row itself is the payload the handler receives.
many :: KnownSymbol k => Label k -> (a -> Text) -> (a -> v) -> [a] -> Alts (Offer v) (k ::* a)
many _ key wording rows = Grp [(key x, wording x, x) | x <- rows]

-- | One rubric level: its label, its wording for the provider, and the
-- result 'grade' returns when the score lands on it. The same three things
-- an alternative carries.
level :: KnownSymbol l => Label l -> v -> p -> Alts (Level v p) l
level _ = Lvl

-- | The keys and wording an offer would send, without building a request.
offered :: Alternatives alts => Alts (Offer v) alts -> [(Text, v)]
offered = altOffered

-- Handlers are written with a label and a function. The label fixes which
-- alternative is answered and the function's own type is the chain's, so a
-- handler list is an ordinary value with an inferred type and no order.
type family HandlerShape (k :: Symbol) (x :: Type) :: Constraint where
  HandlerShape k (a -> b) = ()
  HandlerShape k (a, b) = TypeError ('Text "offers are written alt #" ':<>: 'Text k ':<>: 'Text " wording payload; #" ':<>: 'Text k ':<>: 'Text " alone builds a handler")
  HandlerShape k x = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " takes a handler: a function of the payload")

instance (KnownSymbol k, HandlerShape k x, res ~ Alts HandlerT (k :-> x))
  => IsLabel k (x -> res) where
  fromLabel = Hnd

-- ---------------------------------------------------------------------------
-- Labels
-- ---------------------------------------------------------------------------

type family (++) (a :: [k]) (b :: [k]) :: [k] where
  '[] ++ b = b
  (x ': a) ++ b = x ': (a ++ b)

type family (||) (a :: Bool) (b :: Bool) :: Bool where
  'True || b = 'True
  'False || b = b

type family SameLabel (a :: Symbol) (b :: Symbol) :: Bool where
  SameLabel a a = 'True
  SameLabel a b = 'False

type family AltLabels (alts :: Type) :: [Symbol] where
  AltLabels (k ::> p) = '[k]
  AltLabels (k ::* p) = '[k]
  AltLabels (a :|: b) = AltLabels a ++ AltLabels b

type family HsLabels (hs :: Type) :: [Symbol] where
  HsLabels (k :-> x) = '[k]
  HsLabels (a :|: b) = HsLabels a ++ HsLabels b

type family ShowLabels (ls :: [Symbol]) :: ErrorMessage where
  ShowLabels '[] = 'Text "nothing"
  ShowLabels '[l] = 'Text "#" ':<>: 'Text l
  ShowLabels (l ': ls) = 'Text "#" ':<>: 'Text l ':<>: 'Text ", " ':<>: ShowLabels ls

type family NoRepeats (ls :: [Symbol]) (what :: ErrorMessage) :: Constraint where
  NoRepeats '[] what = ()
  NoRepeats (l ': ls) what = (NotIn l ls what, NoRepeats ls what)
type family NotIn (l :: Symbol) (ls :: [Symbol]) (what :: ErrorMessage) :: Constraint where
  NotIn l '[] what = ()
  NotIn l (l ': ls) what = TypeError (what ':<>: 'Text " #" ':<>: 'Text l)
  NotIn l (j ': ls) what = NotIn l ls what

-- | Static labels unique, checked at compile time; counts at preparation.
type AltsOk alts = NoRepeats (AltLabels alts) ('Text "Jev: duplicate label")

-- ---------------------------------------------------------------------------
-- Handlers against alternatives, by label
-- ---------------------------------------------------------------------------

-- | Whether a chain contains a label, used to walk to the side that has it.
type family HasLabel (k :: Symbol) (hs :: Type) :: Bool where
  HasLabel k (k :-> x) = 'True
  HasLabel k (j :-> y) = 'False
  HasLabel k (a :|: b) = HasLabel k a || HasLabel k b

-- | The handler written for a label, or the error that names it.
type HandlerAt :: Symbol -> Type -> Type -> Type
type family HandlerAt k hs all where
  HandlerAt k (k :-> x) all = x
  HandlerAt k (j :-> y) all = TypeError ('Text "no handler for #" ':<>: 'Text k)
  HandlerAt k (a :|: b) all = HandlerAt' (HasLabel k a) k a b all
type family HandlerAt' (hit :: Bool) k a b all where
  HandlerAt' 'True k a b all = HandlerAt k a all
  HandlerAt' 'False k a b all = HandlerAt k b all

-- | Every alternative has a handler, of the shape that alternative needs.
type Covers :: Type -> Type -> Type -> Constraint
type family Covers alts hs r where
  Covers (k ::> p) hs r = HandlerAt k hs hs ~ (p -> r)
  -- A handler of the wrong shape is reported by the functional dependency
  -- on 'Fetch' before this equality is reached, so there is no sentence to
  -- put here that would ever be the one printed.
  Covers (k ::* p) hs r = HandlerAt k hs hs ~ (Text -> p -> r)
  Covers (a :|: b) hs r = (Covers a hs r, Covers b hs r)

-- | Every handler answers an alternative.
type Fits :: Type -> Type -> Constraint
type family Fits hs alts where
  Fits (k :-> x) alts = Known k (AltLabels alts) alts
  Fits (a :|: b) alts = (Fits a alts, Fits b alts)
type family Known (k :: Symbol) (ls :: [Symbol]) alts :: Constraint where
  Known k '[] alts = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " has no alternative; the alternatives are " ':<>: ShowLabels (AltLabels alts))
  Known k (k ': ls) alts = ()
  Known k (j ': ls) alts = Known k ls alts

-- | No alternative is answered twice.
type HandlersOk hs = NoRepeats (HsLabels hs) ('Text "duplicate handler")

-- | Handlers that fit a disjunction: one per alternative, by label, in any
-- order, each taking that alternative's payload.
type Handles hs alts r =
  (Alternatives alts, Dispatch hs alts r, Covers alts hs r, Fits hs alts, HandlersOk hs)

-- | Pull the handler written for one label out of the chain.
class Fetch (k :: Symbol) (hs :: Type) (x :: Type) | k hs -> x where
  fetch :: Alts HandlerT hs -> x

instance Fetch k (k :-> x) x where
  fetch (Hnd h) = h

instance FetchStep (HasLabel k a) k a b x => Fetch k (a :|: b) x where
  fetch (c :| rest) = fetchStep @(HasLabel k a) @k @a @b c rest

class FetchStep (hit :: Bool) (k :: Symbol) (a :: Type) (b :: Type) (x :: Type) | hit k a b -> x where
  fetchStep :: Alts HandlerT a -> Alts HandlerT b -> x

instance Fetch k a x => FetchStep 'True k a b x where
  fetchStep c _ = fetch @k c

instance Fetch k b x => FetchStep 'False k a b x where
  fetchStep _ rest = fetch @k rest

-- | Run the handler the winner's label names.
class Dispatch (hs :: Type) (alts :: Type) (r :: Type) where
  dispatch :: Alts HandlerT hs -> Selected alts -> r

instance Fetch k hs (p -> r) => Dispatch hs (k ::> p) r where
  dispatch hs (SelOne p) = fetch @k hs p

instance Fetch k hs (Text -> p -> r) => Dispatch hs (k ::* p) r where
  dispatch hs (SelRow k p) = fetch @k hs k p

instance (Dispatch hs x r, Dispatch hs rest r) => Dispatch hs (x :|: rest) r where
  dispatch hs = \case
    SelLeft s -> dispatch hs s
    SelRight s -> dispatch hs s

-- ---------------------------------------------------------------------------
-- Alternatives: compile, decode, and eliminate a disjunction shape by shape
-- ---------------------------------------------------------------------------

-- | The selected alternative, carrying the payload it was offered with.
data Selected alts where
  SelOne :: p -> Selected (k ::> p)
  SelRow :: Text -> p -> Selected (k ::* p)
  SelLeft :: Selected x -> Selected (x :|: rest)
  SelRight :: Selected rest -> Selected (x :|: rest)

class Alternatives (alts :: Type) where
  altWire :: JsonValue v => Text -> Alts (Offer v) alts -> Either PrepError [(Text, v)]
  altOffered :: Alts (Offer v) alts -> [(Text, v)]
  altSelect :: Alts (Offer v) alts -> Text -> Maybe (Selected alts)
  altKeyOf :: Selected alts -> Text

instance KnownSymbol k => Alternatives (k ::> p) where
  altWire key (One (d, _)) = checkDescription key (labelText @k) d >> Right [(labelText @k, d)]
  altOffered (One (d, _)) = [(labelText @k, d)]
  altSelect (One (_, p)) sel = if sel == labelText @k then Just (SelOne p) else Nothing
  altKeyOf _ = labelText @k

instance KnownSymbol k => Alternatives (k ::* p) where
  altWire key (Grp es) = do
    let keys = [k | (k, _, _) <- es]
    if length keys /= length (nub keys) then Left (DuplicateKeys key [k | k <- nub keys, length (filter (== k) keys) > 1]) else Right ()
    mapM_ (\(k, d, _) -> checkDescription key k d) es
    Right [(k, d) | (k, d, _) <- es]
  altOffered (Grp es) = [(k, d) | (k, d, _) <- es]
  altSelect (Grp es) sel =
    case [SelRow k p | (k, _, p) <- es, k == sel] of
      e : _ -> Just e
      [] -> Nothing
  altKeyOf (SelRow k _) = k

instance (Alternatives x, Alternatives rest) => Alternatives (x :|: rest) where
  altWire key (c :| rest) = (++) <$> altWire key c <*> altWire key rest
  altOffered (c :| rest) = altOffered c ++ altOffered rest
  altSelect (c :| rest) sel = case altSelect c sel of
    Just s -> Just (SelLeft s)
    Nothing -> SelRight <$> altSelect rest sel
  altKeyOf = \case
    SelLeft s -> altKeyOf s
    SelRight s -> altKeyOf s

-- ---------------------------------------------------------------------------
-- Uniform payloads: the continuation is the payload
-- ---------------------------------------------------------------------------

-- | The same chain with every payload replaced.
type family Retarget (q :: Type) (alts :: Type) :: Type where
  Retarget q (k ::> p) = k ::> q
  Retarget q (k ::* p) = k ::* q
  Retarget q (a :|: b) = Retarget q a :|: Retarget q b

data Dict c where Dict :: c => Dict c

-- | A disjunction whose alternatives all carry the same kind of thing. The
-- payload is then the answer, and there is no handler list to write.
class Alternatives alts => Carries (alts :: Type) (p :: Type) | alts -> p where
  carriedOf :: Selected alts -> p
  -- | Every alternative: its key, its wording, and what it carries.
  carriedRows :: Alts (Offer v) alts -> [(Text, v, p)]
  -- | Map every payload, as a functor over a node type does.
  mapCarried :: (p -> q) -> Alts (Offer v) alts -> Alts (Offer v) (Retarget q alts)
  -- | Mapping the payloads keeps the labels, so the mapped chain is a
  -- disjunction carrying the mapped payload, and is still unique.
  carriedDict :: forall q. Dict
    ( AltLabels (Retarget q alts) ~ AltLabels alts
    , Alternatives (Retarget q alts), Carries (Retarget q alts) q )

instance KnownSymbol k => Carries (k ::> p) p where
  carriedOf (SelOne p) = p
  carriedRows (One (d, p)) = [(labelText @k, d, p)]
  mapCarried f (One (d, p)) = One (d, f p)
  carriedDict = Dict

instance KnownSymbol k => Carries (k ::* p) p where
  carriedOf (SelRow _ p) = p
  carriedRows (Grp es) = es
  mapCarried f (Grp es) = Grp [(k, d, f p) | (k, d, p) <- es]
  carriedDict = Dict

instance (Carries x p, Carries rest p) => Carries (x :|: rest) p where
  carriedOf = \case
    SelLeft s -> carriedOf s
    SelRight s -> carriedOf s
  carriedRows (c :| rest) = carriedRows c ++ carriedRows rest
  mapCarried f (c :| rest) = mapCarried f c :| mapCarried f rest
  carriedDict :: forall q. Dict
    ( AltLabels (Retarget q (x :|: rest)) ~ AltLabels (x :|: rest)
    , Alternatives (Retarget q (x :|: rest)), Carries (Retarget q (x :|: rest)) q )
  carriedDict = case (carriedDict @x @p @q, carriedDict @rest @p @q) of (Dict, Dict) -> Dict

-- | A disjunction whose payloads are all the same, with what it needs kept
-- alongside. A node type that carries its continuations as payloads holds
-- one of these and is a functor over them.
data Uniform (v :: Type) (r :: Type) where
  Uniform :: (AltsOk alts, Carries alts r) => Alts (Offer v) alts -> Uniform v r

uniform :: (AltsOk alts, Carries alts r) => Alts (Offer v) alts -> Uniform v r
uniform = Uniform

mapUniform :: forall v r s. (r -> s) -> Uniform v r -> Uniform v s
mapUniform f (Uniform (o :: Alts (Offer v) alts)) =
  case carriedDict @alts @r @s of Dict -> Uniform (mapCarried f o)

-- | Open a uniform chain. Its alternatives are existential, so the
-- continuation names them at a type only it can see, and a question built
-- there keeps every check a written-out chain gets.
withUniform :: Uniform v r -> (forall alts. (AltsOk alts, Carries alts r) => Alts (Offer v) alts -> x) -> x
withUniform (Uniform o) k = k o

-- | Every branch of a uniform chain: its key, its wording, and what it
-- carries. The keys and wording are the ones a request would send.
branches :: Uniform v r -> [(Text, v, r)]
branches (Uniform o) = carriedRows o

-- ---------------------------------------------------------------------------
-- Rubrics: a chain of bare labels
-- ---------------------------------------------------------------------------

-- | A rubric's levels in order: label, wording, result. Never empty, which
-- is what lets 'grade' fall back to the lowest level with no partial
-- function and no check.
class Levels (levels :: k) where
  levelEntries :: Alts (Level v p) levels -> NonEmpty (Text, v, p)

instance KnownSymbol l => Levels (l :: Symbol) where
  levelEntries (Lvl d p) = (labelText @l, d, p) NE.:| []

instance (Levels x, Levels rest) => Levels ((x :: kx) :|: (rest :: kr)) where
  levelEntries (x :| rest) = levelEntries x <> levelEntries rest

type RubricLabels :: forall k. k -> [Symbol]
type family RubricLabels levels where
  RubricLabels @Symbol l = '[l]
  RubricLabels @Type (x :|: rest) = RubricLabels x ++ RubricLabels rest
  RubricLabels x = TypeError ('Text "Jev: a rubric is a chain of bare labels, such as \"low\" :|: \"high\"; found " ':<>: 'ShowType x)

type family RubricOk (levels :: k) :: Constraint where
  RubricOk levels = NoRepeats (RubricLabels levels) ('Text "Jev: duplicate label")

type family Index (l :: Symbol) (levels :: k) :: Nat where
  Index l levels = IndexIn l (RubricLabels levels)
type family IndexIn (l :: Symbol) (ls :: [Symbol]) :: Nat where
  IndexIn l '[] = TypeError ('Text "Jev: no level #" ':<>: 'Text l ':<>: 'Text " in this rubric")
  IndexIn l (l ': ls) = 0
  IndexIn l (j ': ls) = 1 + IndexIn l ls

-- ---------------------------------------------------------------------------
-- Leaves
-- ---------------------------------------------------------------------------

data instance Q v Noul = NoulQ (Instructions v) (Presence (Maybe (Criteria v)))

-- | What the provider said about a proposition: @a.enough.yes@.
newtype Yes = Yes { yes :: Double }
newtype instance A v Noul = NoulA Yes

data instance Q v (Choice alts) = ChoiceQ (Instructions v) (Alts (Offer v) alts)

-- | What the provider chose, with everything a caller judges it by:
-- @a.next.key@, @a.next.margin@. The fields are all there is to read; the
-- alternative that won is reached only through 'settle', 'handle',
-- 'contenders', 'taken' or 'takenUnder', so a program cannot hold a
-- selection it has not written a branch for.
data Chosen alts = Chosen
  { key :: Text                          -- ^ the winner's wire key
  , mass :: Double                       -- ^ the winner's probability
  , margin :: Double                     -- ^ winner minus runner-up; the mass when it stands alone
  , confidence :: Double                 -- ^ the provider's own confidence
  , masses :: [(Text, Double)]           -- ^ the full distribution, best first
  , won :: Selected alts                 -- the alternative that won
  , ranked :: [(Double, Selected alts)]  -- every alternative by mass, best first
  }
newtype instance A v (Choice alts) = ChoiceA (Chosen alts)

data instance Q v (Score p levels) = ScoreQ (Instructions v) (Alts (Level v p) levels)

-- | Where on the rubric the provider landed: @a.urgency.expectation@,
-- @a.urgency.masses@. The results the rubric was written with are reached
-- only through 'grade'.
data Scored (p :: Type) (levels :: k) = Scored
  { expectation :: Double        -- ^ the expected level index
  , confidence :: Double         -- ^ the provider's own confidence
  , masses :: [(Text, Double)]   -- ^ the distribution, by level label, in level order
  , results :: NonEmpty (Text, p)  -- every level's label and result, in level order
  }
newtype instance A v (Score p levels) = ScoreA (Scored p levels)

newtype instance Q v (Each a e) = EachQ [(Text, a, Q v e)]
newtype instance A v (Each a e) = EachA [(Text, a, A v e)]

newtype instance Q v (Optional e) = OptionalQ (Maybe (Q v e))
newtype instance A v (Optional e) = OptionalA (Maybe (A v e))

newtype instance Q v (Group s) = GroupQ (s (Questions v))
newtype instance A v (Group s) = GroupA (s (Answers v))

-- Internal readers: 'confidence' and 'masses' are fields of two records, so
-- the module names them by pattern rather than by an ambiguous selector.
chosenMasses :: Chosen alts -> [(Text, Double)]
chosenMasses Chosen { masses = ms } = ms

chosenConfidence :: Chosen alts -> Double
chosenConfidence Chosen { confidence = c } = c

scoreMasses :: Scored p levels -> [(Text, Double)]
scoreMasses Scored { masses = ms } = ms

scoreConfidence :: Scored p levels -> Double
scoreConfidence Scored { confidence = c } = c

-- | Answers print as their own fields. Probabilities are shown to two
-- decimals: they are a provider's judgment, not an exact quantity.
instance Show Yes where
  show a = "Noul {yes = " <> T.unpack (fmt2 (yes a)) <> "}"

instance Show (Chosen alts) where
  show a@Chosen { key = k, mass = m, margin = g } =
    "Choice {key = " <> show k <> ", mass = " <> T.unpack (fmt2 m) <> ", margin = " <> T.unpack (fmt2 g)
      <> ", confidence = " <> T.unpack (fmt2 (chosenConfidence a)) <> ", masses = " <> T.unpack (showMasses (chosenMasses a)) <> "}"

instance Show (Scored p levels) where
  show a@Scored { expectation = e } =
    "Score {expectation = " <> T.unpack (fmt2 e)
      <> ", confidence = " <> T.unpack (fmt2 (scoreConfidence a)) <> ", masses = " <> T.unpack (showMasses (scoreMasses a)) <> "}"

fmt2 :: Double -> Text
fmt2 x = T.pack (showFFloat (Just 2) x "")

showMasses :: [(Text, Double)] -> Text
showMasses ms = "[" <> T.intercalate ", " [T.pack (show k) <> " " <> fmt2 m | (k, m) <- ms] <> "]"

-- ---------------------------------------------------------------------------
-- Builders (all total; shapes are checked at preparation)
-- ---------------------------------------------------------------------------

noul :: JsonValue v => Text -> Q v Noul
noul t = NoulQ (question t) Omitted

choice :: forall alts v. (JsonValue v, AltsOk alts) => Text -> Alts (Offer v) alts -> Q v (Choice alts)
choice t = ChoiceQ (question t)

score :: forall levels p v. (JsonValue v, RubricOk levels) => Text -> Alts (Level v p) levels -> Q v (Score p levels)
score t = ScoreQ (question t)

-- | One question per row, keyed at runtime: the per-item battery. Written
-- as 'many' is, a wire key and a question per row, and the row itself comes
-- back beside its answer, so there is nothing to look up afterwards. A cell
-- holds a question or a nested packet, and so does this.
each :: (ToQ x, NestedQ x (QJson x)) => (a -> Text) -> (a -> x) -> [a] -> Q (QJson x) (Each a (QKind x))
each key q rows = EachQ [(key r, r, toQ (q r)) | r <- rows]

-- | A question or nested packet that may be absent. An absent question
-- sends nothing; its answer is 'Nothing'. A present question uses the
-- containing cell's path, with no synthetic key or row.
optional :: (ToQ x, NestedQ x (QJson x)) => Maybe x -> Q (QJson x) (Optional (QKind x))
optional = OptionalQ . fmap toQ

-- ---------------------------------------------------------------------------
-- Results
-- ---------------------------------------------------------------------------

-- | What a policy weighs: the winner, its mass, its margin over the
-- runner-up, and the provider's confidence where the wire carries one.
data Weight = Weight
  { winner :: Text
  , winnerMass :: Double
  , runnerUp :: Maybe (Text, Double)
  , winnerConfidence :: Maybe Double
  }

-- | Answers a 'Policy' can weigh. A choice weighs its distribution; a Noul
-- weighs yes against no, with no confidence to consult.
class Weighed a where
  weigh :: a -> Weight

instance Weighed (Chosen alts) where
  weigh a@Chosen { key = k, mass = m } = Weight
    { winner = k
    , winnerMass = m
    , runnerUp = case [r | r@(k', _) <- chosenMasses a, k' /= k] of { r : _ -> Just r; [] -> Nothing }
    , winnerConfidence = Just (chosenConfidence a)
    }

instance Weighed Yes where
  weigh (Yes y)
    | y >= 0.5 = Weight "yes" y (Just ("no", 1 - y)) Nothing
    | otherwise = Weight "no" (1 - y) (Just ("yes", y)) Nothing

-- | Which floor the answer failed.
data Cause
  = NearTie (Text, Double) (Text, Double)  -- ^ winner and runner-up too close
  | Underweight Double                     -- ^ the winner's mass is below the floor
  | Unconfident Double                     -- ^ the provider's confidence is below the floor
  deriving (Show, Eq)

-- | An answer the policy would not stand behind, and the line that says
-- why. The line is the one a log, a notification or a planner reads, so
-- nothing has to be rebuilt from the policy and the answer.
data Doubt = Doubt { cause :: Cause, why :: Text } deriving (Show, Eq)

-- | Three floors. The tag records which policy weighed an answer, so a
-- step that must not be taken lightly can demand a verdict from the policy
-- that suits it.
data Policy (p :: Type) = Policy
  { minMass :: Double
  , minMargin :: Double
  , minConfidence :: Double
  } deriving (Show, Eq)

data Lenient
data Careful
data Strict

-- | A verdict, carrying the policy that reached it. A function that merges,
-- stops or files a receipt takes @Settled Strict r@ and no lesser verdict
-- will do.
newtype Settled (p :: Type) (r :: Type) = Settled r deriving (Show, Eq)

instance Functor (Settled p) where
  fmap f (Settled r) = Settled (f r)

doubt :: Weighed a => Policy p -> a -> Maybe Doubt
doubt policy a =
  let w = weigh a
      out c = Just (Doubt c (say policy a (Just c)))
  in case winnerConfidence w of
    Just c | c < minConfidence policy -> out (Unconfident c)
    _ | winnerMass w < minMass policy -> out (Underweight (winnerMass w))
    _ | Just (k2, m2) <- runnerUp w, winnerMass w - m2 < minMargin policy -> out (NearTie (winner w, winnerMass w) (k2, m2))
    _ -> Nothing

-- | The winner under a policy, or a structured doubt. This consumer
-- requires a handler for every alternative, including "no" or "missing".
-- Success supports the selected alternative, not permission to proceed.
settle :: forall alts hs r p. Handles hs alts r => Policy p -> Chosen alts -> Alts HandlerT hs -> Either Doubt (Settled p r)
settle policy a hs = maybe (Right (Settled (handle a hs))) Left (doubt policy a)

-- | 'taken' under a policy: no handlers to repeat when all alternatives
-- already carry the same result type. The author assigns meaning to every
-- payload, including an explicit no-op such as 'Nothing'.
takenUnder :: Carries alts r => Policy p -> Chosen alts -> Either Doubt (Settled p r)
takenUnder policy a = maybe (Right (Settled (taken a))) Left (doubt policy a)

-- | A proposition under a policy: yes, no, or a structured doubt when the
-- provider was not clear either way.
judge :: Policy p -> Yes -> Either Doubt (Settled p Bool)
judge policy a = maybe (Right (Settled (yes a >= 0.5))) Left (doubt policy a)

-- | Whether a proposition holds under a policy: a settled yes, and
-- nothing else. A doubt is not a no, so both read as 'False' here. A
-- caller that must tell them apart uses 'judge', which keeps the doubt and
-- the line that says why.
holds :: Policy p -> Yes -> Bool
holds policy a = case judge policy a of
  Right (Settled b) -> b
  Left _ -> False

-- | One line saying why the policy settled or doubted the answer, with the
-- numbers behind it. A doubt already carries this line as its @why@; this
-- is how to get it for an answer that settled.
explain :: Weighed a => Policy p -> a -> Text
explain policy a = say policy a (fmap cause (doubt policy a))

say :: Weighed a => Policy p -> a -> Maybe Cause -> Text
say policy a mc =
  let w = weigh a
      margin = maybe (winnerMass w) (\(_, m2) -> winnerMass w - m2) (runnerUp w)
      items = [("confidence" :: Text, c, minConfidence policy) | Just c <- [winnerConfidence w]]
           ++ [("mass", winnerMass w, minMass policy), ("margin", margin, minMargin policy)]
  in case mc of
    Nothing -> "settled on " <> winner w <> ": " <> T.intercalate ", " [n <> " " <> fmt2 v <> " \8805 " <> fmt2 t | (n, v, t) <- items]
    Just d ->
      let (ctor, failedName, failedValue, floorValue) = case d of
            Unconfident c -> ("Unconfident", "confidence" :: Text, c, minConfidence policy)
            Underweight m -> ("Underweight", "mass", m, minMass policy)
            NearTie (_, m) (_, m2) -> ("NearTie", "margin", m - m2, minMargin policy)
          floorLine = failedName <> " " <> fmt2 failedValue <> " < " <> fmt2 floorValue <> " by " <> fmt2 (floorValue - failedValue)
          rest = [n <> " " <> fmt2 v | (n, v, _) <- items, n /= failedName]
      in "doubted " <> winner w <> " (" <> ctor <> "): " <> floorLine <> "; " <> T.intercalate ", " rest

-- | The winner against the handler its label names, with no policy: for
-- when the program follows whatever came back. A missing, extra or
-- duplicated handler is a type error naming the label.
handle :: forall alts hs r. Handles hs alts r => Chosen alts -> Alts HandlerT hs -> r
handle a hs = dispatch hs (won a)

-- | The payload the winner was offered with, when every alternative carries
-- the same kind of thing. Having them all is exhaustiveness by
-- construction, so there is no handler list to write.
taken :: Carries alts r => Chosen alts -> r
taken a = carriedOf (won a)

-- | Every alternative at or above a mass floor, best first, each already
-- through the same handlers. The one way to act on a runner-up.
contenders :: forall alts hs r. Handles hs alts r => Double -> Chosen alts -> Alts HandlerT hs -> [(Double, r)]
contenders floor' a hs = [(m, dispatch hs s) | (m, s) <- ranked a, m >= floor']

-- | Run the result for the level the score landed on. Levels run lowest to
-- highest, so this walks from the highest down and takes the first whose
-- mass at or above it clears the floor, and the lowest level when none
-- does. At a floor of 0.5 that is the median level.
--
-- There is always an answer: an ordinal scale has a median even when the
-- distribution is flat, which is why this gives no 'Doubt'. Every level
-- carries its result from the moment it is written, so there is no list to
-- check and no label string to dispatch on.
grade :: Double -> Scored p levels -> p
grade floor' = snd . graded floor'

-- | 'grade', with the label of the level it landed on. A ledger line that
-- names the level then reads it from the answer instead of a label written
-- a second time into the result.
graded :: Double -> Scored p levels -> (Text, p)
graded floor' a =
  let rs = results a
      atOrAbove = drop 1 (scanr (+) 0 (map snd (scoreMasses a)))
  in foldl (\landed (r, m) -> if m >= floor' then r else landed) (NE.head rs) (zip (NE.tail rs) atOrAbove)

-- | Mass at or beyond a level, by label.
massAtOrAbove :: forall l levels p. KnownNat (Index l levels) => Label l -> Scored p levels -> Double
massAtOrAbove _ a = sum [m | (i, m) <- zip [0 :: Integer ..] (map snd (scoreMasses a)), i >= natVal (Proxy @(Index l levels))]

-- ---------------------------------------------------------------------------
-- Paths and compilation output
-- ---------------------------------------------------------------------------

data Path = Segments [Text] | Exactly Text

encodePath :: Path -> Text
encodePath = \case
  Exactly k -> k
  Segments ss -> T.intercalate "." (map escape ss)
  where
    escape = T.concatMap (\c -> case c of
      '\\' -> "\\\\"
      '.' -> "\\."
      _ -> T.singleton c)

extend :: Path -> Text -> Path
extend (Segments ss) s = Segments (ss ++ [s])
extend (Exactly k) s = Segments [k, s]

-- ---------------------------------------------------------------------------
-- Endpoints: compile, decode, unwrap, preview
-- ---------------------------------------------------------------------------

class JsonValue v => Endpoint v e where
  compileQ :: Path -> Q v e -> Either PrepError [(Text, WireQuestion v)]
  decodeA :: Path -> Q v e -> [(Text, v)] -> Either DecodeError (A v e)
  -- | The answer as a cell reads it (transparent for nesting).
  unwrapA :: A v e -> Answers v :- e
  -- | A payload-independent summary for inspection.
  previewA :: A v e -> v

lookupAnswer :: Path -> [(Text, v)] -> Either DecodeError v
lookupAnswer p ws = maybe (Left (MissingAnswer key)) Right (lookup key ws)
  where key = encodePath p

leaf :: Text -> WireQuestion v -> [(Text, WireQuestion v)]
leaf key q = [(key, q)]

instance JsonValue v => Endpoint v Noul where
  compileQ p (NoulQ i c) = do
    let key = encodePath p
    checkInstructions key i
    case c of
      Present (Just (Criteria y n)) -> do
        mapM_ (checkDescription key "true") [d | Present d <- [y]]
        mapM_ (checkDescription key "false") [d | Present d <- [n]]
      _ -> Right ()
    Right (leaf key (WNoul i c))
  decodeA p _ ws = lookupAnswer p ws >>= \v -> do
    NoulAnswer x <- parseNoul (encodePath p) v
    Right (NoulA (Yes x))
  unwrapA (NoulA a) = a
  previewA (NoulA a) = jObject [("yes", jNumber (yes a))]

instance (JsonValue v, Alternatives alts) => Endpoint v (Choice alts) where
  compileQ p (ChoiceQ i offer) = do
    let key = encodePath p
    checkInstructions key i
    alts <- altWire key offer
    if null alts then Left (EmptyOffer key) else Right ()
    if length alts > 255 then Left (TooManyAlternatives key (length alts)) else Right ()
    case [k | k <- map fst alts, length (filter (== k) (map fst alts)) > 1] of
      k : _ -> Left (KeyCollidesWithLabel key k)
      [] -> Right ()
    Right (leaf key (WChoice i alts))
  decodeA p (ChoiceQ _ offer) ws = lookupAnswer p ws >>= \v -> do
    let key = encodePath p
    ChoiceAnswer sel ms conf <- parseChoice key v
    alts <- altWire key offer `orDecode` key
    picked <- maybe (Left (UnknownSelection key sel)) Right (altSelect offer sel)
    distribution key (map fst alts) ms conf
    let best = sortOn (negate . snd) ms
        everyAlt = [(m, s) | (k, m) <- best, Just s <- [altSelect offer k]]
        pickedKey = altKeyOf picked
        pickedMass = maybe 0 id (lookup pickedKey best)
        beaten = [m | (k, m) <- best, k /= pickedKey]
        pickedMargin = case beaten of { m : _ -> pickedMass - m; [] -> pickedMass }
    Right (ChoiceA Chosen
      { key = pickedKey
      , mass = pickedMass
      , margin = pickedMargin
      , confidence = conf
      , masses = best
      , won = picked
      , ranked = everyAlt
      })
  unwrapA (ChoiceA a) = a
  previewA (ChoiceA a@Chosen { key = k, mass = m, margin = g }) = jObject
    [ ("key", jString k)
    , ("mass", jNumber m)
    , ("margin", jNumber g)
    , ("confidence", jNumber (chosenConfidence a))
    , ("masses", jObject [(mk, jNumber mm) | (mk, mm) <- chosenMasses a])
    ]

orDecode :: Either PrepError x -> Text -> Either DecodeError x
orDecode e key = either (const (Left (Malformed key "retained offer failed to render"))) Right e

instance (JsonValue v, Levels levels) => Endpoint v (Score p levels) where
  compileQ p (ScoreQ i rubric) = do
    let key = encodePath p
        wordings = [w | (_, w, _) <- NE.toList (levelEntries rubric)]
    checkInstructions key i
    if length wordings > 10 then Left (BadLevelCount key (length wordings)) else Right ()
    mapM_ (\(ix, l) -> checkLevel key ix l) (zip [0 ..] wordings)
    Right (leaf key (WScore i wordings))
  decodeA p (ScoreQ _ rubric) ws = lookupAnswer p ws >>= \v -> do
    let key = encodePath p
        entries = levelEntries rubric
        labels = [l | (l, _, _) <- NE.toList entries]
        indices = [T.pack (show i) | i <- [0 .. length labels - 1]]
    ScoreAnswer e lg ms conf <- parseScore key v
    distribution key indices ms conf
    checkLegend key [w | (_, w, _) <- NE.toList entries] lg
    checkExpectation key (length labels) e
    let byIndex = [(l, maybe 0 id (lookup i ms)) | (i, l) <- zip indices labels]
    Right (ScoreA Scored { expectation = e, confidence = conf, masses = byIndex, results = fmap (\(l, _, r) -> (l, r)) entries })
  unwrapA (ScoreA a) = a
  previewA (ScoreA a@Scored { expectation = e }) = jObject
    [ ("expectation", jNumber e)
    , ("confidence", jNumber (scoreConfidence a))
    , ("masses", jObject [(ml, jNumber m) | (ml, m) <- scoreMasses a])
    ]

checkLegend :: JsonValue v => Text -> [v] -> [(Text, v)] -> Either DecodeError ()
checkLegend key levels lg =
  let indices = [T.pack (show i) | i <- [0 .. length levels - 1]]
      matches = and [maybe False (jEqual l) (lookup i lg) | (i, l) <- zip indices levels]
  in if not matches || length lg /= length indices || any (`notElem` indices) (map fst lg)
       then Left (LegendMismatch key) else Right ()

checkExpectation :: Text -> Int -> Double -> Either DecodeError ()
checkExpectation key n e =
  if isNaN e || isInfinite e || e < 0 || e > fromIntegral (n - 1) then Left (ValueOutOfRange key "score") else Right ()

instance Endpoint v e => Endpoint v (Each a e) where
  compileQ p (EachQ items) = concat <$> mapM (\(k, _, q) -> compileQ (extend p k) q) items
  decodeA p (EachQ items) ws = EachA <$> mapM (\(k, r, q) -> (,,) k r <$> decodeA (extend p k) q ws) items
  unwrapA (EachA xs) = [(r, unwrapA x) | (_, r, x) <- xs]
  previewA (EachA xs) = jObject [(k, previewA x) | (k, _, x) <- xs]

instance Schema v s => Endpoint v (Group s) where
  compileQ p (GroupQ q) = compileSchema p q
  decodeA p (GroupQ q) ws = GroupA <$> decodeSchema p q ws
  unwrapA (GroupA x) = x
  previewA (GroupA x) = previewSchema x

instance Endpoint v e => Endpoint v (Optional e) where
  compileQ p (OptionalQ q) = maybe (Right []) (compileQ p) q
  decodeA p (OptionalQ q) ws = OptionalA <$> traverse (\x -> decodeA p x ws) q
  unwrapA (OptionalA a) = fmap unwrapA a
  previewA (OptionalA a) = maybe jNull previewA a

-- ---------------------------------------------------------------------------
-- Packets: a cell is a packet of one, and two packets join
-- ---------------------------------------------------------------------------

-- | A labeled cell.
data (k :: Symbol) ::= (e :: Type)
-- | Two packets joined. The type mirrors the value, so a lookup walks the
-- shape the author wrote and packets compose.
data (a :: Type) :& (b :: Type)
infix 6 ::=

-- | What a cell holds under each mode: a question or a nested packet, an
-- answer, or a state field at its own Haskell type.
data Cell (e :: Type) (mode :: Type) where
  Asked :: Q v e -> Cell e (Questions v)
  Answered :: A v e -> Cell e (Answers v)
  Given :: Field v a => a -> Cell a (Fields v)

class ToCell (x :: Type) (mode :: Type) where
  type CellOf x mode :: Type
  toCell :: x -> Cell (CellOf x mode) mode

instance (ToQ x, QJson x ~ v) => ToCell x (Questions v) where
  type CellOf x (Questions v) = QKind x
  toCell = Asked . toQ

instance ToCell (A v e) (Answers v) where
  type CellOf (A v e) (Answers v) = e
  toCell = Answered

instance Field v a => ToCell a (Fields v) where
  type CellOf a (Fields v) = a
  toCell = Given

-- | What a cell needs to be sent, read back, or rendered.
type family CellOk (mode :: Type) (e :: Type) :: Constraint where
  CellOk (Questions v) e = Endpoint v e
  CellOk (Answers v) e = Endpoint v e
  CellOk (Fields v) a = Field v a

-- | A nested packet is written in the mode of the packet that holds it, so
-- the mode travels inward and nothing needs annotating.
type family Nested (x :: Type) (mode :: Type) :: Constraint where
  Nested (Packet t m) mode = (m ~ mode)
  Nested x mode = ()

-- | A nested packet inside a battery is in the questions mode too, so one
-- written there needs no annotation either.
type family NestedQ (x :: Type) (v :: Type) :: Constraint where
  NestedQ (Packet t m) v = (m ~ Questions v)
  NestedQ x v = ()

data Packet (t :: Type) (mode :: Type) where
  (:=) :: (KnownSymbol k, Nested x mode, ToCell x mode, CellOk mode (CellOf x mode))
       => Label k -> x -> Packet (k ::= CellOf x mode) mode
  (:&) :: Packet a mode -> Packet b mode -> Packet (a :& b) mode
infix 6 :=
infixr 5 :&

type family PacketLabels (t :: Type) :: [Symbol] where
  PacketLabels (k ::= e) = '[k]
  PacketLabels (a :& b) = PacketLabels a ++ PacketLabels b
  PacketLabels t = '[]

type family HasCell (k :: Symbol) (t :: Type) :: Bool where
  HasCell k (k ::= e) = 'True
  HasCell k (j ::= e) = 'False
  HasCell k (a :& b) = HasCell k a || HasCell k b
  -- A state sent as given has no cells at all, which is what makes the
  -- raw case of 'HasField' on a 'State' unreachable rather than undefined.
  HasCell k t = 'False

-- | Every label in a packet is written once.
type Unique t = NoRepeats (PacketLabels t) ('Text "Jev: duplicate packet label")

-- | Field access on a packet, carrying the full label list for the error.
class Get (k :: Symbol) (t :: Type) (all :: Type) (e :: Type) | k t all -> e where
  getCell :: Packet t mode -> Cell e mode

instance GetLeaf (SameLabel k j) k j e' all e => Get k (j ::= e') all e where
  getCell = getLeaf @(SameLabel k j) @k @j @e' @all

instance GetSide (HasCell k a) k a b all e => Get k (a :& b) all e where
  getCell (x :& y) = getSide @(HasCell k a) @k @a @b @all x y

class GetLeaf (hit :: Bool) (k :: Symbol) (j :: Symbol) (e' :: Type) (all :: Type) (e :: Type) | hit k j e' all -> e where
  getLeaf :: Packet (j ::= e') mode -> Cell e mode
instance e ~ e' => GetLeaf 'True k j e' all e where
  getLeaf ((_ :: Label j) := (x :: x)) = toCell @x x
instance (TypeError ('Text "Jev: this packet has no #" ':<>: 'Text k ':<>: 'Text "; it has " ':<>: ShowLabels (PacketLabels all)), e ~ ())
  => GetLeaf 'False k j e' all e where
  getLeaf = undefined

class GetSide (left :: Bool) (k :: Symbol) (a :: Type) (b :: Type) (all :: Type) (e :: Type) | left k a b all -> e where
  getSide :: Packet a mode -> Packet b mode -> Cell e mode
instance Get k a all e => GetSide 'True k a b all e where
  getSide x _ = getCell @k @a @all x
instance Get k b all e => GetSide 'False k a b all e where
  getSide _ y = getCell @k @b @all y

-- | A response reads by its packet's labels, and so does a state.
instance (Get k t t e, r ~ (Answers v :- e), Endpoint v e) => HasField k (Packet t (Answers v)) r where
  getField p = case getCell @k @t @t p of Answered a -> unwrapA a
  {-# INLINE getField #-}

-- ---------------------------------------------------------------------------
-- Questions, as cell contents
-- ---------------------------------------------------------------------------

type family QKind (x :: Type) :: Type where
  QKind (Q v e) = e
  QKind (Packet t (Questions v)) = Group (Packet t)
  QKind x = TypeError ('Text "Jev: a cell holds a question or a nested packet; this is " ':<>: 'ShowType x)

type family QJson (x :: Type) :: Type where
  QJson (Q v e) = v
  QJson (Packet t (Questions v)) = v

class ToQ (x :: Type) where
  toQ :: x -> Q (QJson x) (QKind x)
instance ToQ (Q v e) where toQ = id
instance ToQ (Packet t (Questions v)) where toQ = GroupQ

-- ---------------------------------------------------------------------------
-- State: the shared input, with its fields kept
-- ---------------------------------------------------------------------------

-- | How a Haskell value becomes a state field. A nested state is a field,
-- so a state's shape is written the way a packet is.
class JsonValue v => Field v a where
  toField :: a -> v

instance JsonValue v => Field v Text where toField = jString
instance JsonValue v => Field v Bool where toField = jBool
instance JsonValue v => Field v Int where toField = jNumber . fromIntegral
instance JsonValue v => Field v Double where toField = jNumber
instance {-# OVERLAPPABLE #-} Field v a => Field v [a] where toField = jArray . map toField
-- A list of keyed things is an object; pairs have no other reading here.
instance {-# OVERLAPPING #-} Field v a => Field v [(Text, a)] where
  toField xs = jObject [(k, toField x) | (k, x) <- xs]
instance Field v a => Field v (Maybe a) where
  toField = maybe jNull toField
instance (JsonValue v, Unique t) => Field v (Packet t (Fields v)) where
  toField = jObject . fieldPairs

-- | The shared input to every question. Its fields keep their Haskell
-- types, so a row the state carries is the row a question is built from.
-- A state sent as given keeps no packet, and its index says so, so the
-- two cases never need a fallback that cannot happen.
data State (v :: Type) (t :: Type) where
  Typed :: Packet t (Fields v) -> v -> State v t
  Raw :: v -> State v ()

-- | A state written the way a packet is.
state :: (JsonValue v, Unique t) => Packet t (Fields v) -> State v t
state p = Typed p (jObject (fieldPairs p))

-- | A state sent as given, for a shape the authoring surface leaves out.
-- The fields of such a state cannot be referenced.
rawState :: v -> State v ()
rawState = Raw

stateValue :: State v t -> v
stateValue = \case
  Typed _ v -> v
  Raw v -> v

-- | A state reads by its own labels. The witness in the context is what a
-- field access already proves, and it also rules out the raw state, whose
-- index carries no cells: that case is inaccessible, not unwritten.
instance (HasCell k t ~ 'True, Get k t t a, r ~ a) => HasField k (State v t) r where
  getField = \case Typed p _ -> case getCell @k @t @t p of Given a -> a

instance (Get k t t a, r ~ a) => HasField k (Packet t (Fields v)) r where
  getField p = case getCell @k @t @t p of Given a -> a

-- | The name of a state field, as wording refers to it. A name the state
-- does not have is a compile error listing the names it does. Intermediate
-- fields must be nested packets; the final field may have any type.
field :: StatePath ks t => FieldPath ks -> State v t -> Text
field path _ = "`" <> renderFieldPath path <> "`"

-- | A nonempty path of state labels. Bare labels name top-level fields;
-- @#gate :/ #posters@ names a field inside a nested state packet.
data FieldPath (ks :: [Symbol]) where
  FieldLabel :: KnownSymbol k => Label k -> FieldPath '[k]
  (:/) :: KnownSymbol k => Label k -> FieldPath ks -> FieldPath (k ': ks)
infixr 6 :/

instance (KnownSymbol k, ks ~ '[k]) => IsLabel k (FieldPath ks) where
  fromLabel = FieldLabel (Label @k)

-- State references are model-facing wording, not question wire keys.
-- They use the same escaping convention, but have their own renderer.
renderFieldPath :: FieldPath ks -> Text
renderFieldPath = T.intercalate "." . map escape . segments
  where
    segments :: FieldPath ls -> [Text]
    segments (FieldLabel l) = [labelOf l]
    segments (l :/ rest) = labelOf l : segments rest
    escape = T.concatMap (\c -> case c of
      '\\' -> "\\\\"
      '.' -> "\\."
      _ -> T.singleton c)

-- | Every intermediate field is a nested packet; the final field may
-- have any type. Missing labels are reported against their own packet.
type family StatePath (ks :: [Symbol]) (t :: Type) :: Constraint where
  StatePath '[k] t = StateHas k t
  StatePath (k ': rest) t = DescendState k (StateField k t t) rest

type family StateField (k :: Symbol) (t :: Type) (all :: Type) :: Type where
  StateField k (k ::= a) all = a
  StateField k (j ::= a) all = TypeError
    ('Text "Jev: this state has no #" ':<>: 'Text k ':<>: 'Text "; it has " ':<>: ShowLabels (PacketLabels all))
  StateField k (a :& b) all = StateFieldSide (HasCell k a) k a b all

type family StateFieldSide (left :: Bool) k a b all :: Type where
  StateFieldSide 'True k a b all = StateField k a all
  StateFieldSide 'False k a b all = StateField k b all

type family DescendState (k :: Symbol) (a :: Type) (rest :: [Symbol]) :: Constraint where
  DescendState k (Packet t (Fields v)) rest = StatePath rest t
  DescendState k a rest = TypeError
    ('Text "Jev: cannot descend through #" ':<>: 'Text k
     ':<>: 'Text "; expected a nested state packet, found " ':<>: 'ShowType a)

type StateHas k t = StateHas' (HasCell k t) k t
type family StateHas' (there :: Bool) (k :: Symbol) (t :: Type) :: Constraint where
  StateHas' 'True k t = ()
  StateHas' 'False k t =
    TypeError ('Text "Jev: this state has no #" ':<>: 'Text k ':<>: 'Text "; it has " ':<>: ShowLabels (PacketLabels t))

fieldPairs :: forall v t. JsonValue v => Packet t (Fields v) -> [(Text, v)]
fieldPairs = \case
  (l :: Label k) := (x :: x) -> [(labelOf l, render (toCell @x @(Fields v) x))]
  a :& b -> fieldPairs a ++ fieldPairs b
  where
    render :: Cell e (Fields v) -> v
    render (Given a) = toField a

-- ---------------------------------------------------------------------------
-- Schemas
-- ---------------------------------------------------------------------------

class JsonValue v => Schema v (s :: Type -> Type) where
  compileSchema :: Path -> s (Questions v) -> Either PrepError [(Text, WireQuestion v)]
  decodeSchema :: Path -> s (Questions v) -> [(Text, v)] -> Either DecodeError (s (Answers v))
  previewSchema :: s (Answers v) -> v

instance (JsonValue v, Unique t) => Schema v (Packet t) where
  compileSchema = packetCompile
  decodeSchema = packetDecode
  previewSchema = jObject . packetPreview

packetCompile :: forall v t. JsonValue v => Path -> Packet t (Questions v) -> Either PrepError [(Text, WireQuestion v)]
packetCompile p = \case
  (l :: Label k) := (x :: x) -> case toCell @x @(Questions v) x of
    Asked q -> compileQ (extend p (labelOf l)) q
  a :& b -> (++) <$> packetCompile p a <*> packetCompile p b

packetDecode :: forall v t. JsonValue v => Path -> Packet t (Questions v) -> [(Text, v)] -> Either DecodeError (Packet t (Answers v))
packetDecode p q ws = case q of
  (l :: Label k) := (x :: x) -> case toCell @x @(Questions v) x of
    Asked qq -> (l :=) <$> decodeA (extend p (labelOf l)) qq ws
  a :& b -> (:&) <$> packetDecode p a ws <*> packetDecode p b ws

packetPreview :: forall v t. JsonValue v => Packet t (Answers v) -> [(Text, v)]
packetPreview = \case
  (l :: Label k) := (x :: x) -> case toCell @x @(Answers v) x of
    Answered a -> [(labelOf l, previewA a)]
  a :& b -> packetPreview a ++ packetPreview b

instance (JsonValue v, Show v, Unique t) => Show (Packet t (Answers v)) where
  show p = show (jObject (packetPreview p))

-- ---------------------------------------------------------------------------
-- The operation: request, decode
-- ---------------------------------------------------------------------------

newtype Model = Model Text deriving (Eq, Show)
instance IsString Model where fromString = Model . T.pack

jevLatest :: Model
jevLatest = Model "jev-latest"

-- | A transport and the model it is asked for, bound once.
data Session (m :: Type -> Type) (v :: Type) = Session (v -> m (Either Text v)) Model

session :: (v -> m (Either Text v)) -> Model -> Session m v
session = Session

-- | The flattened questions, with every preparation check applied.
prepareWire :: Schema v s => s (Questions v) -> Either PrepError [(Text, WireQuestion v)]
prepareWire q = do
  qs <- compileSchema (Segments []) q
  let keys = map fst qs
  if null qs then Left EmptyQuestionMap else Right ()
  case [k | k <- keys, T.null k] of
    _ : _ -> Left (EmptyQuestionKey "")
    [] -> Right ()
  case [k | k <- keys, length (filter (== k) keys) > 1] of
    k : _ -> Left (DuplicateQuestionPath k)
    [] -> Right ()
  Right qs

-- | The request body a transport sends.
request :: Schema v s => Model -> State v t -> s (Questions v) -> Either JevError v
request (Model m) st q = either (Left . Prepare) Right $ do
  checkState (stateValue st)
  qs <- prepareWire q
  Right (jObject
    [ ("model", jString m)
    , ("state", stateValue st)
    , ("questions", jObject [(k, questionValue w) | (k, w) <- qs])
    ])

data Response v s = Response (s (Answers v)) Text v [Text]

-- | The packet, under 'Answers'. Usually unnecessary: a response reads by
-- its packet's own labels, @r.next@, through the instance below.
answers :: Response v s -> s (Answers v)
answers (Response a _ _ _) = a

-- | The model the request resolved to, as the envelope reported it.
responseModel :: Response v s -> Text
responseModel (Response _ m _ _) = m

-- | The token counts for the call, as the provider sent them.
usage :: Response v s -> v
usage (Response _ _ u _) = u

-- | Distributions that do not sum to one, and the like: worth a log line,
-- never a rejection.
diagnostics :: Response v s -> [Text]
diagnostics (Response _ _ _ d) = d

-- | A response reads by the labels of the packet that produced it:
-- @r.next.key@. A label the packet lacks is the same compile error it is on
-- the packet itself.
instance HasField k (s (Answers v)) r => HasField k (Response v s) r where
  getField = getField @k . answers

-- | A response prints as its answers: the packet's labels over each
-- answer's own fields, nested packets nested.
instance (Show v, Schema v s) => Show (Response v s) where
  show r = show (previewSchema (answers r))

-- | Decode a response body against the packet that produced the request.
decode :: Schema v s => s (Questions v) -> v -> Either JevError (Response v s)
decode q body = do
  qs <- either (Left . Prepare) Right (prepareWire q)
  either (Left . Decode) Right $ parseEnvelope body >>= \case
    Rejected r -> Left (ProviderRejected r)
    Evaluated model use ws -> do
      let expected = map fst qs
          got = map fst ws
      case filter (`notElem` expected) got of
        k : _ -> Left (UnexpectedAnswer k)
        [] -> Right ()
      case [k | k <- got, length (filter (== k) got) > 1] of
        k : _ -> Left (DuplicateAnswer k)
        [] -> Right ()
      ans <- decodeSchema (Segments []) q ws
      let drift = [ k <> ": distribution sums to " <> T.pack (show total)
                  | (k, a) <- ws, Just total <- [driftOf a], abs (total - 1) > 0.01 ]
      Right (Response ans model use drift)

data JevError = Prepare PrepError | Transport Text | Decode DecodeError
  deriving (Show, Eq)

roundTrip
  :: (Monad m, Schema v s)
  => Session m v -> State v t -> s (Questions v)
  -> m (Either JevError (Response v s))
roundTrip (Session transport model) st q = case request model st q of
  Left e -> pure (Left e)
  Right body -> transport body >>= \case
    Left t -> pure (Left (Transport t))
    Right resp -> pure (decode q resp)

-- | The tiny use: one question, one answer.
jev1
  :: forall m v e t. (Monad m, Endpoint v e)
  => Session m v -> State v t -> Q v e
  -> m (Either JevError (Answers v :- e))
jev1 sess st q = fmap (fmap only) (roundTrip sess st ((Label :: Label "value") := q))
  where
    only r = case answers r of
      (_ :: Label k) := (a :: a) -> case toCell @a @(Answers v) a of Answered x -> unwrapA x
