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

-- | The agent-facing form: an anonymous, type-indexed packet of questions,
-- alternatives and rubric levels as type-level chains of labels.
-- Polymorphic over the JSON value through "Jev.Core.Json";
-- "Jev.Operators" fixes it.
--
-- The packet's type is inferred from the questions written. Labels,
-- and handler completeness are checked at compile time with messages in
-- the author's vocabulary; wording, runtime candidates and level counts
-- are checked at preparation.
module Jev.Core.Schema
  ( -- * Modes
    Questions, Answers, type (:-)
    -- * Packets
  , type (::=), Label (..), Cell (..), CellKind, CellJson, ToQ, Packet (..)
  , Unique, Get
    -- * Endpoints
  , Noul, Choice, Score, Each, Group
  , Q (..), A (..)
    -- * Alternatives and rubric levels
  , type (::>), type (:|:), Many, Offer, Handler, Level, Interp
  , Alts (..), Single, (.|), alt, many, onMany, level
  , Alternatives, AltsOk, Match, Handles, Levels, RubricOk, Index, Selected, Ranked
    -- * Builders
  , noul, choice, score, each
    -- * Results
  , Weighed (..), Weight (..), Doubt (..), Policy (..)
  , settle, judge, explain, contenders, handle
  , grade, massAtOrAbove
    -- * The operation
  , Schema (..), PacketSchema, Model (..), jevLatest
  , request, decode, Response, answers, responseModel, usage, diagnostics
  , JevError (..), roundTrip, jev1
    -- * Internals for extension (capture replay lives outside the library)
  , Endpoint (..), Path (..), encodePath, extend, leaf, lookupAnswer
  , checkLegend, checkExpectation
  ) where

import Data.Kind (Constraint, Type)
import Data.List (nub, sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Type.Equality (type (==), type (~~))
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

-- | How a cell of endpoint @e@ reads under a mode. Questions are always the
-- leaf; answers are transparent for nesting.
type family mode :- (e :: Type) :: Type where
  Questions v :- e = Q v e
  Answers v :- Group s = s (Answers v)
  Answers v :- Each a e = [(a, Answers v :- e)]
  Answers v :- e = A v e
infixr 0 :-

-- ---------------------------------------------------------------------------
-- Endpoints
-- ---------------------------------------------------------------------------

data Noul
data Choice (alts :: Type)
data Score (p :: Type) (levels :: k)
data Each (a :: Type) (e :: Type)
data Group (s :: Type -> Type)

data family Q (v :: Type) (e :: Type)
data family A (v :: Type) (e :: Type)

-- ---------------------------------------------------------------------------
-- Alternatives and levels: one chain, three shapes
-- ---------------------------------------------------------------------------

-- | A labeled alternative with a local payload; wording is supplied with
-- the payload by 'alt'.
data (k :: Symbol) ::> (p :: Type)
-- | A chain. Alternatives are @label ::> payload@ or @Many payload@; rubric
-- levels are bare labels.
data (a :: ka) :|: (b :: kb)
-- | A runtime group of alternatives sharing a payload type; keys and
-- wording per element at the value level.
data Many (p :: Type)
infix 6 ::>
infixr 4 :|:

data Label (k :: Symbol) = Label
instance k ~ k' => IsLabel k (Label k') where fromLabel = Label

-- | Interpretations of an alternative: what an offer supplies, what a
-- handler receives, what a level carries.
data Offer (v :: Type)
data Handler (v :: Type) (r :: Type)
data Level (v :: Type) (p :: Type)

type family Interp (f :: Type) (x :: Type) :: Type where
  Interp (Offer v) (k ::> p) = (v, p)
  Interp (Offer v) (Many p) = [(Text, v, p)]
  Interp (Handler v r) (k ::> p) = p -> r
  Interp (Handler v r) (Many p) = Text -> p -> r

-- | Offers, handlers, or levels for a whole chain.
type Alts :: Type -> forall k. k -> Type
data Alts f alts where
  One :: KnownSymbol k => Interp f (k ::> p) -> Alts f (k ::> p)
  Grp :: Interp f (Many p) -> Alts f (Many p)
  Lvl :: KnownSymbol l => v -> p -> Alts (Level v p) (l :: Symbol)
  (:|) :: Alts f x -> Alts f rest -> Alts f (x :|: rest)
infixr 4 :|

-- | The left of a chain is one element; the chain associates to the
-- right, so no parentheses are needed and none are accepted.
type Single :: forall k. k -> Constraint
type family Single x where
  Single @Type (a :|: b) = TypeError ('Text "a parenthesised group stands where one alternative or level is expected; .| associates to the right, so write a .| b .| c without parentheses")
  Single x = ()

(.|) :: Single x => Alts f x -> Alts f rest -> Alts f (x :|: rest)
(.|) = (:|)
infixr 4 .|

-- | One alternative: its label, its wording for the provider, its payload
-- for the program. The alternative's type is inferred from this.
alt :: KnownSymbol k => Label k -> v -> p -> Alts (Offer v) (k ::> p)
alt _ d p = One (d, p)

-- | A runtime group: one wire key and one wording per row, and the row
-- itself is the payload the handler receives.
many :: (a -> Text) -> (a -> v) -> [a] -> Alts (Offer v) (Many a)
many key wording rows = Grp [(key x, wording x, x) | x <- rows]

onMany :: (Text -> p -> r) -> Alts (Handler v r) (Many p)
onMany = Grp

-- | One rubric level: its label, its wording for the provider, and the
-- result 'grade' returns when the score lands on it. The same three things
-- an alternative carries, in the same order.
level :: KnownSymbol l => Label l -> v -> p -> Alts (Level v p) l
level _ = Lvl

-- Handlers are written with labels; the label and the function fix the
-- alternative, so a handler list is an ordinary value with an inferred
-- type. 'handle' checks it against the alternatives in declaration order,
-- with messages that name both.
type family HandlerShape (k :: Symbol) (x :: Type) :: Constraint where
  HandlerShape k (p -> r) = ()
  HandlerShape k (a, b) = TypeError ('Text "offers are written alt #" ':<>: 'Text k ':<>: 'Text " wording payload; #" ':<>: 'Text k ':<>: 'Text " alone builds a handler")
  HandlerShape k x = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " takes a handler: a function of the payload")
type family ArgOf (x :: Type) :: Type where ArgOf (p -> r) = p
type family ResOf (x :: Type) :: Type where ResOf (p -> r) = r

-- When the alternative is already known from context, its label is checked
-- here, with the same messages 'handle' gives when it is inferred first.
type LabelFits :: Symbol -> forall ka. ka -> Constraint
type family LabelFits k alt where
  LabelFits k @Type (k ::> p) = ()
  LabelFits k @Type (Many p) = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " is written where the runtime group (Many) of this disjunction stands; use onMany")
  LabelFits k @Type (k' ::> p) = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " is written where the alternative #" ':<>: 'Text k' ':<>: 'Text " stands (handlers follow declaration order)")
  LabelFits k @Type (a :|: b) = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " stands alone where the disjunction continues; chain handlers with .| (right-associated, without parentheses)")
  LabelFits k alt = ()

instance (KnownSymbol k, HandlerShape k x, LabelFits k alt, x ~ (ArgOf x -> ResOf x), f ~ Handler v (ResOf x), alt ~~ (k ::> ArgOf x)) => IsLabel k (x -> Alts f alt) where
  fromLabel = One

-- | Handlers against alternatives, position by position.
type family Match (hs :: Type) (alts :: Type) :: Constraint where
  Match (k ::> p) (k ::> p') = p ~ p'
  Match (Many p) (Many p') = p ~ p'
  Match (h :|: hs) (a :|: as) = (Match h a, Match hs as)
  Match (k ::> p) (Many p') = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " is written where the runtime group (Many) of this disjunction stands; use onMany")
  Match (Many p) (k ::> p') = TypeError ('Text "onMany is written where the alternative #" ':<>: 'Text k ':<>: 'Text " stands (handlers follow declaration order)")
  Match (k ::> p) (k' ::> p') = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " is written where the alternative #" ':<>: 'Text k' ':<>: 'Text " stands (handlers follow declaration order)")
  Match (h :|: hs) a = TypeError ('Text "handlers continue past the end of the disjunction: " ':<>: Describe hs ':<>: 'Text " has no alternative")
  Match h (a :|: as) = TypeError ('Text "handlers stop after " ':<>: Describe h ':<>: 'Text "; " ':<>: Describe as ':<>: 'Text " still needs a handler; chain handlers with .|")
type family Describe (x :: Type) :: ErrorMessage where
  Describe (k ::> p) = 'Text "#" ':<>: 'Text k
  Describe (Many p) = 'Text "the runtime group (Many)"
  Describe (h :|: hs) = Describe h

-- | Handlers that fit a disjunction: one per alternative, in declaration
-- order, each taking that alternative's payload.
type Handles hs alts = (Alternatives alts, Match hs alts, hs ~ alts)

-- | The selected alternative, carrying the payload it was offered with. A
-- selection has no key of its own: it is consumed by 'handle', and the
-- answer record it came from carries the key for logs.
data Selected v alts where
  SelOne :: p -> Selected v (k ::> p)
  SelMany :: Text -> v -> p -> Selected v (Many p)
  SelLeft :: Selected v x -> Selected v (x :|: rest)
  SelRight :: Selected v rest -> Selected v (x :|: rest)

-- | Static labels unique, checked at compile time; counts at preparation.
type family AltsOk (alts :: Type) :: Constraint where
  AltsOk alts = UniqueLabels (AltLabels alts)
type family AltLabels (alts :: Type) :: [Symbol] where
  AltLabels (k ::> p) = '[k]
  AltLabels (Many p) = '[]
  AltLabels (x :|: rest) = AltLabels x ++ AltLabels rest
type family UniqueLabels (ls :: [Symbol]) :: Constraint where
  UniqueLabels '[] = ()
  UniqueLabels (l ': ls) = (SymbolAbsent l ls, UniqueLabels ls)
type family SymbolAbsent (l :: Symbol) (ls :: [Symbol]) :: Constraint where
  SymbolAbsent l '[] = ()
  SymbolAbsent l (l ': ls) = TypeError ('Text "Jev: duplicate label #" ':<>: 'Text l)
  SymbolAbsent l (j ': ls) = SymbolAbsent l ls

type family (++) (a :: [k]) (b :: [k]) :: [k] where
  '[] ++ b = b
  (x ': a) ++ b = x ': (a ++ b)

-- | Compile, decode, and eliminate a disjunction shape by shape.
class Alternatives (alts :: Type) where
  altWire :: JsonValue v => Text -> Alts (Offer v) alts -> Either PrepError [(Text, v)]
  altSelect :: Alts (Offer v) alts -> Text -> Maybe (Selected v alts)
  altHandle :: Alts (Handler v r) alts -> Selected v alts -> r
  altKeyOf :: Selected v alts -> Text

instance KnownSymbol k => Alternatives (k ::> p) where
  altWire key (One (d, _)) = checkDescription key (label @k) d >> Right [(label @k, d)]
  altSelect (One (_, p)) sel = if sel == label @k then Just (SelOne p) else Nothing
  altHandle (One h) (SelOne p) = h p
  altKeyOf _ = label @k

instance Alternatives (Many p) where
  altWire key (Grp es) = do
    let keys = [k | (k, _, _) <- es]
    if length keys /= length (nub keys) then Left (DuplicateKeys key [k | k <- nub keys, length (filter (== k) keys) > 1]) else Right ()
    mapM_ (\(k, d, _) -> checkDescription key k d) es
    Right [(k, d) | (k, d, _) <- es]
  altSelect (Grp es) sel =
    case [SelMany k d p | (k, d, p) <- es, k == sel] of
      e : _ -> Just e
      [] -> Nothing
  altHandle (Grp h) (SelMany k _ p) = h k p
  altKeyOf (SelMany k _ _) = k

instance (Alternatives x, Alternatives rest) => Alternatives (x :|: rest) where
  altWire key (c :| rest) = (++) <$> altWire key c <*> altWire key rest
  altSelect (c :| rest) sel = case altSelect c sel of
    Just s -> Just (SelLeft s)
    Nothing -> SelRight <$> altSelect rest sel
  altHandle (c :| rest) = \case
    SelLeft s -> altHandle c s
    SelRight s -> altHandle rest s
  altKeyOf = \case
    SelLeft s -> altKeyOf s
    SelRight s -> altKeyOf s

label :: forall k. KnownSymbol k => Text
label = T.pack (symbolVal (Proxy @k))

-- ---------------------------------------------------------------------------
-- Rubrics: a chain of bare labels
-- ---------------------------------------------------------------------------

-- | A rubric's levels in order: label, wording, result. Never empty, which
-- is what lets 'grade' fall back to the lowest level with no partial
-- function and no check.
class Levels (levels :: k) where
  levelEntries :: Alts (Level v p) levels -> NonEmpty (Text, v, p)

instance KnownSymbol l => Levels (l :: Symbol) where
  levelEntries (Lvl d p) = (label @l, d, p) NE.:| []

instance (Levels x, Levels rest) => Levels ((x :: kx) :|: (rest :: kr)) where
  levelEntries (x :| rest) = levelEntries x <> levelEntries rest

type RubricLabels :: forall k. k -> [Symbol]
type family RubricLabels levels where
  RubricLabels @Symbol l = '[l]
  RubricLabels @Type (x :|: rest) = RubricLabels x ++ RubricLabels rest
  RubricLabels x = TypeError ('Text "Jev: a rubric is a chain of bare labels, such as \"low\" :|: \"high\"; found " ':<>: 'ShowType x)

type family RubricOk (levels :: k) :: Constraint where
  RubricOk levels = UniqueLabels (RubricLabels levels)

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

-- | What the provider said about a proposition, in one field.
newtype instance A v Noul = NoulA { yes :: Double }

data instance Q v (Choice alts) = ChoiceQ (Instructions v) (Alts (Offer v) alts)

-- | What the provider chose, with everything a caller judges it by. Read
-- the fields with record dot: @a.next.key@, @a.next.margin@.
data instance A v (Choice alts) = Chosen
  { key :: Text                          -- ^ the winner's wire key
  , mass :: Double                       -- ^ the winner's probability
  , margin :: Double                     -- ^ winner minus runner-up; the mass when it stands alone
  , confidence :: Double                 -- ^ the provider's own confidence
  , masses :: [(Text, Double)]           -- ^ the full distribution, best first
  , ranked :: Ranked v alts              -- ^ opaque: what 'settle', 'handle' and 'contenders' read
  }

-- | The alternative that won, and every alternative by mass, best first.
-- Abstract: the constructor is not exported, so the only way to reach a
-- selection is 'settle', 'handle' or 'contenders', each of which takes a
-- handler per alternative. A program cannot hold one it has not written a
-- branch for.
data Ranked v alts = Ranked (Selected v alts) [(Double, Selected v alts)]

data instance Q v (Score p levels) = ScoreQ (Instructions v) (Alts (Level v p) levels)

-- | Where on the rubric the provider landed. Read with record dot:
-- @a.urgency.expectation@, @a.urgency.masses@; 'grade' picks one of the
-- results the rubric was written with.
data instance A v (Score p levels) = Scored
  { expectation :: Double        -- ^ the expected level index
  , confidence :: Double         -- ^ the provider's own confidence
  , masses :: [(Text, Double)]   -- ^ the distribution, by level label, in level order
  , results :: NonEmpty p        -- ^ every level's result, in level order
  }

newtype instance Q v (Each a e) = EachQ [(Text, a, Q v e)]
newtype instance A v (Each a e) = EachA [(Text, a, A v e)]

newtype instance Q v (Group s) = GroupQ (s (Questions v))
newtype instance A v (Group s) = GroupA (s (Answers v))

-- Internal readers: 'confidence' and 'masses' are fields of two records, so
-- the module names them by pattern rather than by an ambiguous selector.
chosenMasses :: A v (Choice alts) -> [(Text, Double)]
chosenMasses Chosen { masses = ms } = ms

chosenConfidence :: A v (Choice alts) -> Double
chosenConfidence Chosen { confidence = c } = c

scoreMasses :: A v (Score p levels) -> [(Text, Double)]
scoreMasses Scored { masses = ms } = ms

scoreConfidence :: A v (Score p levels) -> Double
scoreConfidence Scored { confidence = c } = c

-- | Answers print as their own fields. Probabilities are shown to two
-- decimals: they are a provider's judgment, not an exact quantity.
instance Show (A v Noul) where
  show a = "Noul {yes = " <> T.unpack (fmt2 (yes a)) <> "}"

instance Show (A v (Choice alts)) where
  show a@Chosen { key = k, mass = m, margin = g } =
    "Choice {key = " <> show k <> ", mass = " <> T.unpack (fmt2 m) <> ", margin = " <> T.unpack (fmt2 g)
      <> ", confidence = " <> T.unpack (fmt2 (chosenConfidence a)) <> ", masses = " <> T.unpack (showMasses (chosenMasses a)) <> "}"

instance Show (A v (Score p levels)) where
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
-- holds a question or a nested packet, and so does this, so one question per
-- row needs no packet around it and several is the same call with a packet.
each :: ToQ x => (a -> Text) -> (a -> x) -> [a] -> Q (CellJson x) (Each a (CellKind x))
each key q rows = EachQ [(key r, r, toQ (q r)) | r <- rows]

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
class Weighed e where
  weigh :: A v e -> Weight

instance Weighed (Choice alts) where
  weigh a@Chosen { key = k, mass = m } = Weight
    { winner = k
    , winnerMass = m
    , runnerUp = case [r | r@(k', _) <- chosenMasses a, k' /= k] of { r : _ -> Just r; [] -> Nothing }
    , winnerConfidence = Just (chosenConfidence a)
    }

instance Weighed Noul where
  weigh (NoulA y)
    | y >= 0.5 = Weight "yes" y (Just ("no", 1 - y)) Nothing
    | otherwise = Weight "no" (1 - y) (Just ("yes", y)) Nothing

data Doubt
  = NearTie (Text, Double) (Text, Double)  -- ^ winner and runner-up too close
  | Underweight Double                     -- ^ the winner's mass is below the floor
  | Unconfident Double                     -- ^ the provider's confidence is below the floor
  deriving (Show, Eq)

data Policy = Policy
  { minMass :: Double
  , minMargin :: Double
  , minConfidence :: Double
  } deriving (Show, Eq)

doubt :: Weighed e => Policy -> A v e -> Maybe Doubt
doubt policy a =
  let w = weigh a
  in case winnerConfidence w of
    Just c | c < minConfidence policy -> Just (Unconfident c)
    _ | winnerMass w < minMass policy -> Just (Underweight (winnerMass w))
    _ | Just (k2, m2) <- runnerUp w, winnerMass w - m2 < minMargin policy -> Just (NearTie (winner w, winnerMass w) (k2, m2))
    _ -> Nothing

-- | The winner under a policy, or structured doubt. There is no way to get
-- a result without a handler for every alternative, so a confident answer
-- that means "no" or "missing" runs its own handler and never reads as a
-- pass.
settle :: forall alts hs v r. Handles hs alts => Policy -> A v (Choice alts) -> Alts (Handler v r) hs -> Either Doubt r
settle policy a hs = maybe (Right (handle a hs)) Left (doubt policy a)

-- | A proposition under a policy: yes, no, or structured doubt when the
-- provider was not clear either way.
judge :: Policy -> A v Noul -> Either Doubt Bool
judge policy a = maybe (Right (yes a >= 0.5)) Left (doubt policy a)

-- | One line saying why the policy settled or doubted the answer, with the
-- numbers behind it. Two-decimal formatting. This is the line a log or a
-- planner reads.
explain :: Weighed e => Policy -> A v e -> Text
explain policy a =
  let w = weigh a
      margin = maybe (winnerMass w) (\(_, m2) -> winnerMass w - m2) (runnerUp w)
      items = [("confidence" :: Text, c, minConfidence policy) | Just c <- [winnerConfidence w]]
           ++ [("mass", winnerMass w, minMass policy), ("margin", margin, minMargin policy)]
  in case doubt policy a of
    Nothing -> "settled on " <> winner w <> ": " <> T.intercalate ", " [n <> " " <> fmt2 v <> " \8805 " <> fmt2 t | (n, v, t) <- items]
    Just d ->
      let (ctor, failedName, failedValue, floorValue) = case d of
            Unconfident c -> ("Unconfident", "confidence" :: Text, c, minConfidence policy)
            Underweight m -> ("Underweight", "mass", m, minMass policy)
            NearTie (_, m) (_, m2) -> ("NearTie", "margin", m - m2, minMargin policy)
          floorLine = failedName <> " " <> fmt2 failedValue <> " < " <> fmt2 floorValue <> " by " <> fmt2 (floorValue - failedValue)
          rest = [n <> " " <> fmt2 v | (n, v, _) <- items, n /= failedName]
      in "doubted " <> winner w <> " (" <> ctor <> "): " <> floorLine <> "; " <> T.intercalate ", " rest

-- | The winner against a handler per alternative in declaration order, with
-- no policy: for when the program follows whatever came back. A missing,
-- extra, or misordered handler is a type error naming the labels.
handle :: forall alts hs v r. Handles hs alts => A v (Choice alts) -> Alts (Handler v r) hs -> r
handle a hs = case ranked a of Ranked sel _ -> altHandle hs sel

-- | Every alternative at or above a mass floor, best first, each already
-- through the same handlers. The one way to act on a runner-up.
contenders :: forall alts hs v r. Handles hs alts => Double -> A v (Choice alts) -> Alts (Handler v r) hs -> [(Double, r)]
contenders floor' a hs = case ranked a of Ranked _ rs -> [(m, altHandle hs s) | (m, s) <- rs, m >= floor']

-- | Run the result for the level the score landed on. Levels run lowest to
-- highest, so this walks from the highest down and takes the first whose
-- mass at or above it clears the floor, and the lowest level when none
-- does. At a floor of 0.5 that is the median level.
--
-- There is always an answer: an ordinal scale has a median even when the
-- distribution is flat, which is why this gives no 'Doubt'. Every level
-- carries its result from the moment it is written, so there is no list to
-- check and no label string to dispatch on.
grade :: Double -> A v (Score p levels) -> p
grade floor' a =
  let rs = results a
      -- Mass at or above each level, aligned with the levels past the lowest.
      -- It falls as the level rises, so the last one to clear the floor is the
      -- highest that clears it, and the lowest level stands when none does.
      atOrAbove = drop 1 (scanr (+) 0 (map snd (scoreMasses a)))
  in foldl (\taken (r, m) -> if m >= floor' then r else taken) (NE.head rs) (zip (NE.tail rs) atOrAbove)

-- | Mass at or beyond a level, by label.
massAtOrAbove :: forall l levels p v. KnownNat (Index l levels) => Label l -> A v (Score p levels) -> Double
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
    Right (NoulA x)
  unwrapA = id
  previewA a = jObject [("yes", jNumber (yes a))]

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
    Right Chosen
      { key = pickedKey
      , mass = pickedMass
      , margin = pickedMargin
      , confidence = conf
      , masses = best
      , ranked = Ranked picked everyAlt
      }
  unwrapA = id
  previewA a@Chosen { key = k, mass = m, margin = g } = jObject
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
    Right Scored { expectation = e, confidence = conf, masses = byIndex, results = fmap (\(_, _, r) -> r) entries }
  unwrapA = id
  previewA a@Scored { expectation = e } = jObject
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

-- ---------------------------------------------------------------------------
-- Packets
-- ---------------------------------------------------------------------------

data (k :: Symbol) ::= (e :: Type)

-- | A cell: a labeled question under 'Questions', a decoded answer under
-- 'Answers'.
data Cell (k :: Symbol) (e :: Type) mode where
  (:=) :: ToQ x => Label k -> x -> Cell k (CellKind x) (Questions (CellJson x))
  Answered :: Endpoint v e => A v e -> Cell k e (Answers v)
infix 6 :=

-- | What a cell may hold: a question, or a nested packet.
type family CellKind (x :: Type) :: Type where
  CellKind (Q v e) = e
  CellKind (Packet fs (Questions v)) = Group (Packet fs)
  CellKind x = TypeError ('Text "Jev: a cell holds a question or a nested packet; this is " ':<>: 'ShowType x)

type family CellJson (x :: Type) :: Type where
  CellJson (Q v e) = v
  CellJson (Packet fs (Questions v)) = v

class ToQ (x :: Type) where
  toQ :: x -> Q (CellJson x) (CellKind x)
instance ToQ (Q v e) where toQ = id
instance ToQ (Packet fs (Questions v)) where toQ = GroupQ

data Packet (fs :: [Type]) mode where
  Nil :: Packet '[] mode
  (:&) :: Cell k e mode -> Packet fs mode -> Packet (k ::= e ': fs) mode
infixr 5 :&

type family Labels (fs :: [Type]) :: ErrorMessage where
  Labels '[] = 'Text "nothing"
  Labels '[k ::= e] = 'Text "#" ':<>: 'Text k
  Labels (k ::= e ': fs) = 'Text "#" ':<>: 'Text k ':<>: 'Text ", " ':<>: Labels fs

type family Unique (fs :: [Type]) :: Constraint where
  Unique '[] = ()
  Unique (k ::= e ': fs) = (Absent k fs, Unique fs)
type family Absent (k :: Symbol) (fs :: [Type]) :: Constraint where
  Absent k '[] = ()
  Absent k (k ::= e ': fs) = TypeError ('Text "Jev: duplicate packet label #" ':<>: 'Text k)
  Absent k (j ::= e ': fs) = Absent k fs

-- | Field access on an answers packet, carrying the full label list for
-- the error message.
class Get (k :: Symbol) (fs :: [Type]) (all :: [Type]) (e :: Type) | k fs all -> e where
  get :: Packet fs (Answers v) -> Answers v :- e
instance (TypeError ('Text "Jev: this packet has no #" ':<>: 'Text k ':<>: 'Text "; it has " ':<>: Labels all), e ~ ())
  => Get k '[] all e where
  get = undefined
instance (flag ~ (k == j), Get' flag k (j ::= e' ': fs) all e) => Get k (j ::= e' ': fs) all e where
  get = get' @flag @k @(j ::= e' ': fs) @all
class Get' (flag :: Bool) (k :: Symbol) (fs :: [Type]) (all :: [Type]) (e :: Type) | flag k fs all -> e where
  get' :: Packet fs (Answers v) -> Answers v :- e
instance Get' 'True k (k ::= e ': fs) all e where
  get' (Answered a :& _) = unwrapA a
instance Get k fs all e => Get' 'False k (j ::= e' ': fs) all e where
  get' (_ :& p) = get @k @fs @all p

instance (Get k fs fs e, r ~ (Answers v :- e)) => HasField k (Packet fs (Answers v)) r where
  getField = get @k @fs @fs

-- | The packet traversal, by induction over the labels.
class JsonValue v => PacketSchema v (fs :: [Type]) where
  packetCompile :: Path -> Packet fs (Questions v) -> Either PrepError [(Text, WireQuestion v)]
  packetDecode :: Path -> Packet fs (Questions v) -> [(Text, v)] -> Either DecodeError (Packet fs (Answers v))
  packetPreview :: Packet fs (Answers v) -> [(Text, v)]

instance JsonValue v => PacketSchema v '[] where
  packetCompile _ Nil = Right []
  packetDecode _ Nil _ = Right Nil
  packetPreview Nil = []

instance (KnownSymbol k, Endpoint v e, PacketSchema v fs) => PacketSchema v (k ::= e ': fs) where
  packetCompile p (_ := x :& rest) = (++) <$> compileQ (extend p (label @k)) (toQ x) <*> packetCompile p rest
  packetDecode p (_ := x :& rest) ws = (:&) <$> (Answered <$> decodeA (extend p (label @k)) (toQ x) ws) <*> packetDecode p rest ws
  packetPreview (Answered a :& rest) = (label @k, previewA a) : packetPreview rest

instance (Show v, PacketSchema v fs) => Show (Packet fs (Answers v)) where
  show p = show (jObject (packetPreview p))

-- ---------------------------------------------------------------------------
-- Schemas
-- ---------------------------------------------------------------------------

class JsonValue v => Schema v (s :: Type -> Type) where
  compileSchema :: Path -> s (Questions v) -> Either PrepError [(Text, WireQuestion v)]
  decodeSchema :: Path -> s (Questions v) -> [(Text, v)] -> Either DecodeError (s (Answers v))
  previewSchema :: s (Answers v) -> v

instance (JsonValue v, Unique fs, PacketSchema v fs) => Schema v (Packet fs) where
  compileSchema = packetCompile
  decodeSchema = packetDecode
  previewSchema = jObject . packetPreview

-- ---------------------------------------------------------------------------
-- The operation: request, decode
-- ---------------------------------------------------------------------------

newtype Model = Model Text deriving (Eq, Show)
instance IsString Model where fromString = Model . T.pack

jevLatest :: Model
jevLatest = Model "jev-latest"

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
request :: Schema v s => Model -> State v -> s (Questions v) -> Either JevError v
request (Model m) st q = either (Left . Prepare) Right $ do
  checkState st
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
  => (v -> m (Either Text v)) -> Model -> State v -> s (Questions v)
  -> m (Either JevError (Response v s))
roundTrip transport model st q = case request model st q of
  Left e -> pure (Left e)
  Right body -> transport body >>= \case
    Left t -> pure (Left (Transport t))
    Right resp -> pure (decode q resp)

-- | The tiny use: one question, one answer.
jev1
  :: (Monad m, Endpoint v e)
  => (v -> m (Either Text v)) -> Model -> State v -> Q v e
  -> m (Either JevError (Answers v :- e))
jev1 transport model st q = fmap (fmap (\r -> case answers r of Answered a :& Nil -> unwrapA a)) (roundTrip transport model st ((Label :: Label "value") := q :& Nil))
