{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | The agent-facing form: an anonymous, type-indexed packet of questions,
-- alternatives as a type-level disjunction with descriptions in the type,
-- rubrics as type-level label lists, pools as packet cells. Polymorphic
-- over the JSON value through "Jev.Core.Json"; "Jev.Operators" fixes it.
--
-- The packet's type is inferred from the questions written; stable
-- structural facts (labels, declarations, shapes, bounds) are checked at
-- compile time with messages in the author's vocabulary; runtime evidence,
-- descriptions of runtime candidates, and pool correspondence are checked
-- at 'prepare'.
module Jev.Core.Schema
  ( -- * Modes
    Questions, Answers, type (:-)
    -- * Packets
  , type (::=), Label (..), Cell (..), CellOk, Packet (..), type (++), (++.)
  , Unique, Get, Lookup
    -- * Endpoints and leaves
  , Noul, Choice, Score, Scale, Each, Group, Dynamic, Raw, PoolDecl
  , Q (..), A (..), SomeQ (..), SomeA (..)
    -- * Alternatives
  , type (::>), type (:?), type (:|:), Many
  , Alts (..), AltCell (..), CellOf (..), CheckLabel, Single, (.|), Offer, Handler, Interp, Element (..), Described (..)
  , Alternatives, AltsOk, Selected (..), many, manyFrom, onMany, describe
  , Sum, sumOffer, sumOfferKeyed, ConName (..)
    -- * Rubrics
  , Rubric (..), Lvl, Index, RubricOk
    -- * Builders
  , noul, noulOn, noulAbout, noulWith, choice, choiceWith, score, scoreWith, scale
  , each, group, dynamic, rawUnchecked, pool, refs, eachIn, askAbout
  , Ref (..), Pool (..), PoolUse, Levels, levelsOf, Premised (..)
    -- * Results
  , selectedKey, handle, caseOf, accept, acceptOr, Doubt (..), Policy (..), lenient
  , massAtOrAbove, levelOf, yesAbove, noBelow, unsure
    -- * Schemas and the operation
  , Schema (..), PacketSchema, Exact (..), ExactLeaf, exact, exactAnswers, previewAnswer
  , Model (..), jevLatest, Compiled (..)
  , Prepared, preparedQuestions, preparedModel, preparedState, preparedWire, preparedPools
  , prepare, requestValue, decodeResponse, Response (..), JevError (..), roundTrip, jev1
  , Endpoint (..)
  ) where

import Data.Char (isUpper, toLower)
import Data.Kind (Constraint, Type)
import Data.List (nub, sortOn)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Type.Equality (type (==))
import GHC.Generics
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits
import Jev.Core.Contract
import Jev.Core.Json

-- ---------------------------------------------------------------------------
-- Modes and the interpretation of a cell
-- ---------------------------------------------------------------------------

data Questions (v :: Type)
data Answers (v :: Type)

-- | How a cell of endpoint @e@ reads under a mode. Questions are always the
-- leaf; answers are transparent for nesting and pools.
type family mode :- (e :: Type) :: Type where
  Questions v :- e = Q v e
  Answers v :- Group s = s (Answers v)
  Answers v :- Each s = [(Text, s (Answers v))]
  Answers v :- PoolDecl n a = Pool v n a
  Answers v :- e = A v e
infixr 0 :-

-- ---------------------------------------------------------------------------
-- Endpoints
-- ---------------------------------------------------------------------------

data Noul
data Choice (alts :: Type)
data Score (levels :: [Type])
data Scale
data Each (s :: Type -> Type)
data Group (s :: Type -> Type)
data Dynamic
data Raw
data PoolDecl (name :: Symbol) (a :: Type)

data family Q (v :: Type) (e :: Type)
data family A (v :: Type) (e :: Type)

-- ---------------------------------------------------------------------------
-- Alternatives: a type-level disjunction
-- ---------------------------------------------------------------------------

-- | A labeled alternative with a local payload. Its description is either in
-- the type (@alt :? "text"@) or supplied with the payload at the value level.
data (k :: Symbol) ::> (p :: Type)
-- | A description in the type. Poly-kinded so rubric labels use it too.
data (alt :: k) :? (d :: Symbol)
-- | Disjunction.
data a :|: b
-- | A runtime group of alternatives sharing a payload type; keys and
-- descriptions per element at the value level.
data Many (p :: Type)
infix 6 ::>
infixl 5 :?
infixr 4 :|:

data Label (k :: Symbol) = Label
instance k ~ k' => IsLabel k (Label k') where fromLabel = Label

-- | Interpretations of an alternative: what an offer supplies, what a
-- handler receives.
data Offer (v :: Type)
data Handler (v :: Type) (r :: Type)

data Element v p = Element { elementKey :: Text, elementDescription :: v, elementPayload :: p }

type family Interp (f :: Type) (x :: Type) :: Type where
  Interp (Offer v) (k ::> p) = (v, p)
  Interp (Offer v) ((k ::> p) :? d) = Described v p
  Interp (Offer v) (Many p) = ManyOffer v p
  Interp (Handler v r) (k ::> p) = p -> r
  Interp (Handler v r) ((k ::> p) :? d) = p -> r
  Interp (Handler v r) (Many p) = Element v p -> r
  Interp (Offer v) (Sum t) = SumOffer v t
  Interp (Handler v r) (Sum t) = t -> r

data ManyOffer v p = ManyOffer [(Text, v, p)] (Maybe (PoolUse v))

-- | A payload offered under a type-level description, optionally
-- overridden at the value level with 'describe'.
data Described v p = Described (Maybe v) p

newtype AltCell f x = AltCell (Interp f x)

-- | What a label takes to build one alternative: an offer supplies the
-- payload (and, for a bare alternative, its description); a handler is a
-- function of the payload. The alternative's shape determines it.
class CellOf f alt x | f alt -> x where
  cellOf :: x -> AltCell f alt
instance x ~ (v, p) => CellOf (Offer v) (k ::> p) x where cellOf = AltCell
instance x ~ p => CellOf (Offer v) ((k ::> p) :? d) x where cellOf p = AltCell (Described Nothing p)
instance x ~ (p -> r) => CellOf (Handler v r) (k ::> p) x where cellOf = AltCell
instance x ~ (p -> r) => CellOf (Handler v r) ((k ::> p) :? d) x where cellOf = AltCell

-- | The label written must be the label of the alternative in that
-- position; the messages name both.
type family CheckLabel (k :: Symbol) (alt :: Type) :: Constraint where
  CheckLabel k (k ::> p) = ()
  CheckLabel k ((k ::> p) :? d) = ()
  CheckLabel k (Many p) = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " is written where the runtime group (Many) of this disjunction stands; use many/manyFrom for offers and onMany for handlers")
  CheckLabel k (Sum t) = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " is written where an ordinary sum (Sum) stands; use sumOffer for offers and a function for handlers")
  CheckLabel k (k' ::> p) = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " is written where the alternative #" ':<>: 'Text k' ':<>: 'Text " stands (alternatives are listed in declaration order)")
  CheckLabel k ((k' ::> p) :? d) = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " is written where the alternative #" ':<>: 'Text k' ':<>: 'Text " stands (alternatives are listed in declaration order)")
  CheckLabel k (a :|: b) = TypeError ('Text "#" ':<>: 'Text k ':<>: 'Text " stands alone where the disjunction continues; chain alternatives with .| (right-associated, without parentheses)")

instance (CheckLabel k alt, CellOf f alt x, Shape alt) => IsLabel k (x -> Alts f alt) where
  fromLabel = single . cellOf

-- | Override a type-level description with a value: structured, null, or
-- runtime wording, keeping the alternative's typed payload.
describe :: v -> Alts (Offer v) ((k ::> p) :? d) -> Alts (Offer v) ((k ::> p) :? d)
describe d (OneDescribed (AltCell (Described _ p))) = OneDescribed (AltCell (Described (Just d) p))

many :: [(Text, v, p)] -> Alts (Offer v) (Many p)
many es = OneMany (AltCell (ManyOffer es Nothing))

onMany :: (Element v p -> r) -> Alts (Handler v r) (Many p)
onMany = OneMany . AltCell

-- | Offers or handlers for a whole disjunction. Singletons are built per
-- shape so a @:|:@ index is provably a cons.
data Alts f alts where
  OneBare :: AltCell f (k ::> p) -> Alts f (k ::> p)
  OneDescribed :: AltCell f ((k ::> p) :? d) -> Alts f ((k ::> p) :? d)
  OneMany :: AltCell f (Many p) -> Alts f (Many p)
  OneSum :: AltCell f (Sum t) -> Alts f (Sum t)
  (:|) :: Alts f x -> Alts f rest -> Alts f (x :|: rest)
infixr 4 :|

class Shape (x :: Type) where
  single :: AltCell f x -> Alts f x
instance Shape (k ::> p) where single = OneBare
instance Shape ((k ::> p) :? d) where single = OneDescribed
instance Shape (Many p) where single = OneMany

-- | The left of a chain is one alternative; the chain associates to the
-- right, so no parentheses are needed and none are accepted.
type family Single (x :: Type) :: Constraint where
  Single (a :|: b) = TypeError ('Text "a parenthesised group of alternatives stands where one alternative is expected; .| associates to the right, so write a .| b .| c without parentheses")
  Single x = ()

(.|) :: Single x => Alts f x -> Alts f rest -> Alts f (x :|: rest)
(.|) = (:|)
infixr 4 .|

-- | The selected alternative, carrying the payload it was offered with.
data Selected v alts where
  SelBare :: p -> Selected v (k ::> p)
  SelDescribed :: p -> Selected v ((k ::> p) :? d)
  SelMany :: Element v p -> Selected v (Many p)
  SelSum :: Text -> t -> Selected v (Sum t)
  SelLeft :: Selected v x -> Selected v (x :|: rest)
  SelRight :: Selected v rest -> Selected v (x :|: rest)

-- | Static labels unique and at most 255, checked at compile time.
type family AltsOk (alts :: Type) :: Constraint where
  AltsOk alts = (UniqueAltLabels (AltLabels alts), AltCountOk (Length (AltLabels alts)))
type family AltLabels (alts :: Type) :: [Symbol] where
  AltLabels (k ::> p) = '[k]
  AltLabels ((k ::> p) :? d) = '[k]
  AltLabels (Many p) = '[]
  AltLabels (Sum t) = '[]
  AltLabels (x :|: rest) = AltLabels x ++ AltLabels rest
type family UniqueAltLabels (ls :: [Symbol]) :: Constraint where
  UniqueAltLabels '[] = ()
  UniqueAltLabels (l ': ls) = (SymbolAbsent l ls, UniqueAltLabels ls)
type family SymbolAbsent (l :: Symbol) (ls :: [Symbol]) :: Constraint where
  SymbolAbsent l '[] = ()
  SymbolAbsent l (l ': ls) = TypeError ('Text "Jev: duplicate alternative #" ':<>: 'Text l)
  SymbolAbsent l (j ': ls) = SymbolAbsent l ls
type family AltCountOk (n :: Nat) :: Constraint where
  AltCountOk n = OkIf (n <=? 255) ('Text "Jev: " ':<>: 'ShowType n ':<>: 'Text " static alternatives; Jev permits at most 255")

-- | Compile, decode, and eliminate a disjunction shape by shape.
class Alternatives (alts :: Type) where
  altWire :: JsonValue v => Text -> Alts (Offer v) alts -> Either PrepError [(Text, v)]
  altUses :: Alts (Offer v) alts -> [PoolUse v]
  altSelect :: Alts (Offer v) alts -> Text -> Maybe (Selected v alts)
  altHandle :: Alts (Handler v r) alts -> Selected v alts -> r
  altKeyOf :: Selected v alts -> Text

instance KnownSymbol k => Alternatives (k ::> p) where
  altWire key (OneBare (AltCell (d, _))) = checkDescription key (label @k) d >> Right [(label @k, d)]
  altUses _ = []
  altSelect (OneBare (AltCell (_, p))) sel = if sel == label @k then Just (SelBare p) else Nothing
  altHandle (OneBare (AltCell h)) (SelBare p) = h p
  altKeyOf _ = label @k

instance (KnownSymbol k, KnownSymbol d) => Alternatives ((k ::> p) :? d) where
  altWire key (OneDescribed (AltCell (Described override _))) = case override of
    Nothing -> Right [(label @k, jString (label @d))]
    Just d -> checkDescription key (label @k) d >> Right [(label @k, d)]
  altUses _ = []
  altSelect (OneDescribed (AltCell (Described _ p))) sel = if sel == label @k then Just (SelDescribed p) else Nothing
  altHandle (OneDescribed (AltCell h)) (SelDescribed p) = h p
  altKeyOf _ = label @k

instance Alternatives (Many p) where
  altWire key (OneMany (AltCell (ManyOffer es pooled'))) = do
    let keys = [k | (k, _, _) <- es]
    if length keys /= length (nub keys) then Left (DuplicateKeys key [k | k <- nub keys, length (filter (== k) keys) > 1]) else Right ()
    case pooled' of
      Nothing -> mapM_ (\(k, d, _) -> checkDescription key k d) es >> Right [(k, d) | (k, d, _) <- es]
      Just _ -> Right [(k, jNull) | (k, _, _) <- es]
  altUses (OneMany (AltCell (ManyOffer _ u))) = maybe [] pure u
  altSelect (OneMany (AltCell (ManyOffer es _))) sel =
    case [Element k d p | (k, d, p) <- es, k == sel] of
      e : _ -> Just (SelMany e)
      [] -> Nothing
  altHandle (OneMany (AltCell h)) (SelMany e) = h e
  altKeyOf (SelMany e) = elementKey e

instance (Alternatives x, Alternatives rest) => Alternatives (x :|: rest) where
  altWire key (c :| rest) = (++) <$> altWire key c <*> altWire key rest
  altUses (c :| rest) = altUses c ++ altUses rest
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

-- | Ordinary Haskell sums as alternatives: the seam the declared-record
-- front lowers through. Constructor names are wire keys (snake-cased,
-- overridable per offer); the answer is the offered value; elimination is
-- @case@. The same 'Choice' endpoint, compiler, and validator serve both
-- forms.
data Sum (t :: Type)
instance Shape (Sum t) where single = OneSum

data SumOffer v t = SumOffer [(Maybe Text, v, t)]   -- optional key override, description, value


-- | Offer values of a sum with descriptions; keys default to the
-- constructor name, overridable for repeated constructors.
sumOffer :: [(v, t)] -> Alts (Offer v) (Sum t)
sumOffer vs = OneSum (AltCell (SumOffer [(Nothing, d, t) | (d, t) <- vs]))

sumOfferKeyed :: [(Text, v, t)] -> Alts (Offer v) (Sum t)
sumOfferKeyed vs = OneSum (AltCell (SumOffer [(Just k, d, t) | (k, d, t) <- vs]))

instance ConName t => Alternatives (Sum t) where
  altWire key (OneSum (AltCell (SumOffer es))) = do
    let keys = [maybe (conKey t) id k | (k, _, t) <- es]
    if length keys /= length (nub keys) then Left (DuplicateKeys key [k | k <- nub keys, length (filter (== k) keys) > 1]) else Right ()
    mapM_ (\(k, d) -> checkDescription key k d) (zip keys [d | (_, d, _) <- es])
    Right (zip keys [d | (_, d, _) <- es])
  altUses _ = []
  altSelect (OneSum (AltCell (SumOffer es))) sel =
    case [t | (k, _, t) <- es, maybe (conKey t) id k == sel] of
      t : _ -> Just (SelSum sel t)
      [] -> Nothing
  altHandle (OneSum (AltCell h)) (SelSum _ t) = h t
  altKeyOf (SelSum k _) = k

-- | The snake-cased constructor name of a value, via Generic.
class ConName t where
  conKey :: t -> Text
  default conKey :: (Generic t, GConName (Rep t)) => t -> Text
  conKey = toSnakeCase . gConName . from

class GConName f where
  gConName :: f x -> String
instance GConName f => GConName (M1 D d f) where gConName (M1 x) = gConName x
instance (GConName f, GConName g) => GConName (f :+: g) where
  gConName (L1 x) = gConName x
  gConName (R1 x) = gConName x
instance Constructor c => GConName (M1 C c f) where gConName m = conName m

toSnakeCase :: String -> Text
toSnakeCase = T.pack . go
  where
    go [] = []
    go (c : cs) = toLower c : rest cs
    rest [] = []
    rest (c : cs)
      | isUpper c = '_' : toLower c : rest cs
      | otherwise = c : rest cs

-- ---------------------------------------------------------------------------
-- Rubrics: type-level label lists, described or bare
-- ---------------------------------------------------------------------------

-- | A bare rubric level, described at the value level with 'scoreWith'.
data Lvl (l :: Symbol)

-- | A rubric is a list of @Lvl l@ or @l :? "description"@ entries.
class Rubric (levels :: [Type]) where
  rubricLabels :: [Text]
  rubricDescriptions :: JsonValue v => Maybe [v]   -- ^ Nothing when any level is bare

instance Rubric '[] where
  rubricLabels = []
  rubricDescriptions = Just []
instance (KnownSymbol l, Rubric ls) => Rubric (Lvl l ': ls) where
  rubricLabels = label @l : rubricLabels @ls
  rubricDescriptions = Nothing
instance (KnownSymbol l, KnownSymbol d, Rubric ls) => Rubric (((l :: Symbol) :? d) ': ls) where
  rubricLabels = label @l : rubricLabels @ls
  rubricDescriptions = (jString (label @d) :) <$> rubricDescriptions @ls

type family LabelOf (x :: Type) :: Symbol where
  LabelOf (Lvl l) = l
  LabelOf ((l :: Symbol) :? d) = l

type family Length (xs :: [k]) :: Nat where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

type family Index (l :: Symbol) (levels :: [Type]) :: Nat where
  Index l '[] = TypeError ('Text "Jev: no level #" ':<>: 'Text l ':<>: 'Text " in this rubric")
  Index l (x ': xs) = IndexIf (l == LabelOf x) l xs
type family IndexIf (hit :: Bool) (l :: Symbol) (rest :: [Type]) :: Nat where
  IndexIf 'True l rest = 0
  IndexIf 'False l rest = 1 + Index l rest

type family RubricOk (levels :: [Type]) :: Constraint where
  RubricOk levels = (CountOk (Length levels), UniqueLabels levels)
type family CountOk (n :: Nat) :: Constraint where
  CountOk 0 = TypeError ('Text "Jev: a rubric needs at least one level")
  CountOk n = OkIf (n <=? 10) ('Text "Jev: a rubric declares " ':<>: 'ShowType n ':<>: 'Text " levels; Jev permits 1 to 10")
type family UniqueLabels (levels :: [Type]) :: Constraint where
  UniqueLabels '[] = ()
  UniqueLabels (x ': xs) = (LabelAbsent (LabelOf x) xs, UniqueLabels xs)
type family LabelAbsent (l :: Symbol) (levels :: [Type]) :: Constraint where
  LabelAbsent l '[] = ()
  LabelAbsent l (x ': xs) = LabelAbsentIf (l == LabelOf x) l xs
type family LabelAbsentIf (hit :: Bool) (l :: Symbol) (rest :: [Type]) :: Constraint where
  LabelAbsentIf 'True l rest = TypeError ('Text "Jev: duplicate rubric level #" ':<>: 'Text l)
  LabelAbsentIf 'False l rest = LabelAbsent l rest
type family OkIf (ok :: Bool) (msg :: ErrorMessage) :: Constraint where
  OkIf 'True msg = ()
  OkIf 'False msg = TypeError msg

-- ---------------------------------------------------------------------------
-- Leaves
-- ---------------------------------------------------------------------------

type PoolUse v = (Text, v)   -- pool name, serialized {key: description}

data instance Q v Noul = NoulQ (Instructions v) (Presence (Maybe (Criteria v))) [PoolUse v]
newtype instance A v Noul = NoulA { probabilityYes :: Double }

data instance Q v (Choice alts) = ChoiceQ (Instructions v) (Alts (Offer v) alts)
data instance A v (Choice alts) = Chosen
  { chosen :: Selected v alts
  , ranked :: [(Double, Selected v alts)]
  , confidence :: Double
  , alternatives :: [(Text, Double)]          -- wire key and mass, what the provider judged
  }

data instance Q v (Score levels) = ScoreQ (Instructions v) (Maybe [(Text, v)])   -- runtime descriptions for bare labels
data instance A v (Score levels) = Scored
  { expectation :: Double
  , masses :: [(Text, Double)]
  , scoreConfidence :: Double
  , legend :: [(Text, v)]     -- ^ the returned legend, keyed by rubric label
  }

data instance Q v Scale = ScaleQ (Instructions v) (Levels v)
data instance A v Scale = Scaled
  { scaleExpectation :: Double
  , scaleMasses :: [(v, Double)]
  , scaleConfidence :: Double
  }

newtype instance Q v (Each s) = EachQ [(Text, s (Questions v))]
newtype instance A v (Each s) = EachA [(Text, s (Answers v))]

newtype instance Q v (Group s) = GroupQ (s (Questions v))
newtype instance A v (Group s) = GroupA (s (Answers v))

newtype instance Q v Dynamic = DynamicQ [(Text, SomeQ v)]
newtype instance A v Dynamic = DynamicA [(Text, SomeA v)]

newtype instance Q v Raw = RawQ v
newtype instance A v Raw = RawA v

newtype instance Q v (PoolDecl n a) = PoolQ [(Text, v, a)]
newtype instance A v (PoolDecl n a) = PoolA (Pool v n a)

data SomeQ v = forall e. Endpoint v e => SomeQ (Q v e)
data SomeA v = forall e. Endpoint v e => SomeA (Q v e) (A v e)

-- | A declared pool after the fact: key to description and payload.
data Pool v (n :: Symbol) a = Pool { poolEntries :: [(Text, v, a)] }

-- | A reference into a declared pool; constructor hidden.
data Ref v (n :: Symbol) a = Ref
  { refKey :: Text
  , refDescription :: v
  , refPayload :: a
  , refUse :: PoolUse v
  }

newtype Levels v = Levels [v]

levelsOf :: [v] -> Levels v
levelsOf = Levels

-- ---------------------------------------------------------------------------
-- Builders (all total; shapes are checked at 'prepare')
-- ---------------------------------------------------------------------------

noul :: JsonValue v => Text -> Q v Noul
noul t = NoulQ (question t) Omitted []

noulOn :: Instructions v -> Presence (Maybe (Criteria v)) -> Q v Noul
noulOn i c = NoulQ i c []

noulAbout :: JsonValue v => Text -> v -> Q v Noul
noulAbout t v = NoulQ (Structured [("question", jString t), ("about", v)]) Omitted []

noulWith :: Instructions v -> Presence (Maybe (Criteria v)) -> Q v Noul
noulWith = noulOn

choice :: forall alts v. (JsonValue v, AltsOk alts) => Text -> Alts (Offer v) alts -> Q v (Choice alts)
choice t = ChoiceQ (question t)

choiceWith :: forall alts v. AltsOk alts => Instructions v -> Alts (Offer v) alts -> Q v (Choice alts)
choiceWith = ChoiceQ

-- | A described rubric: no values needed.
score :: forall levels v. JsonValue v => Text -> Q v (Score levels)
score t = ScoreQ (question t) Nothing

-- | A bare-label rubric with runtime descriptions, checked against the
-- labels in order at 'prepare'.
scoreWith :: forall levels v. Instructions v -> [(Text, v)] -> Q v (Score levels)
scoreWith i ds = ScoreQ i (Just ds)

scale :: Instructions v -> Levels v -> Q v Scale
scale = ScaleQ

each :: [(Text, s (Questions v))] -> Q v (Each s)
each = EachQ

group :: s (Questions v) -> Q v (Group s)
group = GroupQ

dynamic :: [(Text, SomeQ v)] -> Q v Dynamic
dynamic = DynamicQ

-- | UNCHECKED, outside the validity guarantee: any value is sent as the
-- question object and the answer is the original parsed JSON.
rawUnchecked :: v -> Q v Raw
rawUnchecked = RawQ

-- | A pool declaration, named at its binding; the cell it is placed in
-- must carry the same label.
pool :: Label n -> [(Text, v, a)] -> Q v (PoolDecl n a)
pool _ = PoolQ

poolUse :: forall n v a. (JsonValue v, KnownSymbol n) => Q v (PoolDecl n a) -> PoolUse v
poolUse (PoolQ es) = (label @n, jObject [(k, d) | (k, d, _) <- es])

refs :: forall n v a. (JsonValue v, KnownSymbol n) => Q v (PoolDecl n a) -> [Ref v n a]
refs p@(PoolQ es) = [Ref k d a (poolUse p) | (k, d, a) <- es]

-- | Runtime alternatives drawn from a pool: null descriptions on the wire,
-- the pool's descriptions retained locally.
manyFrom :: forall n v a. (JsonValue v, KnownSymbol n) => Q v (PoolDecl n a) -> Alts (Offer v) (Many a)
manyFrom p@(PoolQ es) = OneMany (AltCell (ManyOffer es (Just (poolUse p))))

eachIn :: forall n v a s. (JsonValue v, KnownSymbol n) => Q v (PoolDecl n a) -> (Ref v n a -> s (Questions v)) -> Q v (Each s)
eachIn p f = EachQ [(refKey r, f r) | r <- refs p]

-- | A question about one pool entry, addressed by structured fields.
askAbout :: forall n v a. (JsonValue v, KnownSymbol n) => Ref v n a -> Text -> Q v Noul
askAbout r t = NoulQ (Structured [("question", jString t), ("pool", jString (label @n)), ("key", jString (refKey r))]) Omitted [refUse r]

-- | Prefix a runtime premise. The original instruction is preserved under
-- the premise; nested premises wrap again.
class Premised e where
  given :: JsonValue v => Text -> Q v e -> Q v e

prefix :: JsonValue v => Text -> Instructions v -> Instructions v
prefix premise original = Structured (("premise", jString premise) : inner)
  where
    inner = case original of
      NoInstructions -> []
      Instructions v -> [("instructions", v)]
      Structured kv -> [("instructions", jObject kv)]

instance Premised Noul where given p (NoulQ i c u) = NoulQ (prefix p i) c u
instance Premised (Choice alts) where given p (ChoiceQ i o) = ChoiceQ (prefix p i) o
instance Premised (Score levels) where given p (ScoreQ i d) = ScoreQ (prefix p i) d
instance Premised Scale where given p (ScaleQ i l) = ScaleQ (prefix p i) l

-- ---------------------------------------------------------------------------
-- Results
-- ---------------------------------------------------------------------------

-- | The fundamental eliminator: a selection (the chosen one, an accepted
-- one, or a ranked contender) against a handler per alternative in
-- declaration order. A missing, extra, or misordered handler is a type
-- error naming the labels.
-- | The wire key of a selection: a label, a runtime element key, or a sum
-- constructor's key.
selectedKey :: Alternatives alts => Selected v alts -> Text
selectedKey = altKeyOf

handle :: forall alts v r. Alternatives alts => Selected v alts -> Alts (Handler v r) alts -> r
handle s hs = altHandle hs s

-- | The unconditional convenience: eliminate the provider's choice.
caseOf :: forall alts v r. Alternatives alts => A v (Choice alts) -> Alts (Handler v r) alts -> r
caseOf a = handle (chosen a)

data Doubt
  = NearTie (Text, Double) (Text, Double)  -- winner and runner-up too close
  | Underweight Double                     -- the winner's mass is below the floor
  | Unconfident Double                     -- the provider's confidence is below the floor
  deriving (Show, Eq)

data Policy = Policy
  { minMass :: Double
  , minMargin :: Double
  , minConfidence :: Double
  }

lenient :: Policy
lenient = Policy 0 0 0

-- | Pure policy-aware selection: the chosen alternative, or structured
-- doubt. The answer stays in hand for inspection or resumption.
accept :: Alternatives alts => Policy -> A v (Choice alts) -> Either Doubt (Selected v alts)
accept policy a =
  let winner = altKeyOf (chosen a)
      mass = maybe 0 id (lookup winner (alternatives a))
      runnerUp = [r | r@(k, _) <- sortOn (negate . snd) (alternatives a), k /= winner]
  in if confidence a < minConfidence policy then Left (Unconfident (confidence a))
     else if mass < minMass policy then Left (Underweight mass)
     else case runnerUp of
       (k2, p2) : _ | mass - p2 < minMargin policy -> Left (NearTie (winner, mass) (k2, p2))
       _ -> Right (chosen a)

acceptOr :: Alternatives alts => (Doubt -> r) -> Policy -> A v (Choice alts) -> Alts (Handler v r) alts -> r
acceptOr onDoubt policy a hs = either onDoubt (`handle` hs) (accept policy a)

-- | Mass at or beyond a level, by label.
massAtOrAbove :: forall l levels v. KnownNat (Index l levels) => Label l -> A v (Score levels) -> Double
massAtOrAbove _ a = sum [m | (i, m) <- zip [0 :: Integer ..] (map snd (masses a)), i >= natVal (Proxy @(Index l levels))]

-- | The level nearest the expectation.
levelOf :: A v (Score levels) -> Text
levelOf a = case drop (round (expectation a)) (map fst (masses a)) of
  l : _ -> l
  [] -> maybe "" fst (safeLast (masses a))
  where safeLast xs = if null xs then Nothing else Just (last xs)

yesAbove :: Double -> A v Noul -> Bool
yesAbove floor' a = probabilityYes a >= floor'

noBelow :: Double -> A v Noul -> Bool
noBelow ceiling' a = probabilityYes a <= ceiling'

unsure :: Double -> A v Noul -> Bool
unsure margin a = abs (probabilityYes a - 0.5) < margin

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

isTopLevel :: Path -> Bool
isTopLevel = \case
  Segments [_] -> True
  Exactly _ -> True
  _ -> False

data Compiled v = Compiled
  { wire :: [(Text, WireQuestion v)]
  , declared :: [PoolUse v]
  , used :: [PoolUse v]
  }
instance Semigroup (Compiled v) where
  Compiled w d u <> Compiled w' d' u' = Compiled (w ++ w') (d ++ d') (u ++ u')
instance Monoid (Compiled v) where
  mempty = Compiled [] [] []

-- ---------------------------------------------------------------------------
-- Endpoints: compile, decode, unwrap, preview
-- ---------------------------------------------------------------------------

class JsonValue v => Endpoint v e where
  compileQ :: Path -> Q v e -> Either PrepError (Compiled v)
  decodeA :: Path -> Q v e -> [(Text, v)] -> Either DecodeError (A v e)
  -- | The answer as a cell reads it (transparent for nesting and pools).
  unwrapA :: A v e -> Answers v :- e
  -- | A payload-independent summary for inspection.
  previewA :: Answers v :- e -> v

-- | Preview an answer whose endpoint is fixed by the answer type.
previewAnswer :: forall v e. Endpoint v e => A v e -> v
previewAnswer = previewA @v @e . unwrapA

lookupAnswer :: Path -> [(Text, v)] -> Either DecodeError v
lookupAnswer p ws = maybe (Left (MissingAnswer key)) Right (lookup key ws)
  where key = encodePath p

leaf :: JsonValue v => Text -> WireQuestion v -> Compiled v
leaf key q = Compiled [(key, q)] [] []

instance JsonValue v => Endpoint v Noul where
  compileQ p (NoulQ i c uses) = do
    let key = encodePath p
    checkInstructions key i
    case c of
      Present (Just (Criteria y n)) -> do
        mapM_ (checkDescription key "true") [d | Present d <- [y]]
        mapM_ (checkDescription key "false") [d | Present d <- [n]]
      _ -> Right ()
    Right (leaf key (WNoul i c)) { used = uses }
  decodeA p _ ws = lookupAnswer p ws >>= \v -> do
    NoulAnswer x <- parseNoul (encodePath p) v
    Right (NoulA x)
  unwrapA = id
  previewA a = jObject [("noul", jNumber (probabilityYes a))]

instance (JsonValue v, Alternatives alts, AltsOk alts) => Endpoint v (Choice alts) where
  compileQ p (ChoiceQ i offer) = do
    let key = encodePath p
    checkInstructions key i
    alts <- altWire key offer
    if null alts then Left (EmptyOffer key) else Right ()
    if length alts > 255 then Left (TooManyAlternatives key (length alts)) else Right ()
    case [k | k <- map fst alts, length (filter (== k) (map fst alts)) > 1] of
      k : _ -> Left (KeyCollidesWithLabel key k)
      [] -> Right ()
    Right (leaf key (WChoice i alts)) { used = altUses offer }
  decodeA p (ChoiceQ _ offer) ws = lookupAnswer p ws >>= \v -> do
    let key = encodePath p
    ChoiceAnswer sel ms conf <- parseChoice key v
    alts <- altWire key offer `orDecode` key
    let keys = map fst alts
    winner <- maybe (Left (UnknownSelection key sel)) Right (altSelect offer sel)
    distribution key keys ms conf
    let rankedAll = [(m, s) | (k, m) <- sortOn (negate . snd) ms, Just s <- [altSelect offer k]]
    Right (Chosen winner rankedAll conf (sortOn (negate . snd) ms))
  unwrapA = id
  previewA a = jObject
    [ ("chosen", jString (altKeyOf (chosen a)))
    , ("confidence", jNumber (confidence a))
    , ("probabilities", jObject [(k, jNumber m) | (k, m) <- alternatives a])
    ]

orDecode :: Either PrepError x -> Text -> Either DecodeError x
orDecode e key = either (const (Left (Malformed key "retained offer failed to render"))) Right e

instance (JsonValue v, Rubric levels, RubricOk levels) => Endpoint v (Score levels) where
  compileQ p (ScoreQ i runtime) = do
    let key = encodePath p
        labels = rubricLabels @levels
    checkInstructions key i
    descs <- case (rubricDescriptions @levels, runtime) of
      (Just typed, Nothing) -> Right typed
      (_, Just given') ->
        if map fst given' /= labels then Left (RubricMismatch key) else Right (map snd given')
      (Nothing, Nothing) -> Left (RubricMismatch key)
    mapM_ (\(ix, l) -> checkLevel key ix l) (zip [0 ..] descs)
    Right (leaf key (WScore i descs))
  decodeA p q@(ScoreQ _ _) ws = lookupAnswer p ws >>= \v -> do
    let key = encodePath p
        labels = rubricLabels @levels
        indices = [T.pack (show i) | i <- [0 .. length labels - 1]]
    ScoreAnswer e lg ms conf <- parseScore key v
    descs <- case compileQ p q of
      Right (Compiled [(_, WScore _ ds)] _ _) -> Right ds
      _ -> Left (Malformed key "retained rubric failed to render")
    distribution key indices ms conf
    checkLegend key descs lg
    checkExpectation key (length labels) e
    let byIndex = [(l, maybe 0 id (lookup i ms)) | (i, l) <- zip indices labels]
        byLabel = [(l, d) | (i, l) <- zip indices labels, Just d <- [lookup i lg]]
    Right (Scored e byIndex conf byLabel)
  unwrapA = id
  previewA a = jObject
    [ ("score", jNumber (expectation a))
    , ("confidence", jNumber (scoreConfidence a))
    , ("probabilities", jObject [(l, jNumber m) | (l, m) <- masses a])
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

instance JsonValue v => Endpoint v Scale where
  compileQ p (ScaleQ i (Levels ls)) = do
    let key = encodePath p
    checkInstructions key i
    if null ls || length ls > 10 then Left (BadLevelCount key (length ls)) else Right ()
    mapM_ (\(ix, l) -> checkLevel key ix l) (zip [0 ..] ls)
    Right (leaf key (WScore i ls))
  decodeA p (ScaleQ _ (Levels ls)) ws = lookupAnswer p ws >>= \v -> do
    let key = encodePath p
        indices = [T.pack (show i) | i <- [0 .. length ls - 1]]
    ScoreAnswer e lg ms conf <- parseScore key v
    distribution key indices ms conf
    checkLegend key ls lg
    checkExpectation key (length ls) e
    built <- mapM (\(i, c) -> maybe (Left (MissingMass key i)) (Right . (,) c) (lookup i ms)) (zip indices ls)
    Right (Scaled e built conf)
  unwrapA = id
  previewA a = jObject [("score", jNumber (scaleExpectation a)), ("confidence", jNumber (scaleConfidence a))]

instance Schema v s => Endpoint v (Each s) where
  compileQ p (EachQ items) = mconcat <$> mapM (\(k, q) -> compileSchema (extend p k) q) items
  decodeA p (EachQ items) ws = EachA <$> mapM (\(k, q) -> (,) k <$> decodeSchema (extend p k) q ws) items
  unwrapA (EachA xs) = xs
  previewA xs = jObject [(k, previewSchema x) | (k, x) <- xs]

instance Schema v s => Endpoint v (Group s) where
  compileQ p (GroupQ q) = compileSchema p q
  decodeA p (GroupQ q) ws = GroupA <$> decodeSchema p q ws
  unwrapA (GroupA x) = x
  previewA = previewSchema

instance JsonValue v => Endpoint v Dynamic where
  compileQ p (DynamicQ qs) = mconcat <$> mapM (\(k, SomeQ q) -> compileQ (extend p k) q) qs
  decodeA p (DynamicQ qs) ws = DynamicA <$> mapM (\(k, SomeQ q) -> (,) k . SomeA q <$> decodeA (extend p k) q ws) qs
  unwrapA = id
  previewA (DynamicA xs) = jObject [(k, previewAnswer a) | (k, SomeA _ a) <- xs]

instance JsonValue v => Endpoint v Raw where
  compileQ p (RawQ v) = Right (leaf (encodePath p) (WRaw v))
  decodeA p _ ws = RawA <$> lookupAnswer p ws
  unwrapA = id
  previewA (RawA v) = v

instance (JsonValue v, KnownSymbol n) => Endpoint v (PoolDecl n a) where
  compileQ p q = do
    if isTopLevel p then Right () else Left (PoolDeclaredInNested (label @n))
    Right (Compiled [] [poolUse q] [])
  decodeA _ (PoolQ es) _ = Right (PoolA (Pool es))
  unwrapA (PoolA x) = x
  previewA x = jObject [(k, d) | (k, d, _) <- poolEntries x]

-- ---------------------------------------------------------------------------
-- Packets
-- ---------------------------------------------------------------------------

data (k :: Symbol) ::= (e :: Type)

-- | A cell: a labeled question under 'Questions', a decoded answer under
-- 'Answers'. A pool cell's label is its pool's name, by construction.
data Cell (k :: Symbol) (e :: Type) mode where
  (:=) :: CellOk k e => Label k -> Q v e -> Cell k e (Questions v)
  Answered :: (Answers v :- e) -> Cell k e (Answers v)
infix 6 :=

type family CellOk (k :: Symbol) (e :: Type) :: Constraint where
  CellOk k (PoolDecl n a) = PoolNamed k n
  CellOk k e = ()

type family PoolNamed (k :: Symbol) (n :: Symbol) :: Constraint where
  PoolNamed k k = ()
  PoolNamed k n = TypeError ('Text "Jev: pool #" ':<>: 'Text n ':<>: 'Text " placed under label #" ':<>: 'Text k ':<>: 'Text "; a pool's cell label must be its name")

data Packet (fs :: [Type]) mode where
  Nil :: Packet '[] mode
  (:&) :: Cell k e mode -> Packet fs mode -> Packet (k ::= e ': fs) mode
infixr 5 :&

type family (++) (a :: [k]) (b :: [k]) :: [k] where
  '[] ++ b = b
  (x ': a) ++ b = x ': (a ++ b)

(++.) :: Packet fs m -> Packet gs m -> Packet (fs ++ gs) m
Nil ++. g = g
(c :& p) ++. g = c :& (p ++. g)
infixr 5 ++.

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

type family Lookup (k :: Symbol) (fs :: [Type]) :: Type where
  Lookup k (k ::= e ': fs) = e
  Lookup k (j ::= e ': fs) = Lookup k fs

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
  get' (Answered a :& _) = a
instance Get k fs all e => Get' 'False k (j ::= e' ': fs) all e where
  get' (_ :& p) = get @k @fs @all p

instance (Get k fs fs e, r ~ (Answers v :- e)) => HasField k (Packet fs (Answers v)) r where
  getField = get @k @fs @fs

-- | The packet traversal, by induction over the labels.
class JsonValue v => PacketSchema v (fs :: [Type]) where
  packetCompile :: Path -> Packet fs (Questions v) -> Either PrepError (Compiled v)
  packetDecode :: Path -> Packet fs (Questions v) -> [(Text, v)] -> Either DecodeError (Packet fs (Answers v))
  packetPreview :: Packet fs (Answers v) -> [(Text, v)]

instance JsonValue v => PacketSchema v '[] where
  packetCompile _ Nil = Right mempty
  packetDecode _ Nil _ = Right Nil
  packetPreview Nil = []

instance (KnownSymbol k, Endpoint v e, PacketSchema v fs) => PacketSchema v (k ::= e ': fs) where
  packetCompile p (Label := q :& rest) = (<>) <$> compileQ (extend p (label @k)) q <*> packetCompile p rest
  packetDecode p (Label := q :& rest) ws = (:&) <$> (Answered . unwrapA <$> decodeA (extend p (label @k)) q ws) <*> packetDecode p rest ws
  packetPreview (Answered a :& rest) = (label @k, previewA @v @e a) : packetPreview rest

instance (Show v, PacketSchema v fs) => Show (Packet fs (Answers v)) where
  show p = show (jObject (packetPreview p))

-- ---------------------------------------------------------------------------
-- Schemas
-- ---------------------------------------------------------------------------

class JsonValue v => Schema v (s :: Type -> Type) where
  compileSchema :: Path -> s (Questions v) -> Either PrepError (Compiled v)
  decodeSchema :: Path -> s (Questions v) -> [(Text, v)] -> Either DecodeError (s (Answers v))
  previewSchema :: s (Answers v) -> v

instance (JsonValue v, Unique fs, PacketSchema v fs) => Schema v (Packet fs) where
  compileSchema = packetCompile
  decodeSchema = packetDecode
  previewSchema = jObject . packetPreview

-- | A root-level dynamic map whose keys go on the wire verbatim.
newtype Exact mode = Exact [(Text, ExactLeaf mode)]

type family ExactLeaf mode where
  ExactLeaf (Questions v) = SomeQ v
  ExactLeaf (Answers v) = SomeA v

exact :: [(Text, SomeQ v)] -> Exact (Questions v)
exact = Exact

exactAnswers :: Exact (Answers v) -> [(Text, SomeA v)]
exactAnswers (Exact xs) = xs

instance JsonValue v => Schema v Exact where
  compileSchema _ (Exact qs) = mconcat <$> mapM (\(k, SomeQ q) -> compileQ (Exactly k) q) qs
  decodeSchema _ (Exact qs) ws = Exact <$> mapM (\(k, SomeQ q) -> (,) k . SomeA q <$> decodeA (Exactly k) q ws) qs
  previewSchema (Exact xs) = jObject [(k, previewAnswer a) | (k, SomeA _ a) <- xs]

-- ---------------------------------------------------------------------------
-- The operation: prepare, render, decode
-- ---------------------------------------------------------------------------

newtype Model = Model Text deriving (Eq, Show)

jevLatest :: Model
jevLatest = Model "jev-latest"

data Prepared v s = forall p. Prepared
  { preparedQuestions :: s (Questions v)
  , preparedWire :: [(Text, WireQuestion v)]
  , preparedModel :: Model
  , preparedState :: State p v
  , preparedPools :: [PoolUse v]
  }

prepare :: Schema v s => Model -> State p v -> s (Questions v) -> Either PrepError (Prepared v s)
prepare model st q = do
  checkState st
  Compiled qs decl uses <- compileSchema (Segments []) q
  case [n | (n, _) <- decl, length (filter ((== n) . fst) decl) > 1] of
    n : _ -> Left (DuplicatePool n)
    [] -> Right ()
  if not (null decl) && not (isPooled st) then Left PoolsRequirePooledState else Right ()
  mapM_ (\(n, u) -> case lookup n decl of
    Nothing -> Left (UndeclaredPool n)
    Just d -> if jEqual d u then Right () else Left (ConflictingPool n)) uses
  let keys = map fst qs
  if null qs then Left EmptyQuestionMap else Right ()
  case [k | k <- keys, T.null k] of
    _ : _ -> Left (EmptyQuestionKey "")
    [] -> Right ()
  case [k | k <- keys, length (filter (== k) keys) > 1] of
    k : _ -> Left (DuplicateQuestionPath k)
    [] -> Right ()
  Right (Prepared q qs model st decl)

-- | The request body a transport sends.
requestValue :: JsonValue v => Prepared v s -> v
requestValue (Prepared _ qs (Model m) st pools) = jObject
  [ ("model", jString m)
  , ("state", if isPooled st then jObject [("context", stateValue st), ("pools", jObject pools)] else stateValue st)
  , ("questions", jObject [(k, questionValue q) | (k, q) <- qs])
  ]

data Response v s = Response
  { answers :: s (Answers v)
  , resolvedModel :: Text
  , usage :: v
  , diagnostics :: [Text]
  }

-- | Decode a response body against this exact request.
decodeResponse :: Schema v s => Prepared v s -> v -> Either DecodeError (Response v s)
decodeResponse (Prepared q qs _ _ _) body = parseEnvelope body >>= \case
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
  => (v -> m (Either Text v)) -> Model -> State p v -> s (Questions v)
  -> m (Either JevError (Response v s))
roundTrip transport model st q = case prepare model st q of
  Left e -> pure (Left (Prepare e))
  Right prepared -> transport (requestValue prepared) >>= \case
    Left t -> pure (Left (Transport t))
    Right body -> pure (either (Left . Decode) Right (decodeResponse prepared body))

-- | The tiny use: one question, one answer.
jev1
  :: (Monad m, Endpoint v e, CellOk "value" e)
  => (v -> m (Either Text v)) -> Model -> State p v -> Q v e
  -> m (Either JevError (Answers v :- e))
jev1 transport model st q = fmap (fmap (\r -> case answers r of Answered a :& Nil -> a)) (roundTrip transport model st ((Label :: Label "value") := q :& Nil))
