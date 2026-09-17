{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | One authored record, interpreted under 'Questions' and 'Answers'.
--
-- Boot packages only. Polymorphic over the JSON value type through
-- "Jev.Core.Json"; a facade such as "Jev" fixes the type so users never see
-- it. Data-family leaves keep each field's endpoint recoverable, so one
-- generic traversal compiles the question record to the wire and rebuilds
-- the answer record from the response against exactly the alternatives this
-- request sent.
module Jev.Core.Schema
  ( -- * Modes and the operator
    Questions, Answers, Masses, Legend, Handlers, type (:-)
    -- * Endpoints
  , Noul, Choice, Choose, Score, Scale, Each, Group, Many, Raw, Option, Level
    -- * Leaves
  , Q (..), A (..), SomeQ (..), SomeA (..)
    -- * Builders (all total; shapes are checked at 'prepare')
  , noul, noulWith, choice, choiceWith, choose, chooseWith, score, scoreWith, scale
  , each, group, many, rawUnchecked, option, optionWith, optionKeyed, level, levelWith
  , Candidates, candidates, Candidate (..), Exit (..), noMatch, deferToModel
  , Levels, levelsOf, Premised (..)
    -- * Results
  , ChoiceResult, Selected, Distribution, match, withChoice, probabilityOf, selectedKey, masses
  , Picked (..), pickOr, ranked, yesAbove, noBelow, unsure
  , eachAnswers, groupAnswer, manyAnswers, rawAnswer
    -- * Schemas and the operation
  , Schema (..), Only (..), Exact (..), ExactLeaf, exact, exactAnswers
  , Model (..), jevLatest
  , Prepared, preparedQuestions, preparedModel, preparedState, preparedWire
  , prepare, requestValue, decodeResponse, Response (..), JevError (..), roundTrip, jev1
    -- * Generic classes (exported for constraints on user signatures)
  , GApply, GCompile, GDecode, Endpoint
  ) where

import Data.Char (isUpper, toLower)
import Data.Kind (Type)
import Data.List (nub, sortOn)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Jev.Core.Contract
import Jev.Core.Json

-- ---------------------------------------------------------------------------
-- Modes, endpoints, operator
-- ---------------------------------------------------------------------------

data Questions (v :: Type)
data Answers (v :: Type)
data Masses
data Legend (v :: Type)
data Handlers (result :: Type)

data Noul
data Choice (opts :: Type -> Type)
data Choose (payload :: Type)
data Score (levels :: Type -> Type)
data Scale
data Each (schema :: Type -> Type)
data Group (schema :: Type -> Type)
data Many
data Raw
data Option (payload :: Type)
data Level

data family Q (v :: Type) endpoint
data family A (v :: Type) endpoint

type family mode :- endpoint where
  Questions v :- e = Q v e
  Answers v :- e = A v e
  Masses :- Option a = Double
  Handlers r :- Option a = a -> r
  Masses :- Level = Double
  Legend v :- Level = v
  mode :- e =
    TypeError
      ( 'Text "unsupported Jev field: `" ':<>: 'ShowType e
          ':<>: 'Text "` under mode `" ':<>: 'ShowType mode ':<>: 'Text "`."
          ':$$: 'Text "Question records use Questions/Answers; alternative records use Questions, Masses, or Handlers r; level records use Questions, Masses, or Legend."
      )
infixr 0 :-

-- ---------------------------------------------------------------------------
-- Leaves
-- ---------------------------------------------------------------------------

data instance Q v Noul = NoulQ (Instructions v) (Presence (Maybe (NoulCriteria v)))
newtype instance A v Noul = NoulA { probabilityYes :: Double }

data instance Q v (Choice opts) = ChoiceQ (Instructions v) (opts (Questions v))
data instance A v (Choice opts) = ChoiceA
  { choiceResult :: ChoiceResult v opts
  , confidence :: Double
  }

data instance Q v (Choose a) = ChooseQ (Instructions v) (Candidates v a) [Exit v]
data instance A v (Choose a) = ChooseA
  { picked :: Picked v a
  , chooseRanked :: [(Candidate v a, Double)]
  , exitMass :: [(Text, Double)]
  , chooseConfidence :: Double
  }

data instance Q v (Score ls) = ScoreQ (Instructions v) (ls (Questions v))
data instance A v (Score ls) = ScoreA
  { expectation :: Double
  , levelMasses :: ls Masses
  , legend :: ls (Legend v)
  , scoreConfidence :: Double
  }

-- | A runtime-sized ordered rubric.
data instance Q v Scale = ScaleQ (Instructions v) (Levels v)
data instance A v Scale = ScaleA
  { scaleExpectation :: Double
  , scaleMasses :: [(v, Double)]   -- in rubric order, legend paired
  , scaleConfidence :: Double
  }

newtype instance Q v (Each s) = EachQ [(Text, s (Questions v))]
newtype instance A v (Each s) = EachA [(Text, s (Answers v))]

newtype instance Q v (Group s) = GroupQ (s (Questions v))
newtype instance A v (Group s) = GroupA (s (Answers v))

-- | A fully dynamic, heterogeneous question map.
newtype instance Q v Many = ManyQ [(Text, SomeQ v)]
newtype instance A v Many = ManyA [(Text, SomeA v)]

-- | UNCHECKED escape hatch, outside the validity guarantee: any value is sent
-- as the question object, and the answer is the original parsed JSON.
newtype instance Q v Raw = RawQ v
newtype instance A v Raw = RawA v

-- | An alternative: optional wire-key override, description, local payload.
data instance Q v (Option a) = OptionQ (Maybe Text) (Description v) a
newtype instance Q v Level = LevelQ v

data SomeQ v = forall e. Endpoint v e => SomeQ (Q v e)
data SomeA v = forall e. Endpoint v e => SomeA (Q v e) (A v e)

-- ---------------------------------------------------------------------------
-- Builders
-- ---------------------------------------------------------------------------

text :: JsonValue v => Text -> Instructions v
text = Present . jString

noul :: JsonValue v => Text -> Q v Noul
noul t = NoulQ (text t) Omitted

noulWith :: Instructions v -> Presence (Maybe (NoulCriteria v)) -> Q v Noul
noulWith = NoulQ

choice :: JsonValue v => Text -> opts (Questions v) -> Q v (Choice opts)
choice t = ChoiceQ (text t)

choiceWith :: Instructions v -> opts (Questions v) -> Q v (Choice opts)
choiceWith = ChoiceQ

choose :: JsonValue v => Text -> Candidates v a -> [Exit v] -> Q v (Choose a)
choose t = ChooseQ (text t)

chooseWith :: Instructions v -> Candidates v a -> [Exit v] -> Q v (Choose a)
chooseWith = ChooseQ

score :: JsonValue v => Text -> ls (Questions v) -> Q v (Score ls)
score t = ScoreQ (text t)

scoreWith :: Instructions v -> ls (Questions v) -> Q v (Score ls)
scoreWith = ScoreQ

scale :: Instructions v -> Levels v -> Q v Scale
scale = ScaleQ

each :: [(Text, x)] -> (x -> s (Questions v)) -> Q v (Each s)
each items f = EachQ [(k, f x) | (k, x) <- items]

group :: s (Questions v) -> Q v (Group s)
group = GroupQ

many :: [(Text, SomeQ v)] -> Q v Many
many = ManyQ

-- | Unchecked; see 'Raw'.
rawUnchecked :: v -> Q v Raw
rawUnchecked = RawQ

-- | A text-described alternative with a local payload.
option :: JsonValue v => Text -> a -> Q v (Option a)
option t = OptionQ Nothing (jString t)

-- | A structured (or null) description.
optionWith :: Description v -> a -> Q v (Option a)
optionWith = OptionQ Nothing

-- | Any provider-valid key, including punctuation or the empty string,
-- instead of the snake-cased selector.
optionKeyed :: Text -> Description v -> a -> Q v (Option a)
optionKeyed k = OptionQ (Just k)

level :: JsonValue v => Text -> Q v Level
level = LevelQ . jString

levelWith :: v -> Q v Level
levelWith = LevelQ

-- | Prefix a premise to a question's instructions, so a speculative question
-- states the branch it assumes. The original instruction is preserved as-is
-- under the premise; nested premises wrap again.
class Premised e where
  given :: JsonValue v => Text -> Q v e -> Q v e

prefix :: JsonValue v => Text -> Instructions v -> Instructions v
prefix premise original = Present (jObject (("premise", jString premise) : inner))
  where
    inner = case original of
      Omitted -> []
      Present v -> [("instructions", v)]

instance Premised Noul where given p (NoulQ i c) = NoulQ (prefix p i) c
instance Premised (Choice opts) where given p (ChoiceQ i o) = ChoiceQ (prefix p i) o
instance Premised (Choose a) where given p (ChooseQ i c e) = ChooseQ (prefix p i) c e
instance Premised (Score ls) where given p (ScoreQ i l) = ScoreQ (prefix p i) l
instance Premised Scale where given p (ScaleQ i l) = ScaleQ (prefix p i) l

-- | A homogeneous candidate set. Total; emptiness, duplicate keys, and
-- cardinality are checked at 'prepare'.
newtype Candidates v a = Candidates [Candidate v a]

data Candidate v a = Candidate
  { candidateKey :: Text
  , candidateDescription :: Description v
  , candidatePayload :: a
  }

candidates :: [(Text, Description v, a)] -> Candidates v a
candidates xs = Candidates [Candidate k d a | (k, d, a) <- xs]

-- | A static exit beside dynamic candidates. For static 'Choice' records an
-- exit is simply another 'Option' field.
data Exit v = Exit { exitKey :: Text, exitDescription :: Description v }

noMatch :: JsonValue v => Text -> Exit v
noMatch = Exit "no_match" . jString

deferToModel :: JsonValue v => Text -> Exit v
deferToModel = Exit "defer_to_model" . jString

-- | A runtime rubric, in order. Total; the count is checked at 'prepare'.
newtype Levels v = Levels [v]

levelsOf :: [v] -> Levels v
levelsOf = Levels

-- ---------------------------------------------------------------------------
-- Results
-- ---------------------------------------------------------------------------

-- | The selected key with the exact alternatives record it was selected
-- from. The scope phantom guards selections against distributions from
-- another result; handler records are not scoped.
type role Selected nominal nominal nominal
data Selected (scope :: Type) v opts = Selected Text (opts (Questions v)) (opts Masses -> Double)

type role Distribution nominal nominal
newtype Distribution (scope :: Type) opts = Distribution (opts Masses)

data ChoiceResult v opts = forall scope. ChoiceResult (Selected scope v opts) (Distribution scope opts)

-- | Eliminate with an exhaustive handler record for the same alternatives
-- record. A foreign handler record or a wrong payload type fails here.
match
  :: forall v opts r.
     (Generic (opts (Questions v)), Generic (opts (Handlers r)),
      GApply v (Rep (opts (Questions v))) (Rep (opts (Handlers r))) r)
  => A v (Choice opts) -> opts (Handlers r) -> r
match (ChoiceA (ChoiceResult (Selected k q _) _) _) hs =
  case lookup k (gApply @v (from q) (from hs)) of
    Just r -> r
    Nothing -> error "selected key is not an alternative of this record"

withChoice
  :: A v (Choice opts)
  -> (forall scope. Selected scope v opts -> Distribution scope opts -> r)
  -> r
withChoice (ChoiceA (ChoiceResult s d) _) use = use s d

probabilityOf :: Selected scope v opts -> Distribution scope opts -> Double
probabilityOf (Selected _ _ project) (Distribution ms) = project ms

-- | The full distribution as the alternatives record under 'Masses'.
masses :: A v (Choice opts) -> opts Masses
masses (ChoiceA (ChoiceResult _ (Distribution m)) _) = m

selectedKey :: A v (Choice opts) -> Text
selectedKey (ChoiceA (ChoiceResult (Selected k _ _) _) _) = k

data Picked v a = PickedCandidate (Candidate v a) | PickedExit (Exit v)

-- | The tiny use: run the continuation on a picked payload, or hand back.
pickOr :: (Exit v -> m r) -> A v (Choose a) -> (a -> m r) -> m r
pickOr handBack answer continue = case picked answer of
  PickedCandidate c -> continue (candidatePayload c)
  PickedExit e -> handBack e

-- | Every candidate and exit by descending mass, so a winning exit is first.
ranked :: A v (Choose a) -> [(Text, Double)]
ranked a = sortOn (negate . snd) ([(candidateKey c, p) | (c, p) <- chooseRanked a] ++ exitMass a)

-- | Decision vocabulary for Nouls, so a tree of natural-language conditions
-- reads like the sentence it encodes.
yesAbove :: Double -> A v Noul -> Bool
yesAbove floor' a = probabilityYes a >= floor'

noBelow :: Double -> A v Noul -> Bool
noBelow ceiling' a = probabilityYes a <= ceiling'

-- | Neither side clears its threshold.
unsure :: Double -> A v Noul -> Bool
unsure margin a = abs (probabilityYes a - 0.5) < margin

eachAnswers :: A v (Each s) -> [(Text, s (Answers v))]
eachAnswers (EachA xs) = xs

groupAnswer :: A v (Group s) -> s (Answers v)
groupAnswer (GroupA x) = x

manyAnswers :: A v Many -> [(Text, SomeA v)]
manyAnswers (ManyA xs) = xs

rawAnswer :: A v Raw -> v
rawAnswer (RawA x) = x

-- ---------------------------------------------------------------------------
-- Paths
-- ---------------------------------------------------------------------------

-- | Flattened question ids. 'Segments' joins with dots after escaping dots
-- and backslashes inside a segment, so distinct segment lists give distinct
-- ids and any provider-valid key is admissible. 'Exactly' is a root-level id
-- used verbatim.
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
-- Endpoints: compile and decode per leaf
-- ---------------------------------------------------------------------------

class JsonValue v => Endpoint v e where
  compileQ :: Path -> Q v e -> Either PrepError [(Text, WireQuestion v)]
  decodeA :: Path -> Q v e -> [(Text, v)] -> Either DecodeError (A v e)

lookupAnswer :: Path -> [(Text, v)] -> Either DecodeError v
lookupAnswer p ws = maybe (Left (MissingAnswer key)) Right (lookup key ws)
  where key = encodePath p

uniqueWire :: Text -> [Text] -> Either PrepError ()
uniqueWire key ks = case [k | k <- ks, length (filter (== k) ks) > 1] of
  k : _ -> Left (DuplicateWireKey key k)
  [] -> Right ()

instance JsonValue v => Endpoint v Noul where
  compileQ p (NoulQ i c) = do
    let key = encodePath p
    checkInstructions key i
    case c of
      Present (Just (NoulCriteria y n)) -> do
        mapM_ (checkDescription key "true") [d | Present d <- [y]]
        mapM_ (checkDescription key "false") [d | Present d <- [n]]
      _ -> Right ()
    Right [(key, WNoul i c)]
  decodeA p _ ws = lookupAnswer p ws >>= \v -> do
    NoulAnswer x <- parseNoul (encodePath p) v
    Right (NoulA x)

instance (JsonValue v, Generic (opts (Questions v)), GAlts v (Rep (opts (Questions v))),
          GSelectors (Rep (opts (Questions v))),
          Generic (opts Masses), GBuild Double (Rep (opts Masses)), GCollect Double (Rep (opts Masses)))
      => Endpoint v (Choice opts) where
  compileQ p (ChoiceQ i opts) = do
    let alts = gAlts @v (from opts)
        key = encodePath p
    checkInstructions key i
    if length alts > 255 || null alts then Left (TooManyAlternatives key (length alts)) else Right ()
    uniqueWire key (map fst alts)
    mapM_ (\(k, d) -> checkDescription key k d) alts
    Right [(key, WChoice i alts)]
  decodeA p (ChoiceQ _ opts) ws = lookupAnswer p ws >>= \v -> do
    let key = encodePath p
    ChoiceAnswer sel ms conf <- parseChoice key v
    let alts = gAlts @v (from opts)
        keys = map fst alts
        -- original selector identity -> wire key; normalization is only the default label
        wireOf = zip (gSelectors (from opts)) keys
    if sel `notElem` keys then Left (UnknownSelection key sel) else Right ()
    distribution key keys ms conf
    built <- to <$> gBuild @Double (\field -> maybe (Left (MissingMass key field)) Right (lookup field wireOf >>= \w -> lookup w ms))
    let selField = maybe sel id (lookup sel [(w, f) | (f, w) <- wireOf])
        project m = maybe 0 id (lookup selField (gCollect @Double (from m)))
    Right (ChoiceA (ChoiceResult (Selected sel opts project) (Distribution built)) conf)

instance JsonValue v => Endpoint v (Choose a) where
  compileQ p (ChooseQ i (Candidates cs) exits) = do
    let keys = map candidateKey cs
        key = encodePath p
    checkInstructions key i
    if null cs then Left (EmptyCandidates key) else Right ()
    if length keys /= length (nub keys) then Left (DuplicateKeys key [k | k <- nub keys, length (filter (== k) keys) > 1]) else Right ()
    mapM_ (\e -> if exitKey e `elem` keys then Left (ExitCollidesWithCandidate key (exitKey e)) else Right ()) exits
    uniqueWire key (map exitKey exits)
    let total = length cs + length exits
    if total > 255 then Left (TooManyAlternatives key total) else Right ()
    mapM_ (\c -> checkDescription key (candidateKey c) (candidateDescription c)) cs
    mapM_ (\e -> checkDescription key (exitKey e) (exitDescription e)) exits
    Right [(key, WChoice i ([(candidateKey c, candidateDescription c) | c <- cs]
                            ++ [(exitKey e, exitDescription e) | e <- exits]))]
  decodeA p (ChooseQ _ (Candidates cs) exits) ws = lookupAnswer p ws >>= \v -> do
    let key = encodePath p
    ChoiceAnswer sel ms conf <- parseChoice key v
    let mass k = maybe (Left (MissingMass key k)) Right (lookup k ms)
    pick <- case filter ((== sel) . candidateKey) cs of
      c : _ -> Right (PickedCandidate c)
      [] -> case filter ((== sel) . exitKey) exits of
        e : _ -> Right (PickedExit e)
        [] -> Left (UnknownSelection key sel)
    distribution key (map candidateKey cs ++ map exitKey exits) ms conf
    rankedCs <- mapM (\c -> (,) c <$> mass (candidateKey c)) cs
    exitMs <- mapM (\e -> (,) (exitKey e) <$> mass (exitKey e)) exits
    Right (ChooseA pick (sortOn (negate . snd) rankedCs) exitMs conf)

checkLegend :: JsonValue v => Text -> [v] -> [(Text, v)] -> Either DecodeError ()
checkLegend key levels lg =
  let indices = [T.pack (show i) | i <- [0 .. length levels - 1]]
      matches = and [maybe False (jEqual l) (lookup i lg) | (i, l) <- zip indices levels]
  in if not matches || length lg /= length indices || any (`notElem` indices) (map fst lg)
       then Left (LegendMismatch key) else Right ()

checkExpectation :: Text -> Int -> Double -> Either DecodeError ()
checkExpectation key n e =
  if isNaN e || isInfinite e || e < 0 || e > fromIntegral (n - 1) then Left (ValueOutOfRange key "score") else Right ()

instance (JsonValue v, Generic (ls (Questions v)), GLevels v (Rep (ls (Questions v))),
          Generic (ls Masses), GBuild Double (Rep (ls Masses)),
          Generic (ls (Legend v)), GBuild v (Rep (ls (Legend v))))
      => Endpoint v (Score ls) where
  compileQ p (ScoreQ i ls) = do
    let levels = gLevels @v (from ls)
        key = encodePath p
    checkInstructions key i
    if length levels > 10 || null levels then Left (BadLevelCount key (length levels)) else Right ()
    mapM_ (\(ix, (_, l)) -> checkLevel key ix l) (zip [0 ..] levels)
    Right [(key, WScore i (map snd levels))]
  decodeA p (ScoreQ _ ls) ws = lookupAnswer p ws >>= \v -> do
    let key = encodePath p
    ScoreAnswer e lg ms conf <- parseScore key v
    let named = gLevels @v (from ls)
        names = map fst named
        indices = [T.pack (show i) | i <- [0 .. length names - 1]]
        index name = maybe (Left (MissingMass key name)) (Right . T.pack . show) (lookup name (zip names [0 :: Int ..]))
        byIndex :: [(Text, x)] -> Text -> Either DecodeError x
        byIndex table name = index name >>= \i -> maybe (Left (MissingMass key i)) Right (lookup i table)
    distribution key indices ms conf
    checkLegend key (map snd named) lg
    checkExpectation key (length names) e
    built <- to <$> gBuild @Double (byIndex ms)
    lgd <- to <$> gBuild @v (byIndex lg)
    Right (ScoreA e built lgd conf)

instance JsonValue v => Endpoint v Scale where
  compileQ p (ScaleQ i (Levels ls)) = do
    let key = encodePath p
    checkInstructions key i
    if null ls || length ls > 10 then Left (BadLevelCount key (length ls)) else Right ()
    mapM_ (\(ix, l) -> checkLevel key ix l) (zip [0 ..] ls)
    Right [(key, WScore i ls)]
  decodeA p (ScaleQ _ (Levels ls)) ws = lookupAnswer p ws >>= \v -> do
    let key = encodePath p
    ScoreAnswer e lg ms conf <- parseScore key v
    let indices = [T.pack (show i) | i <- [0 .. length ls - 1]]
    distribution key indices ms conf
    checkLegend key ls lg
    checkExpectation key (length ls) e
    built <- mapM (\(i, c) -> maybe (Left (MissingMass key i)) (Right . (,) c) (lookup i ms)) (zip indices ls)
    Right (ScaleA e built conf)

instance JsonValue v => Endpoint v Many where
  compileQ p (ManyQ qs) = concat <$> mapM (\(k, SomeQ q) -> compileQ (extend p k) q) qs
  decodeA p (ManyQ qs) ws = ManyA <$> mapM (\(k, SomeQ q) -> (,) k . SomeA q <$> decodeA (extend p k) q ws) qs

instance JsonValue v => Endpoint v Raw where
  compileQ p (RawQ v) = Right [(encodePath p, WRaw v)]
  decodeA p _ ws = RawA <$> lookupAnswer p ws

instance
  (JsonValue v, TypeError ('Text "Level is a field of a Score level record, not a question endpoint; use `Score levels` in the question record"))
  => Endpoint v Level where
  compileQ = undefined
  decodeA = undefined

instance
  (JsonValue v, TypeError ('Text "Option is a field of a Choice alternatives record, not a question endpoint; use `Choice opts` or `Choose payload` in the question record"))
  => Endpoint v (Option a) where
  compileQ = undefined
  decodeA = undefined

instance (JsonValue v, Schema s) => Endpoint v (Each s) where
  compileQ p (EachQ items) = concat <$> mapM (\(k, q) -> compileSchema (extend p k) q) items
  decodeA p (EachQ items) ws = EachA <$> mapM (\(k, q) -> (,) k <$> decodeSchema (extend p k) q ws) items

instance (JsonValue v, Schema s) => Endpoint v (Group s) where
  compileQ p (GroupQ q) = compileSchema p q
  decodeA p (GroupQ q) ws = GroupA <$> decodeSchema p q ws

-- ---------------------------------------------------------------------------
-- Schema: one generic traversal over paired representations
-- ---------------------------------------------------------------------------

-- | A question record. The default methods derive both directions from
-- 'Generic'; the JSON type is universally quantified, so an instance never
-- mentions it.
class Schema (s :: Type -> Type) where
  compileSchema :: JsonValue v => Path -> s (Questions v) -> Either PrepError [(Text, WireQuestion v)]
  default compileSchema
    :: forall v. (JsonValue v, Generic (s (Questions v)), GCompile v (Rep (s (Questions v))))
    => Path -> s (Questions v) -> Either PrepError [(Text, WireQuestion v)]
  compileSchema p q = gCompile @v p (from q)

  decodeSchema :: JsonValue v => Path -> s (Questions v) -> [(Text, v)] -> Either DecodeError (s (Answers v))
  default decodeSchema
    :: forall v. (JsonValue v, Generic (s (Questions v)), Generic (s (Answers v)), GDecode v (Rep (s (Questions v))) (Rep (s (Answers v))))
    => Path -> s (Questions v) -> [(Text, v)] -> Either DecodeError (s (Answers v))
  decodeSchema p q ws = to <$> gDecode @v p (from q) ws

class GCompile v (fq :: Type -> Type) where
  gCompile :: Path -> fq x -> Either PrepError [(Text, WireQuestion v)]

instance GCompile v fq => GCompile v (M1 D d fq) where gCompile p (M1 x) = gCompile @v p x
instance GCompile v fq => GCompile v (M1 C c fq) where gCompile p (M1 x) = gCompile @v p x
instance (Selector sel, GCompile v fq) => GCompile v (M1 S sel fq) where
  gCompile p m@(M1 x) = gCompile @v (extend p (toSnakeCase (selName m))) x
instance (GCompile v fq, GCompile v gq) => GCompile v (fq :*: gq) where
  gCompile p (l :*: r) = (++) <$> gCompile @v p l <*> gCompile @v p r
instance Endpoint v e => GCompile v (K1 i (Q v e)) where gCompile p (K1 q) = compileQ p q
instance
  TypeError ('Text "a Jev question record must be a single-constructor record of endpoints")
  => GCompile v (fq :+: gq) where
  gCompile = undefined

class GDecode v (fq :: Type -> Type) (fa :: Type -> Type) where
  gDecode :: Path -> fq x -> [(Text, v)] -> Either DecodeError (fa x)

instance GDecode v fq fa => GDecode v (M1 D d fq) (M1 D d fa) where
  gDecode p (M1 x) ws = M1 <$> gDecode @v p x ws
instance GDecode v fq fa => GDecode v (M1 C c fq) (M1 C c fa) where
  gDecode p (M1 x) ws = M1 <$> gDecode @v p x ws
instance (Selector sel, GDecode v fq fa) => GDecode v (M1 S sel fq) (M1 S sel fa) where
  gDecode p m@(M1 x) ws = M1 <$> gDecode @v (extend p (toSnakeCase (selName m))) x ws
instance (GDecode v fq fa, GDecode v gq ga) => GDecode v (fq :*: gq) (fa :*: ga) where
  gDecode p (l :*: r) ws = (:*:) <$> gDecode @v p l ws <*> gDecode @v p r ws
instance Endpoint v e => GDecode v (K1 i (Q v e)) (K1 i (A v e)) where
  gDecode p (K1 q) ws = K1 <$> decodeA p q ws

-- ---------------------------------------------------------------------------
-- Alternatives and levels: generic helpers over the second-level records
-- ---------------------------------------------------------------------------

class GAlts v f where
  gAlts :: f x -> [(Text, Description v)]
instance GAlts v f => GAlts v (M1 D d f) where gAlts (M1 x) = gAlts @v x
instance GAlts v f => GAlts v (M1 C c f) where gAlts (M1 x) = gAlts @v x
instance (GAlts v f, GAlts v g) => GAlts v (f :*: g) where gAlts (l :*: r) = gAlts @v l ++ gAlts @v r
instance Selector sel => GAlts v (M1 S sel (K1 i (Q v (Option a)))) where
  gAlts m@(M1 (K1 (OptionQ k d _))) = [(optionKey k m, d)]
instance
  TypeError ('Text "a Choice alternatives record must have at least one Option field")
  => GAlts v U1 where
  gAlts = undefined

optionKey :: Selector sel => Maybe Text -> M1 S sel f x -> Text
optionKey override m = maybe (toSnakeCase (selName m)) id override

class GSelectors f where
  gSelectors :: f x -> [Text]
instance GSelectors f => GSelectors (M1 D d f) where gSelectors (M1 x) = gSelectors x
instance GSelectors f => GSelectors (M1 C c f) where gSelectors (M1 x) = gSelectors x
instance (GSelectors f, GSelectors g) => GSelectors (f :*: g) where gSelectors (l :*: r) = gSelectors l ++ gSelectors r
instance Selector sel => GSelectors (M1 S sel f) where gSelectors m = [T.pack (selName m)]

class GLevels v f where
  gLevels :: f x -> [(Text, v)]
instance GLevels v f => GLevels v (M1 D d f) where gLevels (M1 x) = gLevels @v x
instance GLevels v f => GLevels v (M1 C c f) where gLevels (M1 x) = gLevels @v x
instance (GLevels v f, GLevels v g) => GLevels v (f :*: g) where gLevels (l :*: r) = gLevels @v l ++ gLevels @v r
instance Selector sel => GLevels v (M1 S sel (K1 i (Q v Level))) where
  gLevels m@(M1 (K1 (LevelQ c))) = [(T.pack (selName m), c)]

-- Build a record whose leaves are all one type from a lookup by field name.
class GBuild leaf f where
  gBuild :: (Text -> Either DecodeError leaf) -> Either DecodeError (f x)
instance GBuild leaf f => GBuild leaf (M1 D d f) where gBuild l = M1 <$> gBuild @leaf l
instance GBuild leaf f => GBuild leaf (M1 C c f) where gBuild l = M1 <$> gBuild @leaf l
instance (GBuild leaf f, GBuild leaf g) => GBuild leaf (f :*: g) where
  gBuild l = (:*:) <$> gBuild @leaf l <*> gBuild @leaf l
instance Selector sel => GBuild leaf (M1 S sel (K1 i leaf)) where
  gBuild l = M1 . K1 <$> l (T.pack (selName (M1 Proxy :: M1 S sel Proxy ())))

class GCollect leaf f where
  gCollect :: f x -> [(Text, leaf)]
instance GCollect leaf f => GCollect leaf (M1 D d f) where gCollect (M1 x) = gCollect @leaf x
instance GCollect leaf f => GCollect leaf (M1 C c f) where gCollect (M1 x) = gCollect @leaf x
instance (GCollect leaf f, GCollect leaf g) => GCollect leaf (f :*: g) where
  gCollect (l :*: r) = gCollect @leaf l ++ gCollect @leaf r
instance Selector sel => GCollect leaf (M1 S sel (K1 i leaf)) where
  gCollect m@(M1 (K1 x)) = [(T.pack (selName m), x)]

-- Apply every handler to its alternative's payload, lazily, keyed by wire
-- key. Pairs the Questions representation with the Handlers representation
-- so a wrong payload type or a foreign handler record is a type error.
class GApply v (fq :: Type -> Type) (fh :: Type -> Type) r where
  gApply :: fq x -> fh x -> [(Text, r)]
instance GApply v fq fh r => GApply v (M1 D d fq) (M1 D d fh) r where gApply (M1 q) (M1 h) = gApply @v q h
instance GApply v fq fh r => GApply v (M1 C c fq) (M1 C c fh) r where gApply (M1 q) (M1 h) = gApply @v q h
instance (GApply v fq fh r, GApply v gq gh r) => GApply v (fq :*: gq) (fh :*: gh) r where
  gApply (a :*: b) (c :*: d) = gApply @v a c ++ gApply @v b d
instance Selector sel => GApply v (M1 S sel (K1 i (Q v (Option a)))) (M1 S sel (K1 i (a -> r))) r where
  gApply m@(M1 (K1 (OptionQ k _ payload))) (M1 (K1 h)) = [(optionKey k m, h payload)]

-- ---------------------------------------------------------------------------
-- Root schemas for the tiny and the fully dynamic use
-- ---------------------------------------------------------------------------

-- | One-field schema.
data Only e mode = Only { value :: mode :- e } deriving (Generic)
instance (forall v. JsonValue v => Endpoint v e) => Schema (Only e)

-- | A root-level dynamic map whose keys go on the wire verbatim.
newtype Exact mode = Exact [(Text, ExactLeaf mode)]

type family ExactLeaf mode where
  ExactLeaf (Questions v) = SomeQ v
  ExactLeaf (Answers v) = SomeA v

exact :: [(Text, SomeQ v)] -> Exact (Questions v)
exact = Exact

exactAnswers :: Exact (Answers v) -> [(Text, SomeA v)]
exactAnswers (Exact xs) = xs

instance Schema Exact where
  compileSchema _ (Exact qs) = concat <$> mapM (\(k, SomeQ q) -> compileQ (Exactly k) q) qs
  decodeSchema _ (Exact qs) ws = Exact <$> mapM (\(k, SomeQ q) -> (,) k . SomeA q <$> decodeA (Exactly k) q ws) qs

-- ---------------------------------------------------------------------------
-- The operation: prepare, render, decode
-- ---------------------------------------------------------------------------

newtype Model = Model Text deriving (Eq, Show)

jevLatest :: Model
jevLatest = Model "jev-latest"

-- | A checked request: the retained questions and their wire form.
data Prepared v s = Prepared
  { preparedQuestions :: s (Questions v)
  , preparedWire :: [(Text, WireQuestion v)]
  , preparedModel :: Model
  , preparedState :: State v
  }

prepare :: (JsonValue v, Schema s) => Model -> State v -> s (Questions v) -> Either PrepError (Prepared v s)
prepare model st q = do
  checkState st
  qs <- compileSchema (Segments []) q
  let keys = map fst qs
  if null qs then Left EmptyQuestionMap else Right ()
  case [k | k <- keys, T.null k] of
    _ : _ -> Left (EmptyQuestionKey "")
    [] -> Right ()
  case [k | k <- keys, length (filter (== k) keys) > 1] of
    k : _ -> Left (DuplicateQuestionPath k)
    [] -> Right ()
  Right (Prepared q qs model st)

-- | The request body a transport sends.
requestValue :: JsonValue v => Prepared v s -> v
requestValue (Prepared _ qs (Model m) st) = jObject
  [ ("model", jString m)
  , ("state", stateValue st)
  , ("questions", jObject [(k, questionValue q) | (k, q) <- qs])
  ]

data Response v s = Response
  { answers :: s (Answers v)
  , resolvedModel :: Text
  , usage :: v
  , diagnostics :: [Text]
  }

-- | Decode a response body against this exact request. A label never
-- resolves against anything but the alternatives this request sent.
decodeResponse :: (JsonValue v, Schema s) => Prepared v s -> v -> Either DecodeError (Response v s)
decodeResponse (Prepared q qs _ _) body = parseEnvelope body >>= \case
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

-- | Prepare, send through any transport, decode.
roundTrip
  :: (Monad m, JsonValue v, Schema s)
  => (v -> m (Either Text v)) -> Model -> State v -> s (Questions v)
  -> m (Either JevError (Response v s))
roundTrip transport model st q = case prepare model st q of
  Left e -> pure (Left (Prepare e))
  Right prepared -> transport (requestValue prepared) >>= \case
    Left t -> pure (Left (Transport t))
    Right body -> pure (either (Left . Decode) Right (decodeResponse prepared body))

-- | The tiny use: one question, one answer.
jev1
  :: (Monad m, JsonValue v, Schema (Only e))
  => (v -> m (Either Text v)) -> Model -> State v -> Q v e
  -> m (Either JevError (A v e))
jev1 transport model st q = fmap (fmap (value . answers)) (roundTrip transport model st (Only q))
