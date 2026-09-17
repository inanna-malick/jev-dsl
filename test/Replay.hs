{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Capture replay. The wire shapes an author never writes (runtime rubrics,
-- verbatim question ids, raw questions, omitted-versus-null criteria and
-- instruction forms) live here, outside the library, so every recorded
-- exchange still re-renders and decodes through the same core. Nothing in
-- this module is part of the authoring surface.
module Replay
  ( Scale, scale, scaleExpectation, scaleMasses, scaleConfidence, Raw, rawUnchecked, rawAnswer
  , Exact, exact, exactAnswers, SomeQ (..), SomeA (..), someQ
  , noulWith, choiceWith, scoreWith
  , Instructions, Presence (..), Criteria (..), question
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Jev.Aeson ()
import Jev.Core hiding (Answers, Questions)
import qualified Jev.Core
import Jev.Operators (Answers, Questions)

-- ---------------------------------------------------------------------------
-- A score whose levels are runtime values with no labels
-- ---------------------------------------------------------------------------

data Scale
newtype instance Q v Scale = ScaleQ (Instructions v, [v])
data instance A v Scale = Scaled { scaleExpectation :: Double, scaleMasses :: [(v, Double)], scaleConfidence :: Double }

scale :: Instructions Value -> [Value] -> Q Value Scale
scale i ls = ScaleQ (i, ls)

instance JsonValue v => Endpoint v Scale where
  compileQ p (ScaleQ (i, ls)) = do
    let qid = encodePath p
    checkInstructions qid i
    if null ls || length ls > 10 then Left (BadLevelCount qid (length ls)) else Right ()
    mapM_ (\(ix, l) -> checkLevel qid ix l) (zip [0 ..] ls)
    Right (leaf qid (WScore i ls))
  decodeA p (ScaleQ (_, ls)) ws = lookupAnswer p ws >>= \v -> do
    let qid = encodePath p
        indices = [T.pack (show i) | i <- [0 .. length ls - 1]]
    ScoreAnswer e lg ms conf <- parseScore qid v
    distribution qid indices ms conf
    checkLegend qid ls lg
    checkExpectation qid (length ls) e
    built <- mapM (\(i, c) -> maybe (Left (MissingMass qid i)) (Right . (,) c) (lookup i ms)) (zip indices ls)
    Right (Scaled e built conf)
  unwrapA = id
  previewA a = jObject [("score", jNumber (scaleExpectation a)), ("confidence", jNumber (scaleConfidence a))]

-- ---------------------------------------------------------------------------
-- A raw question: any value sent verbatim, the answer's JSON returned as is
-- ---------------------------------------------------------------------------

data Raw
newtype instance Q v Raw = RawQ v
newtype instance A v Raw = RawA v

rawUnchecked :: Value -> Q Value Raw
rawUnchecked = RawQ

rawAnswer :: A Value Raw -> Value
rawAnswer (RawA v) = v

instance JsonValue v => Endpoint v Raw where
  compileQ p (RawQ v) = Right (leaf (encodePath p) (WRaw v))
  decodeA p _ ws = RawA <$> lookupAnswer p ws
  unwrapA = id
  previewA (RawA v) = v

-- ---------------------------------------------------------------------------
-- A root-level map of heterogeneous questions with verbatim ids
-- ---------------------------------------------------------------------------

data SomeQ v = forall e. Endpoint v e => SomeQ (Q v e)
data SomeA v = forall e. Endpoint v e => SomeA (Q v e) (A v e)

someQ :: Endpoint Value e => Q Value e -> SomeQ Value
someQ = SomeQ

newtype Exact mode = Exact [(Text, ExactLeaf mode)]

type family ExactLeaf mode where
  ExactLeaf (Questions' v) = SomeQ v
  ExactLeaf (Answers' v) = SomeA v

type Questions' = Jev.Core.Questions
type Answers' = Jev.Core.Answers

exact :: [(Text, SomeQ Value)] -> Exact Questions
exact = Exact

exactAnswers :: Exact Answers -> [(Text, SomeA Value)]
exactAnswers (Exact xs) = xs

instance JsonValue v => Schema v Exact where
  compileSchema _ (Exact qs) = mconcat <$> mapM (\(k, SomeQ q) -> compileQ (Exactly k) q) qs
  decodeSchema _ (Exact qs) ws = Exact <$> mapM (\(k, SomeQ q) -> (,) k . SomeA q <$> decodeA (Exactly k) q ws) qs
  previewSchema (Exact xs) = jObject [(k, previewAnswer a) | (k, SomeA _ a) <- xs]

-- ---------------------------------------------------------------------------
-- Builders over the raw instruction and criteria positions
-- ---------------------------------------------------------------------------

noulWith :: Instructions Value -> Presence (Maybe (Criteria Value)) -> Q Value Noul
noulWith i c = NoulQ i c []

choiceWith :: Instructions Value -> Alts (Offer Value) alts -> Q Value (Choice alts)
choiceWith = ChoiceQ

scoreWith :: Instructions Value -> Alts (Level Value) levels -> Q Value (Score levels)
scoreWith = ScoreQ
