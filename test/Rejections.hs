{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Every captured rejection is either inexpressible in the DSL, rejected at
-- 'prepare' with a named error, or a decision only the provider can make.
module Rejections (rejectionChecks) where

import Check
import Control.Monad (forM_)
import Data.Aeson (Value (..))
import qualified Data.Text as T
import Fixtures
import Jev.Operators

data Disposition
  = Prep PrepError                 -- rejected locally with exactly this error
  | Inexpressible String           -- the DSL has no way to write it
  | ProviderDecided                -- valid locally; the provider decides

st :: State 'Plain
st = stateObject [("message", "m")]

prepErr :: Schema s => s Questions -> Maybe PrepError
prepErr q = either Just (const Nothing) (prepare jevLatest st q)

route :: [(T.Text, Value, ())] -> Packet '["route" ::= Choice (Many ())] Questions
route cs = #route := choice "?" (many cs) :& Nil

count :: [Value] -> Packet '["count" ::= Scale] Questions
count ls = #count := scale (question "?") (levelsOf ls) :& Nil

wake :: Instructions -> Packet '["wake" ::= Noul] Questions
wake i = #wake := noulWith i noCriteria :& Nil

table :: [(String, Disposition, Maybe PrepError)]
table =
  [ ("choice256", Prep (TooManyAlternatives "route" 256), prepErr (route [(T.pack ("owner_" ++ show i), Null, ()) | i <- [0 .. 255 :: Int]]))
  , ("choice-zero", Prep (EmptyOffer "route"), prepErr (route []))
  , ("mixed-valid-invalid-questions", Prep (EmptyOffer "route"), prepErr (route []))
  , ("score-eleven", Prep (BadLevelCount "count" 11), prepErr (count (replicate 11 "l")))
  , ("score-zero", Prep (BadLevelCount "count" 0), prepErr (count []))
  , ("score-null-level", Prep (BadLevel "count" 0), prepErr (count [Null, "l"]))
  , ("question-empty-key", Prep (EmptyQuestionKey ""), prepErr (exact [("", someQ (noul "?"))]))
  , ("empty-questions", Prep EmptyQuestionMap, prepErr (exact []))
  , ("state-null", Prep BadStateShape, either Just (const Nothing) (prepare jevLatest (stateOf Null) (wake (question "?"))))
  , ("state-boolean", Prep BadStateShape, either Just (const Nothing) (prepare jevLatest (stateOf (Bool True)) (wake (question "?"))))
  , ("state-number", Prep BadStateShape, either Just (const Nothing) (prepare jevLatest (stateOf (Number 42)) (wake (question "?"))))
  , ("instructions-boolean", Prep (BadInstructions "wake"), prepErr (wake (Instructions (Bool True))))
  , ("instructions-number", Prep (BadInstructions "wake"), prepErr (wake (Instructions (Number 42))))
  , ("choice-boolean-description", Prep (BadDescription "route" "owner_0"), prepErr (route [("owner_0", Bool True, ()), ("owner_1", Null, ())]))
  , ("choice-number-description", Prep (BadDescription "route" "owner_0"), prepErr (route [("owner_0", Number 42, ()), ("owner_1", Null, ())]))
  , ("questions-null", Inexpressible "prepare takes a packet; the map is never null", Nothing)
  , ("questions-array", Inexpressible "prepare takes a packet; the map is never an array", Nothing)
  , ("questions-omitted", Inexpressible "prepare takes a packet; the map is always present", Nothing)
  , ("model-null", Inexpressible "Model is Text", Nothing)
  , ("model-omitted", Inexpressible "prepare takes a Model", Nothing)
  , ("state-omitted", Inexpressible "prepare takes a State", Nothing)
  , ("question-type-omitted", Inexpressible "every endpoint renders its type; Raw is explicitly outside the guarantee", Nothing)
  , ("question-type-unknown", Inexpressible "the endpoint set is closed; Raw is explicitly outside the guarantee", Nothing)
  , ("question-type-uppercase", Inexpressible "type tags are rendered by the library", Nothing)
  , ("model-unknown", ProviderDecided, Nothing)
  , ("bounding-box-empty", ProviderDecided, Nothing)
  , ("max-tokens-exceeded", ProviderDecided, Nothing)
  ]

rejectionChecks :: Checks -> IO ()
rejectionChecks c = forM_ table $ \(name, disposition, observed) -> do
  fx <- loadFixture name
  check c (name ++ ": fixture is a rejection") (fixtureStatus fx `elem` [400, 422])
  case disposition of
    Prep expected -> checkEq c (name ++ ": rejected at prepare") (Just expected) observed
    Inexpressible why -> check c (name ++ ": inexpressible (" ++ why ++ ")") True
    ProviderDecided -> check c (name ++ ": provider-decided") True
