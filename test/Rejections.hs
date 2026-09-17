{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Every captured rejection is either inexpressible in the DSL, rejected at
-- 'prepare' with a named error, or a decision only the provider can make.
module Rejections (rejectionChecks) where

import Check
import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Fixtures
import GHC.Generics (Generic)
import Jev

data Disposition
  = Prep PrepError                 -- rejected locally with exactly this error
  | Inexpressible String           -- the DSL has no way to write it
  | CompileFail FilePath           -- the static form is a compile error (see test/reject)
  | ProviderDecided                -- valid locally; the provider decides

data RouteQ mode = RouteQ { route :: mode :- Choose () } deriving (Generic)
instance Schema RouteQ

data Wake mode = Wake { wake :: mode :- Noul } deriving (Generic)
instance Schema Wake

data CountQ mode = CountQ { count :: mode :- Scale } deriving (Generic)
instance Schema CountQ

st :: State
st = stateObject [("message", "m")]

prepErr :: Schema s => s Questions -> Maybe PrepError
prepErr q = either Just (const Nothing) (prepare jevLatest st q)

table :: [(String, Disposition, Maybe PrepError)]
table =
  [ ("choice256", Prep (TooManyAlternatives "route" 256), prepErr (RouteQ (choose "?" (candidates [(T.pack ("owner_" ++ show i), Null, ()) | i <- [0 .. 255 :: Int]]) [])))
  , ("choice-zero", Prep (EmptyCandidates "route"), prepErr (RouteQ (choose "?" (candidates []) [])))
  , ("mixed-valid-invalid-questions", Prep (EmptyCandidates "route"), prepErr (RouteQ (choose "?" (candidates []) [])))
  , ("score-eleven", Prep (BadLevelCount "count" 11), prepErr (CountQ (scale (Present "?") (levelsOf (replicate 11 "l")))))
  , ("score-zero", Prep (BadLevelCount "count" 0), prepErr (CountQ (scale (Present "?") (levelsOf []))))
  , ("score-null-level", Prep (BadLevel "count" 0), prepErr (CountQ (scale (Present "?") (levelsOf [Null, "l"]))))
  , ("question-empty-key", Prep (EmptyQuestionKey ""), prepErr (exact [("", someQ (noul "?"))]))
  , ("empty-questions", Prep EmptyQuestionMap, prepErr (exact []))
  , ("state-null", Prep BadStateShape, either Just (const Nothing) (prepare jevLatest (stateOf Null) (Wake (noul "?"))))
  , ("state-boolean", Prep BadStateShape, either Just (const Nothing) (prepare jevLatest (stateOf (Bool True)) (Wake (noul "?"))))
  , ("state-number", Prep BadStateShape, either Just (const Nothing) (prepare jevLatest (stateOf (Number 42)) (Wake (noul "?"))))
  , ("instructions-boolean", Prep (BadInstructions "wake"), prepErr (Wake (noulWith (Present (Bool True)) Omitted)))
  , ("instructions-number", Prep (BadInstructions "wake"), prepErr (Wake (noulWith (Present (Number 42)) Omitted)))
  , ("choice-boolean-description", Prep (BadDescription "route" "owner_0"), prepErr (RouteQ (choose "?" (candidates [("owner_0", Bool True, ()), ("owner_1", Null, ())]) [])))
  , ("choice-number-description", Prep (BadDescription "route" "owner_0"), prepErr (RouteQ (choose "?" (candidates [("owner_0", Number 42, ()), ("owner_1", Null, ())]) [])))
  , ("questions-null", Inexpressible "prepare takes a schema record; the map is never null", Nothing)
  , ("questions-array", Inexpressible "prepare takes a schema record; the map is never an array", Nothing)
  , ("questions-omitted", Inexpressible "prepare takes a schema record; the map is always present", Nothing)
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
  where
    _ = object ["unused" .= ("" :: Text)]

rejectionChecks :: Checks -> IO ()
rejectionChecks c = forM_ table $ \(name, disposition, observed) -> do
  fx <- loadFixture name
  check c (name ++ ": fixture is a rejection") (fixtureStatus fx `elem` [400, 422])
  case disposition of
    Prep expected -> checkEq c (name ++ ": rejected at prepare") (Just expected) observed
    Inexpressible why -> check c (name ++ ": inexpressible (" ++ why ++ ")") True
    CompileFail path -> check c (name ++ ": compile-fail fixture " ++ path) True
    ProviderDecided -> check c (name ++ ": provider-decided") True
