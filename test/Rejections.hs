{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Every captured rejection is either inexpressible, rejected before the
-- request is built with a named error, or a decision only the provider can
-- make.
module Rejections (rejectionChecks) where

import Check
import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Text as T
import Fixtures
import qualified Jev.Core as Core
import Jev.Operators hiding (alt, level, many, state)
import Replay

data Disposition
  = Prep PrepError                 -- rejected locally with exactly this error
  | Inexpressible String           -- there is no way to write it
  | ProviderDecided                -- valid locally; the provider decides

st :: State ()
st = rawState (object ["message" .= ("m" :: T.Text)])

prepErr :: Schema Value s => s Questions -> Maybe PrepError
prepErr q = case request jevLatest st q of
  Left (Prepare e) -> Just e
  _ -> Nothing

stateErr :: State () -> Maybe PrepError
stateErr s = case request jevLatest s (#wake := noul "?") of
  Left (Prepare e) -> Just e
  _ -> Nothing

route :: [(T.Text, Value)] -> Packet ("route" ::= Choice ("row" ::* (T.Text, Value))) Questions
route cs = #route := choice "?" (many #row fst snd cs)

count :: [Value] -> Packet ("count" ::= Scale) Questions
count ls = #count := scale (question "?") ls

wake :: Instructions Value -> Packet ("wake" ::= Noul) Questions
wake i = #wake := noulWith i Omitted

table :: [(String, Disposition, Maybe PrepError)]
table =
  [ ("choice256", Prep (Core.TooManyAlternatives "route" 256), prepErr (route [(T.pack ("owner_" ++ show i), Null) | i <- [0 .. 255 :: Int]]))
  , ("choice-zero", Prep (Core.EmptyOffer "route"), prepErr (route []))
  , ("mixed-valid-invalid-questions", Prep (Core.EmptyOffer "route"), prepErr (route []))
  , ("score-eleven", Prep (Core.BadLevelCount "count" 11), prepErr (count (replicate 11 "l")))
  , ("score-zero", Prep (Core.BadLevelCount "count" 0), prepErr (count []))
  , ("score-null-level", Prep (Core.BadLevel "count" 0), prepErr (count [Null, "l"]))
  , ("question-empty-key", Prep (Core.EmptyQuestionKey ""), prepErr (exact [("", someQ (noul "?"))]))
  , ("empty-questions", Prep Core.EmptyQuestionMap, prepErr (exact []))
  , ("state-null", Prep Core.BadStateShape, stateErr (rawState Null))
  , ("state-boolean", Prep Core.BadStateShape, stateErr (rawState (Bool True)))
  , ("state-number", Prep Core.BadStateShape, stateErr (rawState (Number 42)))
  , ("instructions-boolean", Prep (Core.BadInstructions "wake"), prepErr (wake (Core.Instructions (Bool True))))
  , ("instructions-number", Prep (Core.BadInstructions "wake"), prepErr (wake (Core.Instructions (Number 42))))
  , ("choice-boolean-description", Prep (Core.BadDescription "route" "owner_0"), prepErr (route [("owner_0", Bool True), ("owner_1", Null)]))
  , ("choice-number-description", Prep (Core.BadDescription "route" "owner_0"), prepErr (route [("owner_0", Number 42), ("owner_1", Null)]))
  , ("questions-null", Inexpressible "request takes a packet; the map is never null", Nothing)
  , ("questions-array", Inexpressible "request takes a packet; the map is never an array", Nothing)
  , ("questions-omitted", Inexpressible "request takes a packet; the map is always present", Nothing)
  , ("model-null", Inexpressible "Model is Text", Nothing)
  , ("model-omitted", Inexpressible "request takes a Model", Nothing)
  , ("state-omitted", Inexpressible "request takes a State", Nothing)
  , ("question-type-omitted", Inexpressible "every endpoint renders its type; Raw is replay-only", Nothing)
  , ("question-type-unknown", Inexpressible "the endpoint set is closed; Raw is replay-only", Nothing)
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
    Prep expected -> checkEq c (name ++ ": rejected before the request is built") (Just expected) observed
    Inexpressible why -> check c (name ++ ": inexpressible (" ++ why ++ ")") True
    ProviderDecided -> check c (name ++ ": provider-decided") True
