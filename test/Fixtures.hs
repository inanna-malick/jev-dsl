{-# LANGUAGE OverloadedStrings #-}
-- | Curated real exchanges under test/fixtures.
module Fixtures (Fixture (..), loadFixture, requestState, requestQuestions, requestModel, objectPairs, keyText) where

import Data.Aeson (Value (..), eitherDecodeFileStrict)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)

data Fixture = Fixture
  { fixtureName :: String
  , fixtureProbe :: Value
  , fixtureStatus :: Int
  , fixtureRequest :: Value
  , fixtureResponse :: Value
  }

loadFixture :: String -> IO Fixture
loadFixture name = do
  v <- either fail pure =<< eitherDecodeFileStrict ("test/fixtures/" ++ name ++ ".json")
  case v of
    Object o -> pure Fixture
      { fixtureName = name
      , fixtureProbe = maybe Null id (KeyMap.lookup "probe" o)
      , fixtureStatus = case KeyMap.lookup "status" o of Just (Number n) -> round n; _ -> 0
      , fixtureRequest = maybe Null id (KeyMap.lookup "request" o)
      , fixtureResponse = maybe Null id (KeyMap.lookup "response" o)
      }
    _ -> fail (name ++ ": fixture is not an object")

keyText :: Key.Key -> Text
keyText = Key.toText

objectPairs :: Value -> [(Text, Value)]
objectPairs (Object o) = [(Key.toText k, v) | (k, v) <- KeyMap.toList o]
objectPairs _ = []

requestState :: Value -> Value
requestState v = maybe Null id (lookup "state" (objectPairs v))

requestModel :: Value -> Text
requestModel v = case lookup "model" (objectPairs v) of
  Just (String m) -> m
  _ -> "jev-latest"

requestQuestions :: Value -> [(Text, Value)]
requestQuestions v = maybe [] objectPairs (lookup "questions" (objectPairs v))
