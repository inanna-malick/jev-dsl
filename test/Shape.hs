{-# LANGUAGE OverloadedStrings #-}
-- | Build an 'Exact' schema from any captured request, so every recorded
-- success can be re-rendered and decoded generically. Questions whose typed
-- form the DSL does not admit (an unknown Noul criteria member, an unknown
-- question type) fall back to 'rawUnchecked', which is what it is for.
module Shape (shapeRequest, Shaped (..), requestExtras) where

import Data.Aeson (Value (..))
import Data.Text (Text)
import Fixtures
import Jev

data Shaped = Shaped
  { shapedModel :: Model
  , shapedState :: State
  , shapedQuestions :: Exact Questions
  , shapedRawCount :: Int
  }

shapeRequest :: Value -> Shaped
shapeRequest req =
  let qs = [(k, shapeQuestion q) | (k, q) <- requestQuestions req]
  in Shaped (Model (requestModel req)) (stateOf (requestState req)) (exact [(k, q) | (k, (q, _)) <- qs]) (length [() | (_, (_, True)) <- qs])

presence :: Text -> Value -> Presence Value
presence k q = maybe Omitted Present (lookup k (objectPairs q))

shapeQuestion :: Value -> (SomeQ, Bool)
shapeQuestion q
  | any (`notElem` ["type", "instructions", "criteria"]) (map fst (objectPairs q)) = raw
  | otherwise = case lookup "type" (objectPairs q) of
  Just (String "noul") -> case lookup "criteria" (objectPairs q) of
    Nothing -> (someQ (noulWith (presence "instructions" q) Omitted), False)
    Just Null -> (someQ (noulWith (presence "instructions" q) (Present Nothing)), False)
    Just c@(Object _)
      | all (`elem` ["true", "false"]) (map fst (objectPairs c)) ->
          (someQ (noulWith (presence "instructions" q) (Present (Just (NoulCriteria (presence "true" c) (presence "false" c))))), False)
    _ -> raw
  Just (String "choice") -> case lookup "criteria" (objectPairs q) of
    Just c@(Object _) -> (someQ (chooseWith (presence "instructions" q) (candidates [(k, d, ()) | (k, d) <- objectPairs c]) []), False)
    _ -> raw
  Just (String "score") -> case lookup "criteria" (objectPairs q) of
    Just (Array ls) -> (someQ (scale (presence "instructions" q) (levelsOf (foldr (:) [] ls))), False)
    _ -> raw
  _ -> raw
  where raw = (someQ (rawUnchecked q), True)

-- | Request-level members beyond model, state, and questions. The request
-- spine is closed, so a capture with extras is inexpressible by design.
requestExtras :: Value -> [Text]
requestExtras req = [k | (k, _) <- objectPairs req, k `notElem` ["model", "state", "questions"]]
