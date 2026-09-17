{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- | Build an 'Exact' schema from any captured request, so every recorded
-- success can be re-rendered and decoded generically. Questions whose typed
-- form the DSL does not admit (an unknown Noul criteria member, an unknown
-- question type) fall back to 'rawUnchecked', which is what it is for.
module Shape (shapeRequest, Shaped (..), requestExtras, instructionsOf) where

import Data.Aeson (Value (..))
import Data.Text (Text)
import Fixtures
import Jev.Operators

data Shaped = Shaped
  { shapedModel :: Model
  , shapedState :: State 'Plain
  , shapedQuestions :: Exact Questions
  , shapedRawCount :: Int
  }

shapeRequest :: Value -> Shaped
shapeRequest req =
  let qs = [(k, shapeQuestion q) | (k, q) <- requestQuestions req]
  in Shaped (Model (requestModel req)) (stateOf (requestState req)) (exact [(k, q) | (k, (q, _)) <- qs]) (length [() | (_, (_, True)) <- qs])

presence :: Text -> Value -> Presence Value
presence k q = maybe Omitted Present (lookup k (objectPairs q))

instructionsOf :: Value -> Instructions
instructionsOf q = case lookup "instructions" (objectPairs q) of
  Nothing -> NoInstructions
  Just v -> Instructions v

shapeQuestion :: Value -> (SomeQ, Bool)
shapeQuestion q
  | any (`notElem` ["type", "instructions", "criteria"]) (map fst (objectPairs q)) = raw
  | otherwise = case lookup "type" (objectPairs q) of
  Just (String "noul") -> case lookup "criteria" (objectPairs q) of
    Nothing -> (someQ (noulWith (instructionsOf q) Omitted), False)
    Just Null -> (someQ (noulWith (instructionsOf q) (Present Nothing)), False)
    Just c@(Object _)
      | all (`elem` ["true", "false"]) (map fst (objectPairs c)) ->
          (someQ (noulWith (instructionsOf q) (Present (Just (Criteria (presence "true" c) (presence "false" c))))), False)
    _ -> raw
  Just (String "choice") -> case lookup "criteria" (objectPairs q) of
    Just c@(Object _) -> (someQ (choiceWith @(Many ()) (instructionsOf q) (many [(k, d, ()) | (k, d) <- objectPairs c])), False)
    _ -> raw
  Just (String "score") -> case lookup "criteria" (objectPairs q) of
    Just (Array ls) -> (someQ (scale (instructionsOf q) (levelsOf (foldr (:) [] ls))), False)
    _ -> raw
  _ -> raw
  where raw = (someQ (rawUnchecked q), True)

-- | Request-level members beyond model, state, and questions. The request
-- spine is closed, so a capture with extras is inexpressible by design.
requestExtras :: Value -> [Text]
requestExtras req = [k | (k, _) <- objectPairs req, k `notElem` ["model", "state", "questions"]]
