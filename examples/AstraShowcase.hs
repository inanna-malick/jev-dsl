{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}

-- | Compilable application sketches over the public facade. The caller supplies
-- transport and actual tools; no commands, network calls, or edits run on import.
module AstraShowcase where

import Data.Aeson (Value, object, (.=))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import GHC.Generics (Generic)
import Jev

type Transport = Value -> IO (Either Text Value)

-- 1. A candidate can contain executable Haskell. Only its description crosses
-- the wire. These observations should be bounded reads of a fixed revision.
data Hypothesis = Hypothesis
  { hypothesisKey :: Text
  , hypothesisDescription :: Value
  , observe :: IO Value
  }

data Diagnosis
  = Supported Hypothesis [(Text, Value)] (A (Choose Hypothesis))
  | Unresolved [(Text, Value)] (A (Choose Hypothesis))

-- First packet ranks explanations. Read evidence for up to two contenders.
-- Second packet asks what the evidence actually supports, including neither.
-- Thresholds are caller policy, not claims about calibrated accuracy.
diagnose
  :: Transport -> Policy -> Double -> State -> [Hypothesis]
  -> IO (Either JevError Diagnosis)
diagnose transport policy beamFloor world hypotheses = do
  let pool = candidates
        [(hypothesisKey h, hypothesisDescription h, h) | h <- hypotheses]
      exits = [noMatch "None of these explanations fits the supplied evidence."]
  initial <- jev1 transport jevLatest world $
    choose "Which explanation most deserves investigation? This is a lead, not a conclusion."
      pool exits
  case initial of
    Left err -> pure (Left err)
    Right first -> case picked first of
      PickedExit _ -> pure (Right (Unresolved [] first))
      PickedCandidate _ -> do
        let live = take 2
              [candidatePayload c
              | (mass, PickedCandidate c) <- NE.toList (contenders first)
              , mass >= beamFloor]
        observations <- traverse (\h -> do
          evidence <- observe h
          pure (hypothesisKey h, evidence)) live
        if null observations
          then pure (Right (Unresolved [] first))
          else do
            final <- jev1 transport jevLatest
              (stateObject
                [ ("original", stateValue world)
                , ("observations", object
                    ["results" .= [object ["hypothesis" .= k, "evidence" .= v]
                                  | (k, v) <- observations]])
                ]) $
              choose "Which explanation is supported by the observations? Mere plausibility is insufficient."
                pool exits
            pure $ fmap (\answer -> case select policy answer of
              Left _ -> Unresolved observations answer
              Right c -> Supported (candidatePayload c) observations answer) final

-- 2. One heterogeneous branch can return evidence, follow an LSP edge, or run
-- a check. There is no need to flatten these payloads into a universal command.
data Edge = Edge
  { edgeDescription :: Value
  , readDestination :: IO Value
  }
data Check = Check
  { checkDescription :: Value
  , runFocusedCheck :: IO Value
  }
data Witness = Witness
  { witnessDescription :: Value
  , witnessRevision :: Text
  }

data Next mode = Next
  { followCaller :: mode :- Option Edge
  , discriminate :: mode :- Option Check
  , retainWitness :: mode :- Option Witness
  , askAuthor :: mode :- Option ()
  } deriving Generic

data Traverse mode = Traverse
  { nextStep :: mode :- Choice Next
  , evidenceSufficient :: mode :- Noul
  , premiseContradicted :: mode :- Noul
  } deriving Generic
instance Schema Traverse

data WalkResult
  = ReadResult Value (Traverse Answers)
  | CheckResult Value (Traverse Answers)
  | Found Witness (Traverse Answers)
  | HandBack State (Traverse Answers)

-- A single semantic boundary of a bounded traversal. The caller can feed a
-- ReadResult into its next iteration, retaining the evidence in notebook scope.
walkStep
  :: Transport -> Double -> Double -> State -> Edge -> Check -> Witness
  -> IO (Either JevError WalkResult)
walkStep transport minimumMass minimumGap world edge check witness = do
  response <- roundTrip transport jevLatest world Traverse
    { nextStep = choice
        "Which continuation best advances the inquiry? Ask the author if the available actions do not cover it."
        Next
          { followCaller = optionWith (edgeDescription edge) edge
          , discriminate = optionWith (checkDescription check) check
          , retainWitness = optionWith (witnessDescription witness) witness
          , askAuthor = option "Needs new candidates, a design preference, or evidence outside this helper's scope." ()
          }
    , evidenceSufficient = noul "Does the supplied source directly establish the requested behavior, including the relevant condition?"
    , premiseContradicted = noul "Does the supplied evidence contradict the inquiry's premise?"
    }
  case response of
    Left err -> pure (Left err)
    Right envelope -> do
      let a = answers envelope
          m = masses (nextStep a)
          probabilities = [followCaller m, discriminate m, retainWitness m, askAuthor m]
          winner = withChoice (nextStep a) probabilityOf
          competitors = removeFirst winner probabilities
          decisive = winner >= minimumMass
            && all (\p -> winner - p >= minimumGap) competitors
      result <- if not decisive || not (noBelow 0.2 (premiseContradicted a))
        then pure (HandBack world a)
        else match (nextStep a) Next
          { followCaller = \e -> do
              v <- readDestination e
              pure (ReadResult v a)
          , discriminate = \c -> do
              v <- runFocusedCheck c
              pure (CheckResult v a)
          , retainWitness = \w -> pure $
              if yesAbove 0.85 (evidenceSufficient a)
              then Found w a else HandBack world a
          , askAuthor = \() -> pure (HandBack world a)
          }
      pure (Right result)

-- Remove one occurrence, preserving a tied alternative as a competitor.
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst target (x : xs)
  | target == x = xs
  | otherwise = x : removeFirst target xs

-- 3. Read a whole inbox once, producing reusable judgments for each message.
-- The program computes recommendations; it does not send messages or wake actors.
data Message = Message
  { messageKey :: Text
  , messageContext :: Value
  }
data Attention mode = Attention
  { invalidatesWork :: mode :- Noul
  , blocksNextAction :: mode :- Noul
  , answeredByState :: mode :- Noul
  } deriving Generic
instance Schema Attention

data Inbox mode = Inbox
  { attention :: mode :- Each Attention
  } deriving Generic
instance Schema Inbox

data Disposition = WakeNow | NextCheckpoint | CheckRetainedAnswer | AskModel
  deriving (Eq, Show)

triage
  :: Transport -> State -> [Message]
  -> IO (Either JevError ([(Text, Disposition)], Inbox Answers))
triage transport world messages = do
  response <- roundTrip transport jevLatest world Inbox
    { attention = each [(messageKey m, m) | m <- messages] $ \m -> Attention
        { invalidatesWork = ask m "Does this message invalidate an assumption of the recipient's current action?"
        , blocksNextAction = ask m "Does the recipient need this information before taking its next action?"
        , answeredByState = ask m "Is this request already explicitly answered by an applicable retained decision in state?"
        }
    }
  pure $ fmap (\envelope ->
    let a = answers envelope
    in ([(k, disposition j) | (k, j) <- eachAnswers (attention a)], a)) response
  where
    ask m question = noulWith (Present (object
      ["question" .= (question :: Text), "message" .= messageContext m])) Omitted
    -- Illustrative policy: uncertain messages return to the model. A likely
    -- existing answer triggers retrieval/verification, not an invented reply.
    disposition a
      | yesAbove 0.8 (invalidatesWork a) = WakeNow
      | yesAbove 0.8 (blocksNextAction a) = WakeNow
      | noBelow 0.2 (invalidatesWork a) && noBelow 0.2 (blocksNextAction a) =
          if yesAbove 0.9 (answeredByState a)
          then CheckRetainedAnswer else NextCheckpoint
      | otherwise = AskModel
