{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Compilable application sketches, ported from Astra's showcase on the
-- earlier record surface. The caller supplies transport and actual tools;
-- no commands, network calls, or edits run on import.
module AstraShowcase where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Jev.Operators

type Transport = Value -> IO (Either Text Value)

-- 1. A candidate can contain executable Haskell. Only its wording crosses
-- the wire. These observations should be bounded reads of a fixed revision.
data Hypothesis = Hypothesis
  { hypothesisKey :: Text
  , hypothesisDescription :: Value
  , observe :: IO Value
  }

type Explanations = "none" ::> () :|: Many Hypothesis

data Diagnosis
  = Supported Hypothesis [(Text, Value)] (A Value (Choice Explanations))
  | Unresolved [(Text, Value)] (A Value (Choice Explanations))

-- First packet ranks explanations. Read evidence for up to two contenders.
-- Second packet asks what the evidence actually supports, including none.
-- Thresholds are caller policy, not claims about calibrated accuracy.
diagnose
  :: Transport -> Policy -> Double -> Value -> [Hypothesis]
  -> IO (Either JevError Diagnosis)
diagnose transport policy beamFloor world hypotheses = do
  let offers = alt #none "None of these explanations fits the supplied evidence." ()
            .| many [(hypothesisKey h, hypothesisDescription h, h) | h <- hypotheses]
      hypothesisOf s = handle s (#none (\() -> Nothing) .| onMany (\_ h -> Just h))
  initial <- jev1 transport jevLatest (state world) $
    choice "Which explanation most deserves investigation? This is a lead, not a conclusion." offers
  case initial of
    Left err -> pure (Left err)
    Right first -> do
      let live = take 2 [h | (_, s) <- contenders beamFloor first, Just h <- [hypothesisOf s]]
      observations <- traverse (\h -> do
        evidence <- observe h
        pure (hypothesisKey h, evidence)) live
      if null observations
        then pure (Right (Unresolved [] first))
        else do
          final <- jev1 transport jevLatest
            (state (object
              [ "original" .= world
              , "observations" .= object
                  ["results" .= [object ["hypothesis" .= k, "evidence" .= v] | (k, v) <- observations]]
              ])) $
            choice "Which explanation is supported by the observations? Mere plausibility is insufficient." offers
          pure $ fmap (\answer -> case accept policy answer of
            Left _ -> Unresolved observations answer
            Right s -> handle s (#none (\() -> Unresolved observations answer)
                                 .| onMany (\_ h -> Supported h observations answer))) final

-- 2. One heterogeneous branch can return evidence, follow an LSP edge, or run
-- a check. There is no need to flatten these payloads into a universal command.
data Edge = Edge { edgeDescription :: Value, readDestination :: IO Value }
data Check = Check { checkDescription :: Value, runFocusedCheck :: IO Value }
data Witness = Witness { witnessDescription :: Value, witnessRevision :: Text }

type Next = "follow_caller" ::> Edge
        :|: "discriminate" ::> Check
        :|: "retain_witness" ::> Witness
        :|: "ask_author" ::> ()

type Traverse = Packet
  '[ "next_step" ::= Choice Next
   , "evidence_sufficient" ::= Noul
   , "premise_contradicted" ::= Noul ]

data WalkResult
  = ReadResult Value (Traverse Answers)
  | CheckResult Value (Traverse Answers)
  | Found Witness (Traverse Answers)
  | HandBack Value (Traverse Answers)

-- A single semantic boundary of a bounded traversal. The caller can feed a
-- ReadResult into its next iteration, retaining the evidence in notebook scope.
walkStep
  :: Transport -> Policy -> Value -> Edge -> Check -> Witness
  -> IO (Either JevError WalkResult)
walkStep transport policy world edge check witness = do
  response <- roundTrip transport jevLatest (state world)
    (  #next_step := choice "Which continuation best advances the inquiry? Ask the author if the available actions do not cover it."
         (  alt #follow_caller (edgeDescription edge) edge
         .| alt #discriminate (checkDescription check) check
         .| alt #retain_witness (witnessDescription witness) witness
         .| alt #ask_author "Needs new candidates, a design preference, or evidence outside this helper's scope." () )
    :& #evidence_sufficient := noul "Does the supplied source directly establish the requested behavior, including the relevant condition?"
    :& #premise_contradicted := noul "Does the supplied evidence contradict the inquiry's premise?"
    :& Nil )
  case response of
    Left err -> pure (Left err)
    Right envelope -> do
      let a = answers envelope
      result <- if yes a.premise_contradicted > 0.2
        then pure (HandBack world a)
        else case accept policy a.next_step of
          Left _ -> pure (HandBack world a)
          Right s -> handle s
            (  #follow_caller (\e -> ReadResult <$> readDestination e <*> pure a)
            .| #discriminate (\c -> CheckResult <$> runFocusedCheck c <*> pure a)
            .| #retain_witness (\w -> pure (if yes a.evidence_sufficient >= 0.85 then Found w a else HandBack world a))
            .| #ask_author (\() -> pure (HandBack world a)) )
      pure (Right result)

-- 3. Read a whole inbox once, producing reusable judgments for each message.
-- The program computes recommendations; it does not send messages or wake actors.
data Message = Message { messageKey :: Text, messageContext :: Value }

type Attention = Packet
  '[ "invalidates_work" ::= Noul
   , "blocks_next_action" ::= Noul
   , "answered_by_state" ::= Noul ]

data Disposition = WakeNow | NextCheckpoint | CheckRetainedAnswer | AskModel
  deriving (Eq, Show)

triage
  :: Transport -> Value -> [Message]
  -> IO (Either JevError ([(Text, Disposition)], Packet '["attention" ::= Each Attention] Answers))
triage transport world messages = do
  response <- roundTrip transport jevLatest (state world)
    (#attention := each [ (messageKey m, attention m) | m <- messages ] :& Nil)
  pure $ fmap (\envelope ->
    let a = answers envelope
    in ([(k, disposition j) | (k, j) <- a.attention], a)) response
  where
    attention m =
         #invalidates_work := ask m "Does this message invalidate an assumption of the recipient's current action?"
      :& #blocks_next_action := ask m "Does the recipient need this information before taking its next action?"
      :& #answered_by_state := ask m "Is this request already explicitly answered by an applicable retained decision in state?"
      :& Nil
    ask m q = about [("about", messageContext m)] (noul q)
    -- Illustrative policy: uncertain messages return to the model. A likely
    -- existing answer triggers retrieval/verification, not an invented reply.
    disposition :: Attention Answers -> Disposition
    disposition a
      | yes a.invalidates_work >= 0.8 = WakeNow
      | yes a.blocks_next_action >= 0.8 = WakeNow
      | yes a.invalidates_work <= 0.2 && yes a.blocks_next_action <= 0.2 =
          if yes a.answered_by_state >= 0.9 then CheckRetainedAnswer else NextCheckpoint
      | otherwise = AskModel
