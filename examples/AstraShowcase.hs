{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Compilable application sketches over the compositional-operator front,
-- ported from Astra's showcase on the earlier record surface. The caller
-- supplies transport and actual tools; no commands, network calls, or edits
-- run on import.
module AstraShowcase where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Jev.Operators

type Transport = Value -> IO (Either Text Value)

-- 1. A candidate can contain executable Haskell. Only its description crosses
-- the wire. These observations should be bounded reads of a fixed revision.
data Hypothesis = Hypothesis
  { hypothesisKey :: Text
  , hypothesisDescription :: Value
  , observe :: IO Value
  }

type Explanations = "none" ::> () :? "None of these explanations fits the supplied evidence." :|: Many Hypothesis

data Diagnosis
  = Supported Hypothesis [(Text, Value)] (A Value (Choice Explanations))
  | Unresolved [(Text, Value)] (A Value (Choice Explanations))

-- First packet ranks explanations. Read evidence for up to two contenders.
-- Second packet asks what the evidence actually supports, including none.
-- Thresholds are caller policy, not claims about calibrated accuracy.
diagnose
  :: Transport -> Policy -> Double -> State 'Plain -> [Hypothesis]
  -> IO (Either JevError Diagnosis)
diagnose transport policy beamFloor world hypotheses = do
  let offers = #none () .| many [(hypothesisKey h, hypothesisDescription h, h) | h <- hypotheses]
  initial <- jev1 transport jevLatest world $
    choice "Which explanation most deserves investigation? This is a lead, not a conclusion." offers
  case initial of
    Left err -> pure (Left err)
    Right first -> do
      let live = take 2 [h | (mass, s) <- ranked first, mass >= beamFloor, Just h <- [handle s (#none (\() -> Nothing) .| onMany (Just . elementPayload))]]
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
                  ["results" .= [object ["hypothesis" .= k, "evidence" .= v] | (k, v) <- observations]])
              ]) $
            choice "Which explanation is supported by the observations? Mere plausibility is insufficient." offers
          pure $ fmap (\answer -> case accept policy answer of
            Left _ -> Unresolved observations answer
            Right s -> handle s (#none (\() -> Unresolved observations answer)
                                 .| onMany (\e -> Supported (elementPayload e) observations answer))) final

-- 2. One heterogeneous branch can return evidence, follow an LSP edge, or run
-- a check. There is no need to flatten these payloads into a universal command.
data Edge = Edge { edgeDescription :: Value, readDestination :: IO Value }
data Check = Check { checkDescription :: Value, runFocusedCheck :: IO Value }
data Witness = Witness { witnessDescription :: Value, witnessRevision :: Text }

-- Descriptions here are runtime values carried by the payloads, so the
-- alternatives are bare and described at the offer.
type Next = "follow_caller" ::> Edge
        :|: "discriminate" ::> Check
        :|: "retain_witness" ::> Witness
        :|: "ask_author" ::> () :? "Needs new candidates, a design preference, or evidence outside this helper's scope."

type Traverse = Packet
  '[ "next_step" ::= Choice Next
   , "evidence_sufficient" ::= Noul
   , "premise_contradicted" ::= Noul ]

data WalkResult
  = ReadResult Value (Traverse Answers)
  | CheckResult Value (Traverse Answers)
  | Found Witness (Traverse Answers)
  | HandBack (State 'Plain) (Traverse Answers)

-- A single semantic boundary of a bounded traversal. The caller can feed a
-- ReadResult into its next iteration, retaining the evidence in notebook scope.
walkStep
  :: Transport -> Policy -> State 'Plain -> Edge -> Check -> Witness
  -> IO (Either JevError WalkResult)
walkStep transport policy world edge check witness = do
  response <- roundTrip transport jevLatest world
    (  #next_step := choice "Which continuation best advances the inquiry? Ask the author if the available actions do not cover it."
         (  #follow_caller (edgeDescription edge, edge)
         .| #discriminate (checkDescription check, check)
         .| #retain_witness (witnessDescription witness, witness)
         .| #ask_author () )
    :& #evidence_sufficient := noul "Does the supplied source directly establish the requested behavior, including the relevant condition?"
    :& #premise_contradicted := noul "Does the supplied evidence contradict the inquiry's premise?"
    :& Nil )
  case response of
    Left err -> pure (Left err)
    Right envelope -> do
      let a = answers envelope
      result <- if not (noBelow 0.2 a.premise_contradicted)
        then pure (HandBack world a)
        else acceptOr (\_ -> pure (HandBack world a)) policy a.next_step
          (  #follow_caller (\e -> ReadResult <$> readDestination e <*> pure a)
          .| #discriminate (\c -> CheckResult <$> runFocusedCheck c <*> pure a)
          .| #retain_witness (\w -> pure (if yesAbove 0.85 a.evidence_sufficient then Found w a else HandBack world a))
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
  :: Transport -> State 'Plain -> [Message]
  -> IO (Either JevError ([(Text, Disposition)], Packet '["attention" ::= Each Attention] Answers))
triage transport world messages = do
  response <- roundTrip transport jevLatest world
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
    ask m q = noulAbout q (messageContext m)
    -- Illustrative policy: uncertain messages return to the model. A likely
    -- existing answer triggers retrieval/verification, not an invented reply.
    disposition :: Attention Answers -> Disposition
    disposition a
      | yesAbove 0.8 a.invalidates_work = WakeNow
      | yesAbove 0.8 a.blocks_next_action = WakeNow
      | noBelow 0.2 a.invalidates_work && noBelow 0.2 a.blocks_next_action =
          if yesAbove 0.9 a.answered_by_state then CheckRetainedAnswer else NextCheckpoint
      | otherwise = AskModel
