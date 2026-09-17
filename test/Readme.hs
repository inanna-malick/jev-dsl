{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | The README's examples, compiled by check.sh. Top-level signatures are
-- omitted where the README omits them: inference is the point.
module Readme where

import Data.Aeson (Value (..))
import Data.Text (Text)
import qualified Data.Text as T
import Jev.Operators

newtype Witness = Witness Text
newtype Handoff = Handoff Text
newtype Edge = Edge Text
newtype Command = Command Text

type Transport = Value -> IO (Either Text Value)

locate :: Transport -> Text -> [(Int, Text)] -> IO (Maybe Int)
locate transport source numbered = do
  answer <- jev1 transport jevLatest (stateText source)
    (choice @("not_here" ::> () :? "The branch is not in this file" :|: Many Int)
       "Which line begins the retry-timeout branch?"
       (#not_here () .| many [(T.pack (show n), String l, n) | (n, l) <- numbered]))
  pure $ case answer of
    Left _ -> Nothing
    Right a -> caseOf a (#not_here (\() -> Nothing) .| onMany (Just . elementPayload))

type Routes = "use_witness" ::> Witness :? "The current span already answers the inquiry"
          :|: "ask_model"   ::> Handoff :? "Choosing needs a design preference beyond the supplied evidence"
          :|: Many Edge

type Urgency = '[ "background"   :? "No current action depends on this"
                , "checkpoint"   :? "Useful at the next ordinary checkpoint"
                , "blocked"      :? "A worker cannot take its next action"
                , "invalidating" :? "Continuing would invalidate ongoing work" ]

inspection edges =
     #next     := choice @Routes "Which available continuation advances the inquiry?"
                    (#use_witness (Witness "complete_request:41") .| #ask_model (Handoff "preference") .| many edges)
  :& #urgency  := score @Urgency "What is the consequence of waiting?"
  :& #children := each [ (k, #useful := noul ("Is " <> k <> " relevant to the inquiry?") :& Nil) | (k, _, _) <- edges ]
  :& #evidence := group (#enough := noul "Does the supplied evidence answer the inquiry?" :& Nil)
  :& Nil

type Inspection = Packet
  '[ "next" ::= Choice Routes
   , "urgency" ::= Score Urgency
   , "children" ::= Each (Packet '[ "useful" ::= Noul ])
   , "evidence" ::= Group (Packet '[ "enough" ::= Noul ]) ]

-- The inferred type is the declared one.
_inspectionTyped :: [(Text, Value, Edge)] -> Inspection Questions
_inspectionTyped = inspection

act :: Inspection Answers -> Text
act a =
  caseOf a.next
    (  #use_witness (\(Witness w) -> "located at " <> w)
    .| #ask_model   (\(Handoff h) -> "hand back: " <> h)
    .| onMany       (\e -> "follow " <> elementKey e) )
  <> (if massAtOrAbove #blocked a.urgency > 0.5 then " now" else " later")
  <> (if yesAbove 0.8 a.evidence.enough then ", evidence suffices" else "")

contenders :: Inspection Answers -> [Text]
contenders a = [handle s routes | (mass, s) <- ranked a.next, mass > 0.25]
  where routes = #use_witness (const "witness") .| #ask_model (const "model") .| onMany elementKey

probing probes =
     #probes := probes
  :& #best   := choice @(Many Command :|: "none" ::> () :? "No probe discriminates")
                  "Which probe discriminates best?" (manyFrom probes .| #none ())
  :& #per    := eachIn probes (\r -> #useful := askAbout r "Does this probe help answer the inquiry?" :& Nil)
  :& #if_retry := given "the mechanism is retry redelivery"
                    (choice "Which probe confirms it?" (manyFrom probes))
  :& Nil

retryProbes :: Q Value (PoolDecl "probes" Command)
retryProbes = pool #probes [("run_retry", "Retries m42 and counts callbacks", Command "just test-target actor retry")]

preparedProbing = prepare jevLatest (pooled (stateText "inquiry")) (probing retryProbes)
