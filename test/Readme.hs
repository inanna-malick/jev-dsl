{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
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

-- The tiny use: one question, one answer, nothing declared.
locate :: Transport -> Text -> [(Int, Text)] -> IO (Maybe Int)
locate transport source numbered = do
  answer <- jev1 transport jevLatest (state (String source))
    (choice "Which line begins the retry-timeout branch?"
       (alt #not_here "The branch is not in this file" () .| many [(T.pack (show n), String l, n) | (n, l) <- numbered]))
  pure $ case answer of
    Left _ -> Nothing
    Right a -> handle (chosen a) (#not_here (\() -> Nothing) .| onMany (\_ n -> Just n))

-- A packet: the type is inferred from the questions.
inspection edges =
     #next     := choice "Which available continuation advances the inquiry?"
                    (  alt #use_witness "The current span already answers the inquiry" (Witness "complete_request:41")
                    .| alt #ask_model "Choosing needs a design preference beyond the supplied evidence" (Handoff "preference")
                    .| many edges )
  :& #urgency  := score "What is the consequence of waiting?"
                    (  level #background "No current action depends on this"
                    .| level #checkpoint "Useful at the next ordinary checkpoint"
                    .| level #blocked "A worker cannot take its next action" )
  :& #children := each [ (k, #useful := noul ("Is " <> k <> " relevant to the inquiry?") :& Nil) | (k, _, _) <- edges ]
  :& #evidence := (#enough := noul "Does the supplied evidence answer the inquiry?" :& Nil)
  :& Nil

-- The same thing, named. Signatures are optional; this one shows what was inferred.
type Routes = "use_witness" ::> Witness :|: "ask_model" ::> Handoff :|: Many Edge
type Inspection = Packet
  '[ "next" ::= Choice Routes
   , "urgency" ::= Score ("background" :|: "checkpoint" :|: "blocked")
   , "children" ::= Each (Packet '[ "useful" ::= Noul ])
   , "evidence" ::= Group (Packet '[ "enough" ::= Noul ]) ]

_inspectionTyped :: [(Text, Value, Edge)] -> Inspection Questions
_inspectionTyped = inspection

-- Reading answers: exhaustive handlers, rubric mass, nested access.
act :: Inspection Answers -> Text
act a =
  handle (chosen a.next)
    (  #use_witness (\(Witness w) -> "located at " <> w)
    .| #ask_model   (\(Handoff h) -> "hand back: " <> h)
    .| onMany       (\key _ -> "follow " <> key) )
  <> (if massAtOrAbove #blocked a.urgency > 0.5 then " now" else " later")
  <> (if yes a.evidence.enough > 0.8 then ", evidence suffices" else "")

-- The same handlers on every contender above a floor.
routes :: Handlers Text Routes
routes = #use_witness (const "witness") .| #ask_model (const "model") .| onMany (\key _ -> key)

alive :: Inspection Answers -> [Text]
alive a = [handle s routes | (_, s) <- contenders 0.25 a.next]

-- Under a policy: accept the winner or get structured doubt.
decide :: Inspection Answers -> Either Doubt Text
decide a = fmap (`handle` routes) (accept (Policy { minMass = 0.4, minMargin = 0.15, minConfidence = 0.5 }) a.next)

-- Pools: wording sent once, drawn on by several questions.
probing probes =
     #probes   := probes
  :& #best     := choice "Which probe discriminates best?" (manyFrom probes .| alt #none "No probe discriminates" ())
  :& #per      := eachIn probes (\r -> #useful := askAbout r "Does this probe help answer the inquiry?" :& Nil)
  :& #if_retry := given "the mechanism is retry redelivery" (choice "Which probe confirms it?" (manyFrom probes))
  :& Nil

retryProbes = pool #probes [("run_retry", "Retries m42 and counts callbacks", Command "just test-target actor retry")]

probingRequest = request jevLatest (state "inquiry") (probing retryProbes)
