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
data Edge = Edge { edgeKey :: Text, edgeText :: Text }
data Line = Line { lineNo :: Int, lineText :: Text }

type Transport = Value -> IO (Either Text Value)

-- The tiny use: one question, one answer, nothing declared.
locate :: Transport -> Text -> [Line] -> IO (Either Text Int)
locate transport source numbered = do
  answer <- ask1 transport jevLatest (state (String source))
    (choice "Which line begins the retry-timeout branch?"
       (alt #not_here "No line in this file begins that branch" () .| many (T.pack . show . (.lineNo)) (String . (.lineText)) numbered))
  pure $ case answer of
    Left err -> Left (T.pack (show err))
    Right a -> case settle routing a (#not_here (\() -> Nothing) .| onMany (\_ l -> Just l.lineNo)) of
      Right (Just n) -> Right n
      Right Nothing -> Left "not in this file"
      Left _ -> Left (explain routing a)

-- A packet: the type is inferred from the questions.
inspection edges =
     #next     := choice "Which available continuation advances the inquiry?"
                    (  alt #use_witness "The current span already answers the inquiry" (Witness "complete_request:41")
                    .| alt #ask_model "Choosing needs a design preference beyond the supplied evidence" (Handoff "preference")
                    .| many (.edgeKey) (String . (.edgeText)) edges )
  :& #enough   := noul "Does the supplied evidence answer the inquiry?"
  :& #children := each (.edgeKey) (\e -> noul ("Is " <> e.edgeKey <> " (" <> e.edgeText <> ") relevant to the inquiry?")) edges
  :& #evidence := (#gap := noul "Does answering require source that was not supplied?" :& Nil)
  :& Nil

-- The same thing, named. Signatures are optional; this one shows what was inferred.
type Routes = "use_witness" ::> Witness :|: "ask_model" ::> Handoff :|: Many Edge
type Inspection = Packet
  '[ "next" ::= Choice Routes
   , "enough" ::= Noul
   , "children" ::= Each Edge Noul
   , "evidence" ::= Group (Packet '[ "gap" ::= Noul ]) ]

_inspectionTyped :: [Edge] -> Inspection Questions
_inspectionTyped = inspection

-- Acting on answers: settle a choice, judge a Noul, explain either.
act :: Inspection Answers -> Text
act a =
  case settle spawning a.next
         (  #use_witness (\(Witness w) -> "located at " <> w)
         .| #ask_model   (\(Handoff h) -> "hand back: " <> h)
         .| onMany       (\k _ -> "follow " <> k) ) of
    Right step -> step <> (if judge routing a.enough == Right True then "; evidence suffices" else "")
    Left _ -> "stopped: " <> explain spawning a.next

-- Reading answers: every answer is a plain record, read by field.
report :: Inspection Answers -> Text
report a =
  a.next.key <> " by " <> pct a.next.margin
    <> ", relevant: " <> T.intercalate ", " [e.edgeKey | (e, n) <- a.children, judge routing n == Right True]
    <> (if judge routing a.evidence.gap == Right True then ", source missing" else "")
  where pct x = T.pack (show (round (x * 100) :: Int)) <> "%"

-- A rubric is graded, not read off: the result is the level, written
-- beside its wording when the rubric was asked.
data Urgency = Background | AtCheckpoint | Now deriving (Show, Eq)

urgency :: Transport -> Text -> IO (Either Text Urgency)
urgency transport situation = do
  answer <- ask1 transport jevLatest (state (String situation))
    (score "What is the consequence of waiting?"
       (  level #background "No current action depends on this" Background
       .| level #checkpoint "Useful at the next ordinary checkpoint" AtCheckpoint
       .| level #blocked "A worker cannot take its next action" Now ))
  pure $ case answer of
    Left err -> Left (T.pack (show err))
    Right a -> Right (grade 0.5 a)

-- The same handlers on every contender above a floor.
routes :: Handlers Text Routes
routes = #use_witness (const "witness") .| #ask_model (const "model") .| onMany (\k _ -> k)

alive :: Inspection Answers -> [Text]
alive a = map snd (contenders 0.25 a.next routes)
