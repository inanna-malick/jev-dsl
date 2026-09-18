{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | The README's examples, compiled by check.sh. Top-level signatures are
-- omitted where the README omits them: inference is the point.
module Readme where

import Data.Aeson (Value)
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
  let sess = session transport jevLatest
  answer <- ask1 sess (state (#source := source))
    (choice "Which line begins the retry-timeout branch?"
       (alt #not_here "No line in this file begins that branch" () .| many #lines (T.pack . show . (.lineNo)) (.lineText) numbered))
  pure $ case answer of
    Left err -> Left (T.pack (show err))
    Right a -> case settle lenient a (#not_here (\() -> Nothing) .| #lines (\_ l -> Just l.lineNo)) of
      Right (Settled (Just n)) -> Right n
      Right (Settled Nothing) -> Left "not in this file"
      Left d -> Left d.why

-- A packet: the type is inferred from the questions.
inspection edges =
     #next     := choice "Which available continuation advances the inquiry?"
                    (  alt #use_witness "The current span already answers the inquiry" (Witness "complete_request:41")
                    .| alt #ask_model "Choosing needs a design preference beyond the supplied evidence" (Handoff "preference")
                    .| many #edges (.edgeKey) (.edgeText) edges )
  :& #enough   := noul "Does the supplied evidence answer the inquiry?"
  :& #children := each (.edgeKey) (\e -> noul ("Is " <> e.edgeKey <> " (" <> e.edgeText <> ") relevant to the inquiry?")) edges
  :& #evidence := (#gap := noul "Does answering require source that was not supplied?")

-- The same thing, named. Signatures are optional; this one shows what was inferred.
type Routes = "use_witness" ::> Witness :|: "ask_model" ::> Handoff :|: "edges" ::* Edge
type Inspection = Packet
  (    "next" ::= Choice Routes
   :&  "enough" ::= Noul
   :&  "children" ::= Each Edge Noul
   :&  "evidence" ::= Group (Packet ("gap" ::= Noul)) )

_inspectionTyped :: [Edge] -> Inspection Questions
_inspectionTyped = inspection

-- Acting on answers: settle a choice, judge a Noul, explain either.
act :: Inspection Answers -> Text
act a =
  -- Handlers are found by label, so the order here need not be the order
  -- the alternatives were written in.
  case settle careful a.next
         (  #edges       (\k _ -> "follow " <> k)
         .| #use_witness (\(Witness w) -> "located at " <> w)
         .| #ask_model   (\(Handoff h) -> "hand back: " <> h) ) of
    Right (Settled step) -> step <> (if holds lenient a.enough then "; evidence suffices" else "")
    Left d -> "stopped: " <> d.why

-- Reading answers: every answer is a plain record, read by field.
report :: Inspection Answers -> Text
report a =
  a.next.key <> " by " <> pct a.next.margin
    <> ", relevant: " <> T.intercalate ", " [e.edgeKey | (e, n) <- a.children, holds lenient n]
    <> (if holds lenient a.evidence.gap then ", source missing" else "")
  where pct x = T.pack (show (round (x * 100) :: Int)) <> "%"

-- A rubric is graded, not read off: the result is the level, written
-- beside its wording when the rubric was asked.
data Urgency = Background | AtCheckpoint | Now deriving (Show, Eq)

urgency :: Transport -> Text -> IO (Either Text Urgency)
urgency transport situation = do
  let sess = session transport jevLatest
  answer <- ask1 sess (state (#situation := situation))
    (score "What is the consequence of waiting?"
       (  level #background "No current action depends on this" Background
       .| level #checkpoint "Useful at the next ordinary checkpoint" AtCheckpoint
       .| level #blocked "A worker cannot take its next action" Now ))
  pure $ case answer of
    Left err -> Left (T.pack (show err))
    Right a -> Right (grade 0.5 a)

-- The same handlers on every contender above a floor.
routes :: Handlers Text Routes
routes = #use_witness (const "witness") .| #ask_model (const "model") .| #edges (\k _ -> k)

alive :: Inspection Answers -> [Text]
alive a = map snd (contenders 0.25 a.next routes)

-- Uniform choices already carry every result. The policy adds no handlers.
selectLine sess world numbered = do
  answer <- ask1 sess world (choice "Which line begins the branch?"
    (alt #not_here "No line begins it" Nothing
     .| mapCarried (Just . (.lineNo)) (many #lines (T.pack . show . (.lineNo)) (.lineText) numbered)))
  pure (fmap (takenUnder lenient) answer)

-- Optional leaves and packets keep an inferred shape across presence changes.
withExtra extra = #ready := noul "Ready?" :& #extra := optional (noul <$> extra)
withEvidence extra = #ready := noul "Ready?"
                 :& #evidence := optional ((\q -> #gap := noul q) <$> extra)

gateReference = field (#gate :/ #posters)
  (state (#gate := (#posters := (["wanted"] :: [Text]))))

-- A verdict carries the policy that reached it, so the step that cannot be
-- taken back can demand one and nothing weaker will typecheck.
newtype Patch = Patch Text
newtype Receipt = Receipt Text

merge :: Settled Strict Patch -> IO Receipt
merge (Settled (Patch p)) = pure (Receipt p)

-- The state is written once, keeps its Haskell types, and is what the
-- questions draw their rows and their field names from.
data Diagnostic = Diagnostic { diagnosticKey :: Text, diagnosticText :: Text }
data Check = Check { checkKey :: Text, checkText :: Text }

instance Field Value [Diagnostic] where
  toField ds = toField [(d.diagnosticKey, d.diagnosticText) | d <- ds]
instance Field Value [Check] where
  toField cs = toField [(c.checkKey, c.checkText) | c <- cs]

triage failure diagnosticRows checkRows = (world, questions)
  where
    world = state
      (  #failure := (failure :: Text)
      :& #diagnostics := [Diagnostic k t | (k, t) <- diagnosticRows]
      :& #checks := [Check k t | (k, t) <- checkRows] )
    questions =
         #verify := choice "Which available check most directly verifies a fix?"
                      (many #checks (.checkKey) (.checkText) world.checks
                       .| alt #defer "No listed check is a direct verification" ())
      :& #relevant := each (.checkKey) (\c -> noul ("Does the check `" <> c.checkKey <> "` exercise the code path " <> field #failure world <> " names?")) world.checks
      :& #sufficient := noul ("Do " <> field #diagnostics world <> " alone establish the mechanism of " <> field #failure world <> "?")

-- A shared set of questions is an ordinary value, because packets compose.
common :: Packet ("enough" ::= Noul) Questions
common = #enough := noul "Does the supplied evidence answer the inquiry?"

withCommon :: Packet ("enough" ::= Noul :& "gap" ::= Noul) Questions
withCommon = common :& #gap := noul "Does answering require source that was not supplied?"

-- Sorting free-form input into branches the program wrote: the answer is
-- the dispatch, because each branch carries what to do next.
data Account = Account { accountId :: Text, accountSummary :: Text }

sortReply :: Text -> Text -> [Account] -> Offers ("refund" ::> Text :|: "status" ::> Text :|: "other" ::> Text :|: "accounts" ::* Account)
sortReply refundFlow statusFlow knownAccounts =
     alt #refund "Asks for money back, in any words" refundFlow
  .| alt #status "Asks where an existing order is" statusFlow
  .| alt #other  "Anything the two above do not cover" "hand back"
  .| many #accounts (.accountId) (.accountSummary) knownAccounts
