{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | The acceptance suite: microprograms of the kind the library exists
-- for, each written the way an author would write it in a session, with no
-- signatures beyond the domain types, run against the stub transport with
-- the decoded facts asserted. A feature earns its place here or not at all.
module Corpus (corpusChecks) where

import Check
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Jev.Operators
import Proto (stub, stubSplit)

-- ---------------------------------------------------------------------------
-- 1. Locate, then edit: a runtime group over numbered lines with the row
--    as payload, an exit, a policy, exhaustive handlers, and the same
--    handlers on a runner-up.
-- ---------------------------------------------------------------------------

data Line = Line { lineNo :: Int, lineText :: Text, revision :: Text }
data Edit = EditAt Int Text | HandBack deriving (Show, Eq)

locate transport source numbered = do
  r <- ask1 transport jevLatest (state (String source))
    (choice "Which line begins the retry-timeout branch?"
       (alt #not_here "The branch is not in this file" () .| many (T.pack . show . (.lineNo)) (String . (.lineText)) numbered))
  pure $ case r of
    Left _ -> (HandBack, [])
    Right a ->
      let edit = #not_here (\() -> HandBack) .| onMany (\_ l -> EditAt l.lineNo l.revision)
          winner = either (const HandBack) id (settle (Policy 0.3 0.1 0.5) a edit)
          alsoPlausible = [handle s edit | (_, s) <- contenders 0.2 a]
      in (winner, alsoPlausible)

-- ---------------------------------------------------------------------------
-- 2. Diagnose with competing explanations: rank, keep the contenders alive,
--    ask a discriminating follow-up per hypothesis in the same packet,
--    gather, and ask again over the enriched state with a "neither" exit.
-- ---------------------------------------------------------------------------

data Hypothesis = Hypothesis { hKey :: Text, hText :: Text, probe :: Text } deriving (Show, Eq)
data Diagnosis = Supported Hypothesis | Neither | Undecided [Hypothesis] deriving (Show, Eq)

diagnose transport inquiry hypotheses = do
  let offers = alt #neither "None of these explains the evidence" () .| many (.hKey) (String . (.hText)) hypotheses
      hypothesisOf = #neither (\() -> Nothing) .| onMany (\_ h -> Just h)
      first = #mechanism := choice "Which mechanism explains the failure?" offers
           :& #probes := each [ (h.hKey, #useful := noul ("Supposing the mechanism is " <> h.hText <> ": would running " <> h.probe <> " discriminate?") :& Nil) | h <- hypotheses ]
           :& Nil
  r1 <- ask transport jevLatest (state (String inquiry)) first
  case r1 of
    Left e -> pure (Left e)
    Right resp -> do
      let a = answers resp
          live = [h | (_, s) <- contenders 0.3 a.mechanism, Just h <- [handle s hypothesisOf]]
          worthProbing = [h | h <- live, Just sub <- [lookup h.hKey a.probes], judge routing sub.useful == Right True]
          observations = [(h.hKey, String ("ran " <> h.probe)) | h <- worthProbing]
      r2 <- ask1 transport jevLatest (state (object ["inquiry" .= inquiry, "observations" .= object [(Key.fromText k, v) | (k, v) <- observations]]))
              (choice "Which mechanism do the observations support?" offers)
      pure $ fmap (\final -> either (const (Undecided live)) (maybe Neither Supported) (settle (Policy 0.6 0.2 0.5) final hypothesisOf)) r2

-- ---------------------------------------------------------------------------
-- 3. The per-item battery: a Noul per edge with its own wording, and a
--    choice over the same edges with one extra exit, in one call.
-- ---------------------------------------------------------------------------

data Edge = Edge { edgeKey :: Text, edgeText :: Text, command :: Text }

expand transport inquiry edges = do
  let packet = #relevant := each [ (e.edgeKey, #applies := noul ("Does following " <> e.edgeKey <> " (" <> e.edgeText <> ") bear on the inquiry?") :& Nil) | e <- edges ]
            :& #next := choice "Which edge should be followed first?" (many (.edgeKey) (String . (.edgeText)) edges .| alt #stop "No edge is worth following" ())
            :& Nil
  r <- ask transport jevLatest (state (String inquiry)) packet
  pure $ fmap (\resp ->
    let a = answers resp
    in ( [k | (k, sub) <- a.relevant, judge routing sub.applies == Right True]
       , settle routing a.next (onMany (\_ e -> Just e.command) .| #stop (\() -> Nothing)) )) r

-- ---------------------------------------------------------------------------
-- 4. An ordered ladder with a threshold.
-- ---------------------------------------------------------------------------

data Disposition = WakeNow | NextCheckpoint | Background deriving (Show, Eq)

attention transport message = do
  r <- ask1 transport jevLatest (state (String message))
    (score "What is the consequence of waiting to act on this message?"
       (  level #background "No current action depends on it"
       .| level #checkpoint "Useful at the next ordinary checkpoint"
       .| level #blocked "A worker cannot take its next action"
       .| level #invalidating "Continuing would invalidate ongoing work" ))
  -- One result per level, in level order; the compiler checks all four are
  -- there and in the order the rubric declared them.
  pure $ fmap (\a -> grade 0.5 a
    (  level #background   Background
    .| level #checkpoint   NextCheckpoint
    .| level #blocked      WakeNow
    .| level #invalidating WakeNow )) r

-- ---------------------------------------------------------------------------

corpusChecks :: Checks -> IO ()
corpusChecks c = do
  -- 1
  let numbered = [Line 11 "  loop {" "rev1", Line 12 "    if elapsed > timeout {" "rev1", Line 13 "      retry();" "rev1"]
  (winner, plausible) <- locate (stub "12") "source" numbered
  checkEq c "corpus 1: the winner is an edit at the retained line, never text" (EditAt 12 "rev1") winner
  checkEq c "corpus 1: a clear winner has no other contender above the floor" [EditAt 12 "rev1"] plausible
  -- the stub splits 0.45/0.40 when a rival is named; minMargin 0.1 makes that a near tie
  (doubtful, both) <- locate (stubSplit ["12"] "13") "source" numbered
  checkEq c "corpus 1: a near tie hands back under the policy" HandBack doubtful
  checkEq c "corpus 1: both contenders still eliminate through the same handlers" [EditAt 12 "rev1", EditAt 13 "rev1"] both
  -- 2
  let hs = [Hypothesis "retry" "retry redelivered the message" "retry-fixture", Hypothesis "admit" "the inbox admitted twice" "admit-fixture", Hypothesis "clock" "the clock skewed" "clock-fixture"]
  d <- diagnose (stubSplit ["retry"] "admit") "why did the callback fire twice?" hs
  checkEq c "corpus 2: two contenders survive, per-hypothesis follow-ups select probes, the re-ask decides" (Right (Undecided (take 2 hs))) d
  d2 <- diagnose (stub "retry") "why did the callback fire twice?" hs
  checkEq c "corpus 2: a clear winner is supported with its payload" (Right (Supported (Hypothesis "retry" "retry redelivered the message" "retry-fixture"))) d2
  -- 3
  e <- expand (stub "gate") "where is the reply dropped?" [Edge "gate" "gates publication" "sed -n 30,60p gate.rs", Edge "telemetry" "records latency" "sed -n 1,20p telemetry.rs"]
  checkEq c "corpus 3: a battery per entry and a choice over the same entries with an exit" (Right (["gate", "telemetry"], Right (Just "sed -n 30,60p gate.rs"))) (fmap (\(ks, cmd) -> (sort ks, cmd)) e)
  -- 4
  att <- attention (stub "") "The build is red on main."
  checkEq c "corpus 4: a ladder threshold decides the disposition" (Right WakeNow) att
