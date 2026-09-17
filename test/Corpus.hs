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
import Jev.Transport (request)
import Proto (field, questionsOf, stub, stubSplit)

-- ---------------------------------------------------------------------------
-- 1. Locate, then edit: a runtime group over numbered lines with a tuple
--    payload, an exit, a policy, exhaustive handlers, and the same handlers
--    on a runner-up.
-- ---------------------------------------------------------------------------

data Edit = EditAt Int Text | HandBack deriving (Show, Eq)

locate transport source numbered revision = do
  r <- ask1 transport jevLatest (state (String source))
    (choice "Which line begins the retry-timeout branch?"
       (alt #not_here "The branch is not in this file" () .| many [(T.pack (show n), String l, (n, revision)) | (n, l) <- numbered]))
  pure $ case r of
    Left _ -> (HandBack, [])
    Right a ->
      let edit = #not_here (\() -> HandBack) .| onMany (\_ (n, rev) -> EditAt n rev)
          winner = either (const HandBack) (`handle` edit) (accept (Policy 0.3 0.1 0.5) a)
          alsoPlausible = [handle s edit | (_, s) <- contenders 0.2 a]
      in (winner, alsoPlausible)

-- ---------------------------------------------------------------------------
-- 2. Diagnose with competing explanations: rank, keep the contenders alive,
--    ask the discriminating follow-up under each premise in the same packet,
--    gather, and ask again over the enriched state with a "neither" exit.
-- ---------------------------------------------------------------------------

data Hypothesis = Hypothesis { hKey :: Text, hText :: Text, probe :: Text } deriving (Show, Eq)
data Diagnosis = Supported Hypothesis | Neither | Undecided [Hypothesis] deriving (Show, Eq)

diagnose transport inquiry hypotheses = do
  let offers = alt #neither "None of these explains the evidence" () .| many [(hKey h, String (hText h), h) | h <- hypotheses]
      hypothesisOf s = handle s (#neither (\() -> Nothing) .| onMany (\_ h -> Just h))
      first = #mechanism := choice "Which mechanism explains the failure?" offers
           :& #probes := each [ (hKey h, #useful := given ("the mechanism is " <> hText h) (noul ("Would running " <> probe h <> " discriminate?")) :& Nil) | h <- hypotheses ]
           :& Nil
  r1 <- ask transport jevLatest (state (String inquiry)) first
  case r1 of
    Left e -> pure (Left e)
    Right resp -> do
      let a = answers resp
          live = [h | (_, s) <- contenders 0.3 a.mechanism, Just h <- [hypothesisOf s]]
          worthProbing = [h | h <- live, Just sub <- [lookup (hKey h) a.probes], yes sub.useful > 0.5]
          observations = [(hKey h, String ("ran " <> probe h)) | h <- worthProbing]
      r2 <- ask1 transport jevLatest (state (object ["inquiry" .= inquiry, "observations" .= object [(Key.fromText k, v) | (k, v) <- observations]]))
              (choice "Which mechanism do the observations support?" offers)
      pure $ fmap (\final -> case accept (Policy 0.6 0.2 0.5) final of
        Left _ -> Undecided live
        Right s -> maybe Neither Supported (hypothesisOf s)) r2

-- ---------------------------------------------------------------------------
-- 3. Membership over a pool: wording sent once, a Noul per entry, and a
--    choice over the same entries with one extra exit.
-- ---------------------------------------------------------------------------

newtype Command = Command Text deriving (Show, Eq)

expand transport inquiry edges = do
  let pooled = pool #edges [(k, String d, Command c) | (k, d, c) <- edges]
      packet = #edges := pooled
            :& #relevant := eachIn pooled (\r -> #applies := askAbout r "Does following this edge bear on the inquiry?" :& Nil)
            :& #next := choice "Which edge should be followed first?" (manyFrom pooled .| alt #stop "No edge is worth following" ())
            :& Nil
  r <- ask transport jevLatest (state (String inquiry)) packet
  pure $ fmap (\resp ->
    let a = answers resp
    in ( [k | (k, sub) <- a.relevant, yes sub.applies > 0.5]
       , handle (chosen a.next) (onMany (\_ c -> Just c) .| #stop (\() -> Nothing)) )) r

-- ---------------------------------------------------------------------------
-- 4. An ordered ladder with a threshold policy.
-- ---------------------------------------------------------------------------

data Disposition = WakeNow | NextCheckpoint | Background deriving (Show, Eq)

attention transport message = do
  r <- ask1 transport jevLatest (state (String message))
    (score "What is the consequence of waiting to act on this message?"
       (  level #background "No current action depends on it"
       .| level #checkpoint "Useful at the next ordinary checkpoint"
       .| level #blocked "A worker cannot take its next action"
       .| level #invalidating "Continuing would invalidate ongoing work" ))
  pure $ fmap (\a ->
    if massAtOrAbove #blocked a >= 0.5 then WakeNow
    else if massAtOrAbove #checkpoint a >= 0.5 then NextCheckpoint
    else Background) r

-- ---------------------------------------------------------------------------
-- 5. Composition: two fragments written independently, each with its own
--    pool, reusing local keys, joined into one packet.
-- ---------------------------------------------------------------------------

buildFragment = (#build_checks := checks :& #build_next := choice "Which build step?" (manyFrom checks) :& Nil, checks)
  where checks = pool #build_checks [("retry", "Retry the build", Command "just build"), ("cancel", "Cancel the build", Command "just cancel")]

mailFragment = (#mailbox_checks := checks :& #mail_next := given "delivery failed once" (choice "Which mailbox step?" (manyFrom checks)) :& Nil, checks)
  where checks = pool #mailbox_checks [("retry", "Retry delivery", Command "just redeliver"), ("cancel", "Drop the message", Command "just drop")]

composed transport = do
  let (buildQ, _) = buildFragment
      (mailQ, _) = mailFragment
      both = buildQ ++. mailQ
  r <- ask transport jevLatest (state "world") both
  pure $ fmap (\resp ->
    let a = answers resp
        cmd s = handle s (onMany (\_ (Command c) -> c))
    in (cmd (chosen a.build_next), cmd (chosen a.mail_next))) r

-- ---------------------------------------------------------------------------

corpusChecks :: Checks -> IO ()
corpusChecks c = do
  -- 1
  let numbered = [(11, "  loop {"), (12, "    if elapsed > timeout {"), (13, "      retry();")]
  (winner, plausible) <- locate (stub "12") "source" numbered ("rev1" :: Text)
  checkEq c "corpus 1: the winner is an edit at the retained line, never text" (EditAt 12 "rev1") winner
  checkEq c "corpus 1: a clear winner has no other contender above the floor" [EditAt 12 "rev1"] plausible
  -- the stub splits 0.45/0.40 when a rival is named; minMargin 0.1 makes that a near tie
  (doubtful, both) <- locate (stubSplit ["12"] "13") "source" numbered ("rev1" :: Text)
  checkEq c "corpus 1: a near tie hands back under the policy" HandBack doubtful
  checkEq c "corpus 1: both contenders still eliminate through the same handlers" [EditAt 12 "rev1", EditAt 13 "rev1"] both
  -- 2
  let hs = [Hypothesis "retry" "retry redelivered the message" "retry-fixture", Hypothesis "admit" "the inbox admitted twice" "admit-fixture", Hypothesis "clock" "the clock skewed" "clock-fixture"]
  d <- diagnose (stubSplit ["retry"] "admit") "why did the callback fire twice?" hs
  checkEq c "corpus 2: two contenders survive, premised follow-ups select probes, the re-ask decides" (Right (Undecided (take 2 hs))) d
  d2 <- diagnose (stub "retry") "why did the callback fire twice?" hs
  checkEq c "corpus 2: a clear winner is supported with its payload" (Right (Supported (Hypothesis "retry" "retry redelivered the message" "retry-fixture"))) d2
  -- 3
  e <- expand (stub "gate") "where is the reply dropped?" [("gate", "gates publication", "sed -n 30,60p gate.rs"), ("telemetry", "records latency", "sed -n 1,20p telemetry.rs")]
  checkEq c "corpus 3: membership per entry and a pooled choice with an exit" (Right (["gate", "telemetry"], Just (Command "sed -n 30,60p gate.rs"))) (fmap (\(ks, cmd) -> (sort ks, cmd)) e)
  -- 4
  att <- attention (stub "") "The build is red on main."
  checkEq c "corpus 4: a ladder threshold decides the disposition" (Right WakeNow) att
  -- 5
  cmds <- composed (stub "retry")
  checkEq c "corpus 5: two fragments with overlapping local keys compose and decode" (Right ("just build", "just redeliver")) cmds
  case request jevLatest (state "world") (fst buildFragment ++. fst mailFragment) of
    Left err -> check c ("corpus 5: " ++ show err) False
    Right req -> checkEq c "corpus 5: each pooled choice names its own pool, inside a premise when it has one" (Just (String "build_checks"), Just (String "mailbox_checks"))
      ( lookup "build_next" (questionsOf req) >>= field "instructions" >>= field "pool"
      , lookup "mail_next" (questionsOf req) >>= field "instructions" >>= field "instructions" >>= field "pool" )
