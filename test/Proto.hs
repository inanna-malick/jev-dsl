{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}

-- | The prototype's assertions, through the public facade only. Records
-- never mention a JSON type; the stub transport speaks aeson Values.
module Proto (protoChecks) where

import Check
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Jev

newtype Command = Command Text deriving (Show, Eq)
newtype Edge = Edge Text deriving (Show)
newtype Witness = Witness Text deriving (Show)
newtype Handoff = Handoff Text deriving (Show)

data Routes mode = Routes
  { followCaller :: mode :- Option Edge
  , useWitness :: mode :- Option Witness
  , noUsefulPath :: mode :- Option ()
  , askModel :: mode :- Option Handoff
  } deriving (Generic)

data Urgency mode = Urgency
  { background :: mode :- Level
  , checkpoint :: mode :- Level
  , blocked :: mode :- Level
  , invalidating :: mode :- Level
  } deriving (Generic)

data Relevance mode = Relevance
  { useful :: mode :- Noul
  , contradicts :: mode :- Noul
  } deriving (Generic)
instance Schema Relevance

data Sufficiency mode = Sufficiency
  { enough :: mode :- Noul
  , gapRemains :: mode :- Noul
  } deriving (Generic)
instance Schema Sufficiency

data Inspect mode = Inspect
  { next :: mode :- Choice Routes
  , probe :: mode :- Choose Command
  , urgency :: mode :- Score Urgency
  , children :: mode :- Each Relevance
  , evidence :: mode :- Group Sufficiency
  } deriving (Generic)
instance Schema Inspect

data Mechanisms mode = Mechanisms
  { retryRedelivery :: mode :- Option Command
  , doubleAdmission :: mode :- Option Command
  , unknownMechanism :: mode :- Option Command
  } deriving (Generic)

data Investigation mode = Investigation
  { mechanism :: mode :- Choice Mechanisms
  , checkIfRetry :: mode :- Choose Command
  , risk :: mode :- Scale
  , extras :: mode :- Many
  } deriving (Generic)
instance Schema Investigation

inspection :: Candidates Command -> Inspect Questions
inspection probes = Inspect
  { next = choice "Which available continuation advances the inquiry?" Routes
      { followCaller = option "Inspect publish_if_active, which gates publication on cancellation" (Edge "publish_if_active")
      , useWitness = option "The current span already answers the inquiry" (Witness "complete_request:41")
      , noUsefulPath = option "No supplied continuation is useful" ()
      , askModel = option "Choosing needs a design preference beyond the supplied evidence" (Handoff "preference")
      }
  , probe = choose "Which focused query best discriminates the remaining mechanisms?" probes
      [deferToModel "Discriminating needs evidence outside the supplied state"]
  , urgency = score "What is the consequence of waiting?" Urgency
      { background = level "No current action depends on this"
      , checkpoint = level "Useful at the next ordinary checkpoint"
      , blocked = level "A worker cannot take its next action"
      , invalidating = level "Continuing would invalidate ongoing work"
      }
  , children = each [("e1", "telemetry"), ("e.2", "publication")] $ \name -> Relevance
      { useful = noul ("Is child " <> name <> " relevant to the inquiry?")
      , contradicts = noul ("Does child " <> name <> " contradict the premise?")
      }
  , evidence = group Sufficiency
      { enough = noul "Does the supplied evidence answer the inquiry?"
      , gapRemains = noul "Does answering require source not supplied?"
      }
  }

-- ---------------------------------------------------------------------------
-- Stub transport over aeson Values
-- ---------------------------------------------------------------------------

questionsOf :: Value -> [(Text, Value)]
questionsOf v = case v of
  Object o | Just (Object qs) <- KeyMap.lookup "questions" o -> [(Key.toText k, q) | (k, q) <- KeyMap.toList qs]
  _ -> []

field :: Text -> Value -> Maybe Value
field k (Object o) = KeyMap.lookup (Key.fromText k) o
field _ _ = Nothing

stub :: Text -> Value -> IO (Either Text Value)
stub preferred = stubSplit [preferred, "run_retry_fixture"] ""

stubSplit :: [Text] -> Text -> Value -> IO (Either Text Value)
stubSplit preferences rival req = pure (Right (object
  [ "model" .= ("stub-1.0" :: Text)
  , "usage" .= object ["input_tokens" .= (100 :: Int), "output_tokens" .= (7 :: Int)]
  , "answers" .= object [Key.fromText k .= answer q | (k, q) <- questionsOf req]
  ]))
  where
    answer q = case field "type" q of
      Just (String "noul") -> object ["type" .= ("noul" :: Text), "noul" .= (0.8 :: Double)]
      Just (String "choice") ->
        let keys = case field "criteria" q of
              Just (Object cs) -> map Key.toText (KeyMap.keys cs)
              _ -> []
            sel = case filter (`elem` keys) preferences of
              k : _ -> k
              [] -> T.concat (take 1 keys)
            others = filter (/= sel) keys
            ms :: [(Text, Double)]
            ms | rival `elem` others =
                   (sel, 0.45) : (rival, 0.4) : [(x, 0.15 / fromIntegral (max 1 (length others - 1))) | x <- others, x /= rival]
               | otherwise = (sel, 0.7) : [(x, 0.3 / fromIntegral (max 1 (length others))) | x <- others]
        in object ["type" .= ("choice" :: Text), "choice" .= sel, "confidence" .= (0.7 :: Double)
                  , "probabilities" .= object [Key.fromText k .= p | (k, p) <- ms]]
      Just (String "score") ->
        let ls = case field "criteria" q of
              Just (Array xs) -> V.toList xs
              _ -> []
            n = length ls
        in object ["type" .= ("score" :: Text), "score" .= (fromIntegral (n - 1) / 2 :: Double), "confidence" .= (0.5 :: Double)
                  , "probabilities" .= object [Key.fromText (T.pack (show i)) .= (1 / fromIntegral n :: Double) | i <- [0 .. n - 1]]
                  , "legend" .= object [Key.fromText (T.pack (show i)) .= c | (i, c) <- zip [0 :: Int ..] ls]]
      _ -> object ["type" .= ("noul" :: Text), "noul" .= (0.5 :: Double)]

fixed :: Value -> Value -> IO (Either Text Value)
fixed resp _ = pure (Right resp)

answerMap :: [(Text, Value)] -> Value
answerMap kv = object ["model" .= ("stub" :: Text), "answers" .= object [Key.fromText k .= v | (k, v) <- kv]]

-- ---------------------------------------------------------------------------

protoChecks :: Checks -> IO ()
protoChecks c = do
  let world = stateObject [("inquiry", "Where can cancellation drop a computed reply?")]
      groups = candidates
        [ ("run_retry_fixture", "Retries m42 and counts callbacks", Command "just test-target actor retry")
        , ("read_publish_gate", "Reads publish_if_active", Command "sed -n 30,60p session/supervisor.rs")
        ]

  -- tiny use
  r1 <- jev1 (stub "read_publish_gate") jevLatest world (choose "Which command next?" groups [deferToModel "Needs a preference"])
  case r1 of
    Left e -> check c ("tiny: " ++ show e) False
    Right a -> do
      out <- pickOr (\e -> pure ("handback: " ++ T.unpack (exitKey e))) a (\(Command cmd) -> pure ("run: " ++ T.unpack cmd))
      checkEq c "tiny: picked the retained command" "run: sed -n 30,60p session/supervisor.rs" out
      checkEq c "tiny: ranked keeps every candidate and exit" 3 (length (ranked a))
  r1x <- jev1 (stub "defer_to_model") jevLatest world (choose "Which command next?" groups [deferToModel "Needs a preference"])
  case r1x of
    Right a -> do
      out <- pickOr (\e -> pure ("handback: " ++ T.unpack (exitKey e))) a (\(Command cmd) -> pure ("run: " ++ T.unpack cmd))
      checkEq c "tiny: library exit hands back" "handback: defer_to_model" out
      checkEq c "ranked: winning exit is first" ["defer_to_model"] (map fst (take 1 (ranked a)))
    Left e -> check c ("tiny exit: " ++ show e) False
  let eitherGroups = candidates
        [ ("read_publish_gate", "Reads publish_if_active", Right (Command "sed -n 30,60p session/supervisor.rs"))
        , ("ask_model", "Choosing needs a design preference", Left (Handoff "preference"))
        ]
  r1e <- jev1 (stub "ask_model") jevLatest world (choose "Which command next?" eitherGroups [])
  case r1e of
    Right a -> do
      out <- pickOr (\_ -> pure "unreachable") a $ \case
        Right (Command cmd) -> pure ("run: " ++ T.unpack cmd)
        Left (Handoff w) -> pure ("handback: " ++ T.unpack w)
      checkEq c "tiny: Either payload hands back without a library exit" "handback: preference" out
    Left e -> check c ("tiny either: " ++ show e) False

  -- refinements: exit-only choices, typed contenders, policy-aware selection
  r1o <- jev1 (stub "no_match") jevLatest world (choose "Any diagnostic?" (candidates ([] :: [(Text, Description, Command)])) [noMatch "Nothing listed"])
  check c "choose: exit-only choice prepares and hands back" (case r1o of
    Right a -> case picked a of PickedExit e -> exitKey e == "no_match"; _ -> False
    Left _ -> False)
  r1c <- jev1 (stubSplit ["run_retry_fixture"] "read_publish_gate") jevLatest world (choose "?" groups [deferToModel "d"])
  case r1c of
    Right a -> do
      let top = NE.toList (contenders a)
      check c "contenders: typed, ranked, includes exits" (length top == 3 && case top of
        (p1, PickedCandidate c1) : (p2, PickedCandidate c2) : (_, PickedExit _) : _ ->
          p1 >= p2 && candidateKey c1 == "run_retry_fixture" && candidateKey c2 == "read_publish_gate"
        _ -> False)
      check c "select: near-tie is structured doubt" (case select (Policy 0 0.1 0) a of
        Left (NearTie (w, _) (r, _)) -> w == "run_retry_fixture" && r == "read_publish_gate"
        _ -> False)
      check c "select: lenient accepts the winner" (case select lenient a of
        Right cand -> candidateKey cand == "run_retry_fixture"
        Left _ -> False)
      check c "select: confidence floor" (case select (Policy 0 0 0.9) a of Left (Unconfident _) -> True; _ -> False)
      out <- selectOr (\_ -> pure "doubt") (Policy 0.6 0 0) a (\(Command cmd) -> pure (T.unpack cmd))
      checkEq c "selectOr: underweight winner hands back" "doubt" out
    Left e -> check c ("contenders: " ++ show e) False
  -- exact numeric equality in legends: two distinct large integers must not collide
  let big n = object ["id" .= Number n]
      bigRubric = levelsOf [big 9007199254740992, big 9007199254740993]
      forged _ = pure (Right (answerMap [("value", object ["type" .= ("score" :: Text), "score" .= (0.5 :: Double), "confidence" .= (0.5 :: Double)
        , "legend" .= object ["0" .= big 9007199254740993, "1" .= big 9007199254740992]
        , "probabilities" .= object ["0" .= (0.5 :: Double), "1" .= (0.5 :: Double)]])]))
  r1n <- jev1 forged jevLatest world (scale (Present "?") bigRubric)
  check c "decode: legend equality is exact, not Double" (case r1n of Left (Decode (LegendMismatch _)) -> True; _ -> False)

  -- preparation errors, all total builders
  let prepErr :: Schema (Only e) => Q e -> Maybe PrepError
      prepErr q = either Just (const Nothing) (prepare jevLatest world (Only q))
  checkEq c "prepare: exit key collision rejected" (Just (ExitCollidesWithCandidate "value" "defer_to_model"))
    (prepErr (choose "?" (candidates [("defer_to_model", Null, Command "x"), ("other", Null, Command "y")]) [deferToModel "dup"]))
  checkEq c "prepare: no candidates and no exits rejected" (Just (EmptyCandidates "value"))
    (prepErr (choose "?" (candidates ([] :: [(Text, Description, Command)])) []))
  checkEq c "prepare: duplicate candidate keys rejected" (Just (DuplicateKeys "value" ["a"]))
    (prepErr (choose "?" (candidates [("a", Null, Command "x"), ("a", Null, Command "y")]) []))
  checkEq c "prepare: duplicate exit keys rejected" (Just (DuplicateWireKey "value" "no_match"))
    (prepErr (choose "?" (candidates [("a", Null, Command "a")]) [noMatch "x", noMatch "y"]))
  checkEq c "prepare: duplicate overridden keys rejected" (Just (DuplicateWireKey "value" "same"))
    (prepErr (choice "?" Mechanisms
      { retryRedelivery = optionKeyed "same" Null (Command "")
      , doubleAdmission = optionKeyed "same" Null (Command "")
      , unknownMechanism = option "?" (Command "") }))
  checkEq c "prepare: bare-number description rejected" (Just (BadDescription "value" "a"))
    (prepErr (choose "?" (candidates [("a", Number 4, Command "a")]) []))
  checkEq c "prepare: bare-boolean instructions rejected" (Just (BadInstructions "value"))
    (prepErr (noulWith (Present (Bool True)) Omitted))
  checkEq c "prepare: null level rejected" (Just (BadLevel "value" 0))
    (prepErr (scale (Present "?") (levelsOf [Null, "b"])))
  checkEq c "prepare: eleven levels rejected" (Just (BadLevelCount "value" 11))
    (prepErr (scale (Present "?") (levelsOf (map (String . T.pack . show) [1 :: Int .. 11]))))
  checkEq c "prepare: bare-number state rejected" (Just BadStateShape)
    (either Just (const Nothing) (prepare jevLatest (stateOf (Number 1)) (Only (noul "?"))))
  checkEq c "prepare: empty exact key rejected" (Just (EmptyQuestionKey ""))
    (either Just (const Nothing) (prepare jevLatest world (exact [("", someQ (noul "?"))])))

  -- the heterogeneous record
  let request = inspection groups
  case prepare jevLatest world request of
    Left e -> check c ("prepare inspect: " ++ show e) False
    Right prepared -> do
      let qs = questionsOf (requestValue prepared)
      checkEq c "prepare: nine flattened questions" 9 (length qs)
      check c "prepare: injective path for a key containing a dot" ("children.e\\.2.useful" `elem` map fst qs)
      checkEq c "prepare: static keys are snake-cased selectors"
        (Just ["ask_model", "follow_caller", "no_useful_path", "use_witness"])
        (lookup "next" qs >>= field "criteria" >>= \case
          Object o -> Just (sort (map Key.toText (KeyMap.keys o)))
          _ -> Nothing)
  r2 <- roundTrip (stub "ask_model") jevLatest world request
  case r2 of
    Left e -> check c ("inspect: " ++ show e) False
    Right resp -> do
      let a = answers resp
      checkEq c "inspect: resolved model kept in the envelope" "stub-1.0" (resolvedModel resp)
      checkEq c "inspect: usage passed through verbatim" (Just (Number 100)) (field "input_tokens" (usage resp))
      outcome <- match (next a) Routes
        { followCaller = \(Edge e) -> pure ("follow " ++ T.unpack e)
        , useWitness = \(Witness w) -> pure ("located " ++ T.unpack w)
        , noUsefulPath = \() -> pure "need other candidates"
        , askModel = \(Handoff w) -> pure ("handback " ++ T.unpack w)
        }
      checkEq c "inspect: exhaustive handlers ran the selected branch" "handback preference" outcome
      checkEq c "inspect: selected mass through the scoped projection" 0.7 (withChoice (next a) probabilityOf)
      check c "inspect: probe is dynamic with its own exit" (case picked (probe a) of
        PickedCandidate cand -> candidateKey cand == "run_retry_fixture"
        PickedExit _ -> False)
      checkEq c "inspect: score expectation survives" 1.5 (expectation (urgency a))
      checkEq c "inspect: legend survives as the submitted value" (String "A worker cannot take its next action") (blocked (legend (urgency a)))
      check c "inspect: each rebuilt by key" (case eachAnswers (children a) of
        [("e1", _), ("e.2", Relevance { useful = u })] -> probabilityYes u == 0.8
        _ -> False)
      check c "inspect: group rebuilt" (probabilityYes (gapRemains (groupAnswer (evidence a))) == 0.8)
      check c "inspect: noul vocabulary" (yesAbove 0.7 (enough (groupAnswer (evidence a))) && not (unsure 0.2 (enough (groupAnswer (evidence a)))))

  -- the complex cell
  let rubric = levelsOf ["no risk", "adjacent cases", "crosses a contract"]
      mechanisms = Mechanisms
        { retryRedelivery = option "The timeout retry redelivers m42 to the handler" (Command "just test-target actor retry")
        , doubleAdmission = option "The inbox admitted two records" (Command "just test-target node inbox_admission")
        , unknownMechanism = optionKeyed "?" "The observations do not distinguish" (Command "")
        }
      investigation = Investigation
        { mechanism = choice "Which mechanism explains the second callback?" mechanisms
        , checkIfRetry = given "the mechanism is retry redelivery" $ choose "Which check is the focused verification?" groups []
        , risk = scale (Present (object ["question" .= ("How broad is the fix?" :: Text), "focus" .= ("changed callers" :: Text)])) rubric
        , extras = many
            [ ("wake_now", someQ (noul "Does the state satisfy the wake policy?"))
            , ("future_kind", someQ (rawUnchecked (object ["type" .= ("noul" :: Text), "instructions" .= ("opaque to the DSL" :: Text)])))
            ]
        }
  first <- roundTrip (stubSplit ["retry_redelivery", "run_retry_fixture"] "double_admission") jevLatest world investigation
  case first of
    Left e -> check c ("complex: " ++ show e) False
    Right resp -> do
      let a = answers resp
          ms = masses (mechanism a)
          live = [ (name, q) | (name, mass, q) <- [ ("retry", retryRedelivery ms, retryRedelivery mechanisms)
                                                  , ("admission", doubleAdmission ms, doubleAdmission mechanisms) ]
                             , mass > (0.3 :: Double) ]
          wire = either (const []) (questionsOf . requestValue) (prepare jevLatest world investigation)
      checkEq c "complex: near-tie keeps two mechanisms alive" 2 (length live)
      checkEq c "complex: premise wraps the instruction unambiguously"
        (Just (object ["premise" .= ("the mechanism is retry redelivery" :: Text), "instructions" .= ("Which check is the focused verification?" :: Text)]))
        (lookup "check_if_retry" wire >>= field "instructions")
      checkEq c "complex: overridden key reaches the wire" (Just ["?", "double_admission", "retry_redelivery"])
        (lookup "mechanism" wire >>= field "criteria" >>= \case
          Object o -> Just (sort (map Key.toText (KeyMap.keys o)))
          _ -> Nothing)
      checkEq c "complex: dynamic rubric decoded in order" ["no risk", "adjacent cases", "crosses a contract"] (map fst (scaleMasses (risk a)))
      check c "complex: dynamic sub-map and raw answer round-trip" (case manyAnswers (extras a) of
        [("wake_now", SomeA _ _), ("future_kind", SomeA _ _)] -> True
        _ -> False)
      let enriched = stateObject [("observations", object [Key.fromText n .= ("observed via " <> n) | (n, _) <- live])]
      second <- jev1 (stub "retry_redelivery") jevLatest enriched (choice "Which mechanism now?" mechanisms)
      checkEq c "complex: second packet resolves after evidence" (Right "retry_redelivery") (fmap selectedKey second)

  -- malformed responses are decode errors, never values
  let choiceQ = choose "?" groups []
      expectDecode :: Schema (Only e) => String -> Value -> Q e -> (DecodeError -> Bool) -> IO ()
      expectDecode name resp q want = do
        r <- jev1 (fixed resp) jevLatest world q
        check c name (case r of Left (Decode e) -> want e; _ -> False)
  expectDecode "decode: unknown selection rejected"
    (answerMap [("value", object ["type" .= ("choice" :: Text), "choice" .= ("alien" :: Text), "probabilities" .= object ["alien" .= (1 :: Double)], "confidence" .= (1 :: Double)])])
    choiceQ (\case UnknownSelection _ "alien" -> True; _ -> False)
  expectDecode "decode: wrong answer kind rejected"
    (answerMap [("value", object ["type" .= ("noul" :: Text), "noul" .= (0.5 :: Double)])])
    choiceQ (\case WrongKind _ -> True; _ -> False)
  expectDecode "decode: probability key outside the submitted set rejected"
    (answerMap [("value", object ["type" .= ("choice" :: Text), "choice" .= ("run_retry_fixture" :: Text), "confidence" .= (0.5 :: Double)
      , "probabilities" .= object ["run_retry_fixture" .= (0.5 :: Double), "read_publish_gate" .= (0.3 :: Double), "ghost" .= (0.2 :: Double)]])])
    choiceQ (\case ExtraMass _ "ghost" -> True; _ -> False)
  expectDecode "decode: altered legend rejected"
    (answerMap [("value", object ["type" .= ("score" :: Text), "score" .= (1 :: Double), "confidence" .= (0.5 :: Double)
      , "legend" .= object ["0" .= ("no risk" :: Text), "1" .= ("altered" :: Text), "2" .= ("crosses a contract" :: Text)]
      , "probabilities" .= object ["0" .= (0.3 :: Double), "1" .= (0.4 :: Double), "2" .= (0.3 :: Double)]])])
    (scale (Present "?") rubric) (\case LegendMismatch _ -> True; _ -> False)
  expectDecode "decode: extra legend key rejected"
    (answerMap [("value", object ["type" .= ("score" :: Text), "score" .= (1 :: Double), "confidence" .= (0.5 :: Double)
      , "legend" .= object ["0" .= ("no risk" :: Text), "1" .= ("adjacent cases" :: Text), "2" .= ("crosses a contract" :: Text), "3" .= ("extra" :: Text)]
      , "probabilities" .= object ["0" .= (0.3 :: Double), "1" .= (0.4 :: Double), "2" .= (0.3 :: Double)]])])
    (scale (Present "?") rubric) (\case LegendMismatch _ -> True; _ -> False)
  expectDecode "decode: out-of-range probability rejected"
    (answerMap [("value", object ["type" .= ("noul" :: Text), "noul" .= (1.5 :: Double)])])
    (noul "?") (\case ValueOutOfRange _ _ -> True; _ -> False)
  expectDecode "decode: unexpected answer key rejected"
    (answerMap [("value", object ["type" .= ("noul" :: Text), "noul" .= (0.5 :: Double)]), ("stray", object ["type" .= ("noul" :: Text), "noul" .= (0.1 :: Double)])])
    (noul "?") (\case UnexpectedAnswer "stray" -> True; _ -> False)
  expectDecode "decode: provider rejection surfaces as a parsed Rejection"
    (object ["detail" .= ("Too many choices. Must have at most 255 choices." :: Text)])
    (noul "?") (\case ProviderRejected (RejectionMessage _) -> True; _ -> False)
  r5 <- jev1 (fixed (answerMap [("value", object ["type" .= ("noul" :: Text), "noul" .= (0.5 :: Double)])])) jevLatest world (rawUnchecked (object ["type" .= ("noul" :: Text)]))
  check c "decode: raw answer is the original parsed JSON" (case r5 of
    Right a -> rawAnswer a == object ["type" .= ("noul" :: Text), "noul" .= (0.5 :: Double)]
    Left _ -> False)
  r6 <- roundTrip (fixed (answerMap [("value", object ["type" .= ("choice" :: Text), "choice" .= ("run_retry_fixture" :: Text), "confidence" .= (0.5 :: Double)
      , "probabilities" .= object ["run_retry_fixture" .= (0.6 :: Double), "read_publish_gate" .= (0.5 :: Double)]])])) jevLatest world (Only choiceQ)
  check c "decode: rounded sum is a diagnostic, not a rejection" (case r6 of
    Right resp -> length (diagnostics resp) == 1
    Left _ -> False)
