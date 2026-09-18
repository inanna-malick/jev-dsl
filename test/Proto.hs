{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The authoring surface through its public facade only, over a stub
-- transport speaking aeson Values: inference without annotations, label
-- access, nested transparent answers, inline and reusable handler lists,
-- settle and judge under a policy, contenders through the same handlers,
-- and every preparation and decoding guarantee.
module Proto (protoChecks, stub, stubSplit, questionsOf, field) where

import Check
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Jev.Core as Core
import Jev.Operators hiding (field)
import Replay (noulWith, Presence (..))

-- ---------------------------------------------------------------------------
-- Domain payloads: never serialized
-- ---------------------------------------------------------------------------

newtype Command = Command Text deriving (Show, Eq)
data Edge = Edge { edgeKey :: Text, edgeText :: Text, edgeTarget :: Text } deriving (Show, Eq)
newtype Witness = Witness Text deriving (Show, Eq)
newtype Handoff = Handoff Text deriving (Show, Eq)

-- A named alternative set, for a reusable handler list.
type Routes = "use_witness" ::> Witness :|: "ask_model" ::> Handoff :|: "edges" ::* Edge

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
stub preferred = stubSplit [preferred] ""

-- Nouls answer 0.8; choices pick the first preferred key at 0.7, or split
-- 0.45/0.40 against a named rival; scores sit in the middle.
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

noulAt :: Double -> Value
noulAt p = object ["type" .= ("noul" :: Text), "noul" .= p]

-- ---------------------------------------------------------------------------

protoChecks :: Checks -> IO ()
protoChecks c = do
  let world = state (#inquiry := ("Where can cancellation drop a computed reply?" :: Text))
      edges = [Edge "publication_gate" "gates publication on cancellation" "publish_if_active", Edge "telemetry" "records latency" "record_latency"]
      edgeOffers = many #edges (.edgeKey) (.edgeText) edges

  -- tiny use: one question, one answer, no declarations; the row is the payload
  r1 <- ask1 (session (stub "publication_gate") jevLatest) world (choice "Which edge next?" edgeOffers)
  case r1 of
    Left e -> check c ("tiny: " ++ show e) False
    Right a -> do
      checkEq c "tiny: the chosen row is the payload" (Right (Settled "publish_if_active"))
        (settle lenient a (#edges (\_ e -> e.edgeTarget)))
      checkEq c "tiny: masses are wire keys" ["publication_gate", "telemetry"] (map fst a.masses)

  -- a packet inferred from its questions; a reusable handler list; settle under a policy
  let handlers :: (Handoff -> String) -> Handlers String Routes
      handlers onHandoff = #use_witness (\(Witness w) -> "located " ++ T.unpack w)
                        .| #ask_model onHandoff
                        .| #edges (\k _ -> "follow " ++ T.unpack k)
      packet = #next := choice "Which available continuation advances the inquiry?"
                         (  alt #use_witness "The current span already answers the inquiry" (Witness "complete_request:41")
                         .| alt #ask_model "Choosing needs a design preference beyond the supplied evidence" (Handoff "preference")
                         .| edgeOffers )
            :& #enough := noul "Does the supplied evidence answer the inquiry?"
            :& #urgency := score "What is the consequence of waiting?"
                         (  level #background "No current action depends on this" ("background" :: Text)
                         .| level #checkpoint "Useful at the next ordinary checkpoint" ("checkpoint" :: Text)
                         .| level #blocked "A worker cannot take its next action" ("blocked" :: Text)
                         .| level #invalidating "Continuing would invalidate ongoing work" ("invalidating" :: Text) )
            :& #children := each id (\name ->
                                 (   #useful := noul ("Is " <> name <> " relevant?")
                                 :&  #contradicts := noul ("Does " <> name <> " contradict the premise?")
                                 ) :: Packet ("useful" ::= Noul :& "contradicts" ::= Noul) Questions)
                                 ["e1", "e.2"]
            :& #evidence := (#gap := noul "Does answering require source not supplied?")
  case request jevLatest world packet of
    Left e -> check c ("packet: request failed: " ++ show e) False
    Right req -> do
      let qs = questionsOf req
      checkEq c "packet: eight flattened questions" 8 (length qs)
      check c "packet: injective path for a key containing a dot" ("children.e\\.2.useful" `elem` map fst qs)
      checkEq c "packet: alternative keys are the labels and runtime keys" (Just ["ask_model", "publication_gate", "telemetry", "use_witness"])
        (lookup "next" qs >>= field "criteria" >>= \case
          Object o -> Just (sort (map Key.toText (KeyMap.keys o)))
          _ -> Nothing)
      checkEq c "packet: wording reaches the wire" (Just "The current span already answers the inquiry")
        (lookup "next" qs >>= field "criteria" >>= field "use_witness")
      checkEq c "packet: rubric levels in order" (Just (Array (V.fromList ["No current action depends on this", "Useful at the next ordinary checkpoint", "A worker cannot take its next action", "Continuing would invalidate ongoing work"])))
        (lookup "urgency" qs >>= field "criteria")
      checkEq c "packet: state sent as given" (Just (String "Where can cancellation drop a computed reply?")) (field "state" req >>= field "inquiry")
  r2 <- ask (session (stubSplit ["ask_model", "publication_gate"] "") jevLatest) world packet
  case r2 of
    Left e -> check c ("packet: " ++ show e) False
    Right resp -> do
      let a = answers resp
      checkEq c "packet: usage kept" (Usage 100 7) (usage resp)
      checkEq c "packet: the winner runs with no policy at all" "handback preference"
        (handle a.next (handlers (\(Handoff h) -> "handback " ++ T.unpack h)))
      checkEq c "packet: inline handler list ran the chosen branch" (Right (Settled "handback preference"))
        (settle lenient a.next (#use_witness (\(Witness w) -> "located " ++ T.unpack w) .| #ask_model (\(Handoff h) -> "handback " ++ T.unpack h) .| #edges (\k _ -> "follow " ++ T.unpack k)))
      checkEq c "packet: reusable handler list" (Right (Settled "handback preference")) (settle lenient a.next (handlers (\(Handoff h) -> "handback " ++ T.unpack h)))
      checkEq c "packet: contenders eliminate through the same handlers, best first"
        ["handback preference", "follow publication_gate", "follow telemetry", "located complete_request:41"]
        (map snd (contenders 0 a.next (handlers (const "handback preference"))))
      checkEq c "packet: a floor keeps only the mass above it" 1
        (length (contenders 0.5 a.next (handlers (const ""))))
      check c "packet: a near tie is structured doubt, not a result"
        (case settle (Policy 0 0.7 0 :: Policy Strict) a.next (handlers (const "")) of
          Left (Doubt { cause = NearTie ("ask_model", _) ("publication_gate", _) }) -> True
          _ -> False)
      checkEq c "packet: explain names a near-tie doubt with the failing floor and the rest"
        "doubted ask_model (NearTie): margin 0.60 < 0.70 by 0.10; confidence 0.70, mass 0.70"
        (explain (Policy 0 0.7 0 :: Policy Strict) a.next)
      checkEq c "packet: explain names a settlement with every check"
        "settled on ask_model: confidence 0.70 \8805 0.00, mass 0.70 \8805 0.00, margin 0.60 \8805 0.00"
        (explain (Policy 0 0 0 :: Policy Strict) a.next)
      checkEq c "packet: noul" 0.8 a.enough.yes
      checkEq c "packet: a noul is judged under the same policy" (Right (Settled True)) (judge lenient a.enough)
      checkEq c "packet: judge explains in the same words"
        "settled on yes: mass 0.80 \8805 0.40, margin 0.60 \8805 0.08" (explain lenient a.enough)
      checkEq c "packet: rubric expectation" 1.5 a.urgency.expectation
      checkEq c "packet: typed rubric index" 0.5 (massAtOrAbove #blocked a.urgency)
      -- the stub spreads a score evenly, so each of four levels holds 0.25
      let urgencyAt f = grade f a.urgency
      -- flat over four levels: blocked and above holds exactly half
      checkEq c "grade: the median level of a flat rubric" ("blocked" :: Text) (urgencyAt 0.5)
      checkEq c "grade: a strict floor falls back to the lowest level" ("background" :: Text) (urgencyAt 0.9)
      checkEq c "grade: a floor nothing can miss takes the highest" ("invalidating" :: Text) (urgencyAt 0.2)
      checkEq c "grade: a floor above one is still total" ("background" :: Text) (urgencyAt 1.5)
      checkEq c "packet: one confidence for choices and scores" (0.7, 0.5) (a.next.confidence, a.urgency.confidence)
      check c "packet: each answers are a transparent keyed list" (case lookup "e.2" a.children of
        Just sub -> yes sub.useful == 0.8
        Nothing -> False)
      check c "packet: nested packet answers are the transparent sub-packet" (yes a.evidence.gap == 0.8)
      check c "packet: payload-independent Show" (length (show a) > 50)
      -- answers are plain records: every kind reads by field
      checkEq c "fields: a choice carries key, mass, margin and confidence"
        ("ask_model", 70, 60, 70) (a.next.key, cents a.next.mass, cents a.next.margin, cents a.next.confidence)
      checkEq c "fields: choice masses are best first" [("ask_model", 70)] [(k, cents m) | (k, m) <- take 1 a.next.masses]
      checkEq c "fields: a noul is one number" 0.8 a.enough.yes
      checkEq c "fields: a score reads expectation, confidence and masses"
        (150, 50, 4) (cents a.urgency.expectation, cents a.urgency.confidence, length a.urgency.masses)
      checkEq c "fields: a nested answer reads through its label" 0.8 a.evidence.gap.yes
      check c "show: a choice answer prints its own fields"
        (T.pack "Choice {key = \"ask_model\", mass = 0.70, margin = 0.60, confidence = 0.70, masses = [\"ask_model\" 0.70,"
          `T.isPrefixOf` T.pack (show a.next))
      checkEq c "show: a noul answer prints its own field" "Noul {yes = 0.80}" (show a.enough)
      check c "show: a response prints the answers under their labels"
        (all (`T.isInfixOf` T.pack (show resp)) ["next", "ask_model", "urgency", "evidence"])
      checkEq c "json: an answer is a ledger row" (Just (String "ask_model")) (field "key" (toJSON a.next))
      checkEq c "json: a whole packet is a ledger row" (Just (Just (Number 0.8)))
        (fmap (field "yes") (field "gap" (toJSON a.evidence)))

  -- judging a noul: yes, no, and the doubt in between
  let judged p = ask1 (session (fixed (answerMap [("value", noulAt p)])) jevLatest) world (noul "?")
  jy <- judged 0.9
  jn <- judged 0.1
  jd <- judged 0.55
  checkEq c "judge: a clear yes" (Right (Right (Settled True))) (fmap (judge strict) jy)
  checkEq c "judge: a clear no is a result, not a doubt" (Right (Right (Settled False))) (fmap (judge strict) jn)
  checkEq c "judge: a slim majority still answers under a lenient policy" (Right (Right (Settled True))) (fmap (judge lenient) jd)
  check c "judge: the middle is a near tie under a policy that wants a margin"
    (case fmap (judge (Policy 0 0.4 0 :: Policy Strict)) jd of
      Right (Left (Doubt { cause = NearTie ("yes", _) ("no", _) })) -> True
      _ -> False)
  checkEq c "judge: explain names the doubt and the floor it missed"
    (Right "doubted yes (Underweight): mass 0.55 < 0.70 by 0.15; margin 0.10")
    (fmap (explain strict) jd)

  -- structured wording
  let mech = #mechanism := choice "Which mechanism explains the second callback?"
                (  Core.alt #retry_redelivery (object ["what" .= ("retry redelivers m42" :: Text)]) (Command "just test-target actor retry")
                .| Core.alt #double_admission "The inbox admitted two records" (Command "just test-target node inbox")
                .| Core.alt #unknown Null () )
  case request jevLatest world mech of
    Left e -> check c ("wording: " ++ show e) False
    Right req -> do
      let qs = questionsOf req
      checkEq c "wording: structured wording" (Just (object ["what" .= ("retry redelivers m42" :: Text)]))
        (lookup "mechanism" qs >>= field "criteria" >>= field "retry_redelivery")
      checkEq c "wording: null wording admitted" (Just Null) (lookup "mechanism" qs >>= field "criteria" >>= field "unknown")
  r3 <- ask (session (stubSplit ["retry_redelivery"] "double_admission") jevLatest) world mech
  case r3 of
    Left e -> check c ("wording: " ++ show e) False
    Right resp -> do
      let a = answers resp
          live = contenders 0.3 a.mechanism
                   (#retry_redelivery (\(Command x) -> x) .| #double_admission (\(Command x) -> x) .| #unknown (\() -> ""))
      checkEq c "contenders: a near tie keeps two mechanisms alive" 2 (length live)
      checkEq c "contenders: each already through the handlers" ["just test-target actor retry", "just test-target node inbox"]
        (map snd live)

  -- preparation errors, every builder total
  let prepErr :: Core.Endpoint Value e => Q Value e -> Maybe PrepError
      prepErr q = case request jevLatest world (#value := q) of
        Left (Prepare e) -> Just e
        _ -> Nothing
      rows :: [(Text, Value, Edge)] -> Offers ("rows" ::* (Text, Value, Edge))
      rows = Core.many #rows (\(k, _, _) -> k) (\(_, d, _) -> d)
      nine = level #l0 "" () .| level #l1 "" () .| level #l2 "" () .| level #l3 "" () .| level #l4 "" () .| level #l5 "" () .| level #l6 "" () .| level #l7 "" () .| level #l8 "" ()
  checkEq c "prepare: empty runtime group with nothing else is an empty offer" (Just (EmptyOffer "value"))
    (prepErr (choice "?" (rows [])))
  checkEq c "prepare: duplicate runtime keys" (Just (DuplicateKeys "value" ["a"]))
    (prepErr (choice "?" (rows [("a", Null, Edge "x" "" ""), ("a", Null, Edge "y" "" "")])))
  checkEq c "prepare: runtime key colliding with a static label" (Just (KeyCollidesWithLabel "value" "use_witness"))
    (prepErr (choice "?" (alt #use_witness "w" (Witness "w") .| alt #ask_model "h" (Handoff "h") .| rows [("use_witness", Null, Edge "e" "" "")])))
  checkEq c "prepare: two runtime groups colliding" (Just (KeyCollidesWithLabel "value" "k"))
    (prepErr (choice "?" (rows [("k", Null, Edge "e" "" "")] .| Core.many #more fst (const Null) [("k", Command "c")])))
  checkEq c "prepare: bare-number wording rejected" (Just (BadDescription "value" "retry_redelivery"))
    (prepErr (choice "?" (Core.alt #retry_redelivery (Number 1) (Command "") .| Core.alt #unknown Null ())))
  checkEq c "prepare: bare-boolean instructions rejected" (Just (BadInstructions "value"))
    (prepErr (noulWith (Core.Instructions (Bool True)) Omitted))
  checkEq c "prepare: eleven levels rejected" (Just (BadLevelCount "value" 11))
    (prepErr (score "?" (level #l9 "" () .| level #l10 "" () .| nine)))
  checkEq c "prepare: null level rejected" (Just (BadLevel "value" 0))
    (prepErr (score "?" (Core.level #a Null () .| level #b "b" ())))
  checkEq c "prepare: bare-number state rejected" (Just BadStateShape)
    (case request jevLatest (Core.rawState (Number 1)) (#value := noul "?") of Left (Prepare e) -> Just e; _ -> Nothing)

  -- malformed responses are decode errors, never values
  let groups = choice "?" (many #rows fst snd [("run_retry_fixture" :: Text, "r" :: Text), ("read_publish_gate", "p")])
      expectDecode :: Core.Endpoint Value e => String -> Value -> Q Value e -> (DecodeError -> Bool) -> IO ()
      expectDecode name resp q want = do
        r <- ask1 (session (fixed resp) jevLatest) world q
        check c name (case r of Left (Decode e) -> want e; _ -> False)
      choiceAnswer sel ms = object ["type" .= ("choice" :: Text), "choice" .= sel, "confidence" .= (0.5 :: Double), "probabilities" .= object [Key.fromText k .= p | (k, p) <- ms]]
  expectDecode "decode: unknown selection rejected" (answerMap [("value", choiceAnswer ("alien" :: Text) [("alien", 1 :: Double)])]) groups (\case UnknownSelection _ "alien" -> True; _ -> False)
  expectDecode "decode: wrong answer kind rejected" (answerMap [("value", noulAt 0.5)]) groups (\case WrongKind _ -> True; _ -> False)
  expectDecode "decode: probability key outside the submitted set rejected"
    (answerMap [("value", choiceAnswer ("run_retry_fixture" :: Text) [("run_retry_fixture", 0.5 :: Double), ("read_publish_gate", 0.3), ("ghost", 0.2)])]) groups (\case ExtraMass _ "ghost" -> True; _ -> False)
  expectDecode "decode: missing mass rejected"
    (answerMap [("value", choiceAnswer ("run_retry_fixture" :: Text) [("run_retry_fixture", 1 :: Double)])]) groups (\case MissingMass _ "read_publish_gate" -> True; _ -> False)
  let rubric = score "?" (level #none "no risk" ("none" :: Text) .| level #adjacent "adjacent cases" ("adjacent" :: Text) .| level #contract "crosses a contract" ("contract" :: Text))
      scoreAnswer lg = object ["type" .= ("score" :: Text), "score" .= (1 :: Double), "confidence" .= (0.5 :: Double), "legend" .= object lg
        , "probabilities" .= object ["0" .= (0.3 :: Double), "1" .= (0.4 :: Double), "2" .= (0.3 :: Double)]]
  expectDecode "decode: altered legend rejected" (answerMap [("value", scoreAnswer ["0" .= ("no risk" :: Text), "1" .= ("altered" :: Text), "2" .= ("crosses a contract" :: Text)])])
    rubric (\case LegendMismatch _ -> True; _ -> False)
  expectDecode "decode: extra legend key rejected" (answerMap [("value", scoreAnswer ["0" .= ("no risk" :: Text), "1" .= ("adjacent cases" :: Text), "2" .= ("crosses a contract" :: Text), "3" .= ("x" :: Text)])])
    rubric (\case LegendMismatch _ -> True; _ -> False)
  let big n = object ["id" .= Number n]
  expectDecode "decode: legend equality is exact, not Double"
    (answerMap [("value", object ["type" .= ("score" :: Text), "score" .= (0.5 :: Double), "confidence" .= (0.5 :: Double)
      , "legend" .= object ["0" .= big 9007199254740993, "1" .= big 9007199254740992], "probabilities" .= object ["0" .= (0.5 :: Double), "1" .= (0.5 :: Double)]])])
    (score "?" (Core.level #a (big 9007199254740992) () .| Core.level #b (big 9007199254740993) ())) (\case LegendMismatch _ -> True; _ -> False)
  -- grade over real distributions, where the stub's flat one cannot reach
  let graded ps floor' = do
        r <- ask1 (session (fixed (answerMap [("value", object
              [ "type" .= ("score" :: Text), "score" .= (1 :: Double), "confidence" .= (0.5 :: Double)
              , "legend" .= object ["0" .= ("no risk" :: Text), "1" .= ("adjacent cases" :: Text), "2" .= ("crosses a contract" :: Text)]
              , "probabilities" .= object [Key.fromText (T.pack (show i)) .= p | (i, p) <- zip [0 :: Int ..] ps] ])])) jevLatest)
              world rubric
        pure (fmap (grade floor') r)
  g1 <- graded [0.2, 0.3, 0.5 :: Double] 0.5
  checkEq c "grade: the top level clears exactly at the floor" (Right ("contract" :: Text)) g1
  g2 <- graded [0.2, 0.3, 0.5 :: Double] 0.6
  checkEq c "grade: a stricter floor steps down one level" (Right ("adjacent" :: Text)) g2
  g3 <- graded [0.34, 0.33, 0.33 :: Double] 0.5
  checkEq c "grade: a near-flat rubric takes the middle" (Right ("adjacent" :: Text)) g3
  g4 <- graded [0.1, 0.2, 0.3 :: Double] 0.7
  checkEq c "grade: nothing clears, so the lowest level stands" (Right ("none" :: Text)) g4
  expectDecode "decode: out-of-range probability rejected" (answerMap [("value", noulAt 1.5)]) (noul "?") (\case ValueOutOfRange _ _ -> True; _ -> False)
  expectDecode "decode: unexpected answer key rejected"
    (answerMap [("value", noulAt 0.5), ("stray", noulAt 0.1)]) (noul "?") (\case UnexpectedAnswer "stray" -> True; _ -> False)
  expectDecode "decode: provider rejection surfaces as a parsed Rejection" (object ["detail" .= ("Too many choices." :: Text)]) (noul "?") (\case ProviderRejected (RejectionMessage _) -> True; _ -> False)
  r7 <- ask (session (fixed (answerMap [("value", choiceAnswer ("run_retry_fixture" :: Text) [("run_retry_fixture", 0.6 :: Double), ("read_publish_gate", 0.5)])])) jevLatest) world (#value := groups)
  check c "decode: rounded sum is a diagnostic, not a rejection" (case r7 of
    Right resp -> length (diagnostics resp) == 1
    Left _ -> False)
  where
    -- Probabilities compared as whole percents, never as exact Doubles.
    cents :: Double -> Int
    cents x = round (x * 100)
