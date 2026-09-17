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
-- access, append, nested transparent answers, inline and reusable handler
-- lists, accept then handle, contenders through the same handlers, pools
-- named by their binding and stamped into the questions that draw on them,
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
import Jev.Operators
import Jev.Transport (request)
import Replay (noulWith, Presence (..))

-- ---------------------------------------------------------------------------
-- Domain payloads: never serialized
-- ---------------------------------------------------------------------------

newtype Command = Command Text deriving (Show, Eq)
newtype Edge = Edge Text deriving (Show, Eq)
newtype Witness = Witness Text deriving (Show, Eq)
newtype Handoff = Handoff Text deriving (Show, Eq)

-- A named alternative set, for a reusable handler list.
type Routes = "use_witness" ::> Witness :|: "ask_model" ::> Handoff :|: Many Edge

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
  let world = state (object ["inquiry" .= ("Where can cancellation drop a computed reply?" :: Text)])
      edges = [("publication_gate", "gates publication on cancellation", Edge "publish_if_active"), ("telemetry", "records latency", Edge "record_latency")]

  -- tiny use: one question, one answer, no declarations
  r1 <- ask1 (stub "publication_gate") jevLatest world (choice "Which edge next?" (many edges))
  case r1 of
    Left e -> check c ("tiny: " ++ show e) False
    Right a -> do
      checkEq c "tiny: the chosen runtime element carries its payload" (Just (Edge "publish_if_active"))
        (handle (chosen a) (onMany (\_ e -> Just e)))
      checkEq c "tiny: masses are wire keys" ["publication_gate", "telemetry"] (map fst a.masses)

  -- a packet inferred from its questions; a reusable handler list; accept then handle
  let handlers :: (Handoff -> String) -> Handlers String Routes
      handlers onHandoff = #use_witness (\(Witness w) -> "located " ++ T.unpack w)
                        .| #ask_model onHandoff
                        .| onMany (\k _ -> "follow " ++ T.unpack k)
      packet = #next := choice "Which available continuation advances the inquiry?"
                         (  alt #use_witness "The current span already answers the inquiry" (Witness "complete_request:41")
                         .| alt #ask_model "Choosing needs a design preference beyond the supplied evidence" (Handoff "preference")
                         .| many edges )
            :& #enough := noul "Does the supplied evidence answer the inquiry?"
            :& #urgency := score "What is the consequence of waiting?"
                         (  level #background "No current action depends on this"
                         .| level #checkpoint "Useful at the next ordinary checkpoint"
                         .| level #blocked "A worker cannot take its next action"
                         .| level #invalidating "Continuing would invalidate ongoing work" )
            :& #children := each [ (name, #useful := noul ("Is " <> name <> " relevant?") :& #contradicts := noul ("Does " <> name <> " contradict the premise?") :& Nil)
                                 | name <- ["e1", "e.2"] ]
            :& #evidence := (#gap := noul "Does answering require source not supplied?" :& Nil)
            :& Nil
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
      checkEq c "packet: plain state stays as given" (Just (String "Where can cancellation drop a computed reply?")) (field "state" req >>= field "inquiry")
  r2 <- ask (stubSplit ["ask_model", "publication_gate"] "") jevLatest world packet
  case r2 of
    Left e -> check c ("packet: " ++ show e) False
    Right resp -> do
      let a = answers resp
      checkEq c "packet: usage kept" (Usage 100 7) (usage resp)
      checkEq c "packet: inline handler list ran the chosen branch" "handback preference"
        (handle (chosen a.next) (#use_witness (\(Witness w) -> "located " ++ T.unpack w) .| #ask_model (\(Handoff h) -> "handback " ++ T.unpack h) .| onMany (\k _ -> "follow " ++ T.unpack k)))
      checkEq c "packet: reusable handler list" "handback preference" (handle (chosen a.next) (handlers (\(Handoff h) -> "handback " ++ T.unpack h)))
      checkEq c "packet: contenders eliminate through the same handlers" (Right "follow publication_gate")
        (case [s | (_, s) <- contenders 0 a.next, "publication_gate" == keyOf s] of
          s : _ -> Right (handle s (handlers (const "no")))
          [] -> Left ())
      checkEq c "packet: a floor keeps only the mass above it" 1 (length (contenders 0.5 a.next))
      check c "packet: near-tie is structured doubt" (case accept (Policy 0 0.7 0) a.next of Left (NearTie _ _) -> True; _ -> False)
      check c "packet: an empty policy accepts the winner" (either (const False) (\s -> keyOf s == "ask_model") (accept (Policy 0 0 0) a.next))
      checkEq c "packet: explain names a near-tie doubt with the failing floor and the rest"
        "doubted (NearTie): margin 0.60 < 0.70 by 0.10; confidence 0.70, mass 0.70"
        (explain (Policy 0 0.7 0) a.next)
      checkEq c "packet: explain names an acceptance with every check"
        "accepted: confidence 0.70 \8805 0.00, mass 0.70 \8805 0.00, margin 0.60 \8805 0.00"
        (explain (Policy 0 0 0) a.next)
      checkEq c "packet: noul" 0.8 a.enough.yes
      checkEq c "packet: rubric expectation" 1.5 a.urgency.expectation
      checkEq c "packet: typed rubric index" 0.5 (massAtOrAbove #blocked a.urgency)
      checkEq c "packet: nearest level rounds the expectation" "blocked" a.urgency.nearest
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
      checkEq c "fields: a selection reads its own key" "ask_model" (chosen a.next).key
      checkEq c "fields: a noul is one number" 0.8 a.enough.yes
      checkEq c "fields: a score reads nearest, expectation, confidence and masses"
        ("blocked", 150, 50, 4) (a.urgency.nearest, cents a.urgency.expectation, cents a.urgency.confidence, length a.urgency.masses)
      checkEq c "fields: a nested answer reads through its label" 0.8 a.evidence.gap.yes
      check c "show: a choice answer prints its own fields"
        (T.pack "Choice {key = \"ask_model\", mass = 0.70, margin = 0.60, confidence = 0.70, masses = [\"ask_model\" 0.70,"
          `T.isPrefixOf` T.pack (show a.next))
      checkEq c "show: a noul answer prints its own field" "Noul {yes = 0.80}" (show a.enough)
      check c "show: a response prints the answers under their labels"
        (all (`T.isInfixOf` T.pack (show resp)) ["next", "ask_model", "urgency", "blocked", "evidence"])
      checkEq c "json: an answer is a ledger row" (Just (String "ask_model")) (field "key" (toJSON a.next))
      checkEq c "json: a whole packet is a ledger row" (Just (Just (Number 0.8)))
        (fmap (field "yes") (field "gap" (toJSON a.evidence)))

  -- append; structured wording; premise and structured members on a question
  let front = #mechanism := choice "Which mechanism explains the second callback?"
                (  alt #retry_redelivery (object ["what" .= ("retry redelivers m42" :: Text)]) (Command "just test-target actor retry")
                .| alt #double_admission "The inbox admitted two records" (Command "just test-target node inbox")
                .| alt #unknown Null () )
              :& Nil
      back = #check := given "the mechanism is retry redelivery" (about [("focus", "callbacks")] (choice "Which check verifies?"
                (alt #use_witness "w" (Witness "w") .| alt #ask_model "h" (Handoff "h") .| many edges)))
           :& Nil
      both = front ++. back
  case request jevLatest world both of
    Left e -> check c ("append: " ++ show e) False
    Right req -> do
      let qs = questionsOf req
      checkEq c "append: both fragments present" ["check", "mechanism"] (sort (map fst qs))
      checkEq c "append: structured wording" (Just (object ["what" .= ("retry redelivers m42" :: Text)]))
        (lookup "mechanism" qs >>= field "criteria" >>= field "retry_redelivery")
      checkEq c "append: null wording admitted" (Just Null) (lookup "mechanism" qs >>= field "criteria" >>= field "unknown")
      checkEq c "append: premise wraps the structured question"
        (Just (object ["premise" .= ("the mechanism is retry redelivery" :: Text), "instructions" .= object ["question" .= ("Which check verifies?" :: Text), "focus" .= ("callbacks" :: Text)]]))
        (lookup "check" qs >>= field "instructions")
  r3 <- ask (stubSplit ["retry_redelivery"] "double_admission") jevLatest world both
  case r3 of
    Left e -> check c ("append: " ++ show e) False
    Right resp -> do
      let a = answers resp
          live = contenders 0.3 a.mechanism
      checkEq c "append: near-tie keeps two mechanisms alive" 2 (length live)
      checkEq c "append: contenders eliminate through the same handlers" ["just test-target actor retry", "just test-target node inbox"]
        [handle s (#retry_redelivery (\(Command x) -> x) .| #double_admission (\(Command x) -> x) .| #unknown (\() -> "")) | (_, s) <- live]

  -- pools: named at their binding; the state envelope follows the packet;
  -- every question that draws on a pool names it
  let probes = pool #probes [("run_retry_fixture", "Retries m42 and counts callbacks", Command "just test-target actor retry"), ("read.gate", "Reads publish_if_active", Command "sed -n 30,60p x.rs")]
      relevance r =
        let Command command = refPayload r
        in #useful := askAbout r ("Does probe " <> refKey r <> " running `" <> command <> "` help answer the inquiry?") :& Nil
      pooledPacket = #probes := probes
                  :& #best := choice "Which probe first?" (manyFrom probes .| alt #none "No probe helps" ())
                  :& #per := eachIn probes relevance
                  :& Nil
  case request jevLatest world pooledPacket of
    Left e -> check c ("pools: " ++ show e) False
    Right req -> do
      let qs = questionsOf req
      checkEq c "pools: envelope with context and pools when a pool is declared"
        (Just (object ["run_retry_fixture" .= ("Retries m42 and counts callbacks" :: Text), "read.gate" .= ("Reads publish_if_active" :: Text)]))
        (field "state" req >>= field "pools" >>= field "probes")
      checkEq c "pools: context keeps the author's state" (Just (String "Where can cancellation drop a computed reply?"))
        (field "state" req >>= field "context" >>= field "inquiry")
      checkEq c "pools: pooled choice sends null wording" (Just Null) (lookup "best" qs >>= field "criteria" >>= field "read.gate")
      checkEq c "pools: pooled choice names its pool beside the question"
        (Just (object ["question" .= ("Which probe first?" :: Text), "pool" .= ("probes" :: Text)]))
        (lookup "best" qs >>= field "instructions")
      checkEq c "pools: askAbout addresses by structured fields"
        (Just (object ["question" .= ("Does probe read.gate running `sed -n 30,60p x.rs` help answer the inquiry?" :: Text), "pool" .= ("probes" :: Text), "key" .= ("read.gate" :: Text)]))
        (lookup "per.read\\.gate.useful" qs >>= field "instructions")
      checkEq c "pools: eachIn refs expose each public key and payload"
        (Just (object ["question" .= ("Does probe run_retry_fixture running `just test-target actor retry` help answer the inquiry?" :: Text), "pool" .= ("probes" :: Text), "key" .= ("run_retry_fixture" :: Text)]))
        (lookup "per.run_retry_fixture.useful" qs >>= field "instructions")
      checkEq c "pools: no question emitted for the declaration" 3 (length qs)
  r4 <- ask (stub "read.gate") jevLatest world pooledPacket
  case r4 of
    Left e -> check c ("pools: " ++ show e) False
    Right resp -> do
      let a = answers resp
      checkEq c "pools: chosen element keeps the local payload" (Just (Command "sed -n 30,60p x.rs"))
        (handle (chosen a.best) (onMany (\_ p -> Just p) .| #none (\() -> Nothing)))
      checkEq c "pools: per-entry answers keyed by pool key" ["read.gate", "run_retry_fixture"] (sort (map fst a.per))
  let other = pool #probes [("run_retry_fixture", "different text", Command "x")]
      conflicting = #probes := probes :& #best := choice "?" (manyFrom other) :& Nil
  checkEq c "pools: a use whose contents differ from the declaration is a conflict" (Left (Prepare (Core.ConflictingPool "probes")))
    (fmap (const ()) (request jevLatest world conflicting))
  checkEq c "pools: an undeclared pool use is rejected" (Left (Prepare (Core.UndeclaredPool "probes")))
    (fmap (const ()) (request jevLatest world (#best := choice "?" (manyFrom probes) :& Nil)))
  checkEq c "pools: a packet of only pools has no questions" (Left (Prepare Core.EmptyQuestionMap))
    (fmap (const ()) (request jevLatest world (#probes := probes :& Nil)))
  let dup = pool #dup [("same", "first", Command "1"), ("same", "second", Command "2")]
  checkEq c "pools: duplicate keys in a declaration are rejected" (Left (Prepare (Core.DuplicatePoolKey "dup" "same")))
    (fmap (const ()) (request jevLatest world (#dup := dup :& #q := eachIn dup relevance :& Nil)))
  -- two independently written fragments whose pools reuse local keys
  let buildChecks = pool #build_checks [("retry", "Retry the build", Command "just build"), ("cancel", "Cancel it", Command "just cancel")]
      mailChecks = pool #mailbox_checks [("retry", "Retry delivery", Command "just retry"), ("cancel", "Drop it", Command "just drop")]
      fragA = #build_checks := buildChecks :& #build_next := choice "Build step?" (manyFrom buildChecks) :& Nil
      fragB = #mailbox_checks := mailChecks :& #mail_next := choice "Mail step?" (manyFrom mailChecks) :& Nil
  case request jevLatest world (fragA ++. fragB) of
    Left e -> check c ("pools: composition " ++ show e) False
    Right req -> do
      let qs = questionsOf req
      checkEq c "pools: fragments with overlapping local keys compose" (Just (String "mailbox_checks"), Just (String "build_checks"))
        (lookup "mail_next" qs >>= field "instructions" >>= field "pool", lookup "build_next" qs >>= field "instructions" >>= field "pool")
      checkEq c "pools: both pools declared in state" 2 (maybe 0 (\case Object o -> KeyMap.size o; _ -> 0) (field "state" req >>= field "pools"))
  checkEq c "pools: one pool per choice" (Left (Prepare (Core.MultiplePoolsInChoice "value")))
    (fmap (const ()) (request jevLatest world (#build_checks := buildChecks :& #mailbox_checks := mailChecks :& #value := choice "?" (manyFrom buildChecks .| manyFrom mailChecks) :& Nil)))

  -- preparation errors, every builder total
  let prepErr :: (Core.Endpoint Value e, Core.CellOk "value" e) => Q Value e -> Maybe PrepError
      prepErr q = case request jevLatest world (#value := q :& Nil) of
        Left (Prepare e) -> Just e
        _ -> Nothing
      nine = level #l0 "" .| level #l1 "" .| level #l2 "" .| level #l3 "" .| level #l4 "" .| level #l5 "" .| level #l6 "" .| level #l7 "" .| level #l8 ""
  checkEq c "prepare: empty runtime group with nothing else is an empty offer" (Just (Core.EmptyOffer "value"))
    (prepErr (choice "?" (many ([] :: [(Text, Value, Edge)]))))
  checkEq c "prepare: duplicate runtime keys" (Just (Core.DuplicateKeys "value" ["a"]))
    (prepErr (choice "?" (many [("a", Null, Edge "x"), ("a", Null, Edge "y")])))
  checkEq c "prepare: runtime key colliding with a static label" (Just (Core.KeyCollidesWithLabel "value" "use_witness"))
    (prepErr (choice "?" (alt #use_witness "w" (Witness "w") .| alt #ask_model "h" (Handoff "h") .| many [("use_witness", Null, Edge "e")])))
  checkEq c "prepare: two runtime groups colliding" (Just (Core.KeyCollidesWithLabel "value" "k"))
    (prepErr (choice "?" (many [("k", Null, Edge "e")] .| many [("k", Null, Command "c")])))
  checkEq c "prepare: bare-number wording rejected" (Just (Core.BadDescription "value" "retry_redelivery"))
    (prepErr (choice "?" (alt #retry_redelivery (Number 1) (Command "") .| alt #unknown Null ())))
  checkEq c "prepare: bare-boolean instructions rejected" (Just (Core.BadInstructions "value"))
    (prepErr (noulWith (Core.Instructions (Bool True)) Omitted))
  checkEq c "prepare: duplicate structured member" (Just (Core.DuplicateInstructionKey "value" "question"))
    (prepErr (about [("question", "again")] (noul "q")))
  checkEq c "prepare: a premise does not hide a duplicate member" (Just (Core.DuplicateInstructionKey "value" "question"))
    (prepErr (given "premise" (about [("question", "again")] (noul "q"))))
  checkEq c "prepare: eleven levels rejected" (Just (Core.BadLevelCount "value" 11))
    (prepErr (score "?" (level #l9 "" .| level #l10 "" .| nine)))
  checkEq c "prepare: null level rejected" (Just (Core.BadLevel "value" 0))
    (prepErr (score "?" (level #a Null .| level #b "b")))
  checkEq c "prepare: bare-number state rejected" (Just Core.BadStateShape)
    (case request jevLatest (state (Number 1)) (#value := noul "?" :& Nil) of Left (Prepare e) -> Just e; _ -> Nothing)

  -- malformed responses are decode errors, never values
  let groups = choice "?" (many [("run_retry_fixture", "r", Command "a"), ("read_publish_gate", "p", Command "b")])
      expectDecode :: (Core.Endpoint Value e, Core.CellOk "value" e) => String -> Value -> Q Value e -> (DecodeError -> Bool) -> IO ()
      expectDecode name resp q want = do
        r <- ask1 (fixed resp) jevLatest world q
        check c name (case r of Left (Decode e) -> want e; _ -> False)
      choiceAnswer sel ms = object ["type" .= ("choice" :: Text), "choice" .= sel, "confidence" .= (0.5 :: Double), "probabilities" .= object [Key.fromText k .= p | (k, p) <- ms]]
  expectDecode "decode: unknown selection rejected" (answerMap [("value", choiceAnswer ("alien" :: Text) [("alien", 1 :: Double)])]) groups (\case Core.UnknownSelection _ "alien" -> True; _ -> False)
  expectDecode "decode: wrong answer kind rejected" (answerMap [("value", object ["type" .= ("noul" :: Text), "noul" .= (0.5 :: Double)])]) groups (\case Core.WrongKind _ -> True; _ -> False)
  expectDecode "decode: probability key outside the submitted set rejected"
    (answerMap [("value", choiceAnswer ("run_retry_fixture" :: Text) [("run_retry_fixture", 0.5 :: Double), ("read_publish_gate", 0.3), ("ghost", 0.2)])]) groups (\case Core.ExtraMass _ "ghost" -> True; _ -> False)
  expectDecode "decode: missing mass rejected"
    (answerMap [("value", choiceAnswer ("run_retry_fixture" :: Text) [("run_retry_fixture", 1 :: Double)])]) groups (\case Core.MissingMass _ "read_publish_gate" -> True; _ -> False)
  let rubric = score "?" (level #none "no risk" .| level #adjacent "adjacent cases" .| level #contract "crosses a contract")
      scoreAnswer lg = object ["type" .= ("score" :: Text), "score" .= (1 :: Double), "confidence" .= (0.5 :: Double), "legend" .= object lg
        , "probabilities" .= object ["0" .= (0.3 :: Double), "1" .= (0.4 :: Double), "2" .= (0.3 :: Double)]]
  expectDecode "decode: altered legend rejected" (answerMap [("value", scoreAnswer ["0" .= ("no risk" :: Text), "1" .= ("altered" :: Text), "2" .= ("crosses a contract" :: Text)])])
    rubric (\case Core.LegendMismatch _ -> True; _ -> False)
  expectDecode "decode: extra legend key rejected" (answerMap [("value", scoreAnswer ["0" .= ("no risk" :: Text), "1" .= ("adjacent cases" :: Text), "2" .= ("crosses a contract" :: Text), "3" .= ("x" :: Text)])])
    rubric (\case Core.LegendMismatch _ -> True; _ -> False)
  let big n = object ["id" .= Number n]
  expectDecode "decode: legend equality is exact, not Double"
    (answerMap [("value", object ["type" .= ("score" :: Text), "score" .= (0.5 :: Double), "confidence" .= (0.5 :: Double)
      , "legend" .= object ["0" .= big 9007199254740993, "1" .= big 9007199254740992], "probabilities" .= object ["0" .= (0.5 :: Double), "1" .= (0.5 :: Double)]])])
    (score "?" (level #a (big 9007199254740992) .| level #b (big 9007199254740993))) (\case Core.LegendMismatch _ -> True; _ -> False)
  expectDecode "decode: out-of-range probability rejected" (answerMap [("value", object ["type" .= ("noul" :: Text), "noul" .= (1.5 :: Double)])]) (noul "?") (\case Core.ValueOutOfRange _ _ -> True; _ -> False)
  expectDecode "decode: unexpected answer key rejected"
    (answerMap [("value", object ["type" .= ("noul" :: Text), "noul" .= (0.5 :: Double)]), ("stray", object ["type" .= ("noul" :: Text), "noul" .= (0.1 :: Double)])]) (noul "?") (\case Core.UnexpectedAnswer "stray" -> True; _ -> False)
  expectDecode "decode: provider rejection surfaces as a parsed Rejection" (object ["detail" .= ("Too many choices." :: Text)]) (noul "?") (\case Core.ProviderRejected (Core.RejectionMessage _) -> True; _ -> False)
  r7 <- ask (fixed (answerMap [("value", choiceAnswer ("run_retry_fixture" :: Text) [("run_retry_fixture", 0.6 :: Double), ("read_publish_gate", 0.5)])])) jevLatest world (#value := groups :& Nil)
  check c "decode: rounded sum is a diagnostic, not a rejection" (case r7 of
    Right resp -> length (Core.diagnostics resp) == 1
    Left _ -> False)
  where
    keyOf :: Core.Selected Value Routes -> Text
    keyOf = selectedKey
    -- Probabilities compared as whole percents, never as exact Doubles.
    cents :: Double -> Int
    cents x = round (x * 100)
