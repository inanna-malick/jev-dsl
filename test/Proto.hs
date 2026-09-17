{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The compositional-operator front through its public facade only, over a
-- stub transport speaking aeson Values. Covers the spike list: inference
-- without annotations, label access, append, nested transparent answers,
-- inline and reusable handler lists, accept then handle, pools named by
-- their cell label with references used in separate fragments, the
-- ordinary-sum seam, and every preparation and decoding guarantee.
module Proto (protoChecks) where

import Check
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Jev.Operators

-- ---------------------------------------------------------------------------
-- Domain payloads: never serialized
-- ---------------------------------------------------------------------------

newtype Command = Command Text deriving (Show, Eq)
newtype Edge = Edge Text deriving (Show, Eq)
newtype Witness = Witness Text deriving (Show, Eq)
newtype Handoff = Handoff Text deriving (Show, Eq)

-- Alternatives with descriptions in the type; Many for runtime candidates.
type Routes = "use_witness" ::> Witness :? "The current span already answers the inquiry"
          :|: "ask_model" ::> Handoff :? "Choosing needs a design preference beyond the supplied evidence"
          :|: Many Edge

-- Alternatives described at the value level (structured, runtime wording).
type Mechanisms = "retry_redelivery" ::> Command :|: "double_admission" ::> Command :|: "unknown" ::> ()

-- A described rubric and a bare one.
type Urgency = '[ "background" :? "No current action depends on this"
                , "checkpoint" :? "Useful at the next ordinary checkpoint"
                , "blocked" :? "A worker cannot take its next action"
                , "invalidating" :? "Continuing would invalidate ongoing work" ]
type Breadth = '[ Lvl "localized", Lvl "adjacent", Lvl "contract" ]

-- The ordinary-sum seam.
data Next = Rerun Command | ReadSource Text | AskModel Handoff deriving (Generic, Show)
instance ConName Next

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
  let world = stateObject [("inquiry", "Where can cancellation drop a computed reply?")]
      edges = [("publication_gate", "gates publication on cancellation", Edge "publish_if_active"), ("telemetry", "records latency", Edge "record_latency")]

  -- tiny use: one question, one answer, no declarations
  r1 <- jev1 (stub "publication_gate") jevLatest world (choice @(Many Edge) "Which edge next?" (many edges))
  case r1 of
    Left e -> check c ("tiny: " ++ show e) False
    Right a -> do
      checkEq c "tiny: the chosen runtime element carries its payload" (Just (Edge "publish_if_active"))
        (caseOf a (onMany (\e -> Just (elementPayload e))))
      checkEq c "tiny: alternatives are wire keys and masses" ["publication_gate", "telemetry"] (map fst (alternatives a))

  -- a packet inferred from its questions; a reusable handler list; accept then handle
  let handlers :: (Handoff -> String) -> Handlers String Routes
      handlers onHandoff = #use_witness (\(Witness w) -> "located " ++ T.unpack w)
                        .| #ask_model onHandoff
                        .| onMany (\e -> "follow " ++ T.unpack (elementKey e))
      packet = #next := choice @Routes "Which available continuation advances the inquiry?"
                         (#use_witness (Witness "complete_request:41") .| #ask_model (Handoff "preference") .| many edges)
            :& #enough := noul "Does the supplied evidence answer the inquiry?"
            :& #urgency := score @Urgency "What is the consequence of waiting?"
            :& #children := each [ (name, #useful := noul ("Is " <> name <> " relevant?") :& #contradicts := noul ("Does " <> name <> " contradict the premise?") :& Nil)
                                 | name <- ["e1", "e.2"] ]
            :& #evidence := group (#gap := noul "Does answering require source not supplied?" :& Nil)
            :& Nil
  case prepare jevLatest world packet of
    Left e -> check c ("packet: prepare failed: " ++ show e) False
    Right prepared -> do
      let qs = questionsOf (requestValue prepared)
      checkEq c "packet: eight flattened questions" 8 (length qs)
      check c "packet: injective path for a key containing a dot" ("children.e\\.2.useful" `elem` map fst qs)
      checkEq c "packet: alternative keys are the labels and runtime keys" (Just ["ask_model", "publication_gate", "telemetry", "use_witness"])
        (lookup "next" qs >>= field "criteria" >>= \case
          Object o -> Just (sort (map Key.toText (KeyMap.keys o)))
          _ -> Nothing)
      checkEq c "packet: typed description reaches the wire" (Just "The current span already answers the inquiry")
        (lookup "next" qs >>= field "criteria" >>= field "use_witness")
      check c "packet: preview renders" (T.length (preview prepared) > 100)
  r2 <- roundTrip (stubSplit ["ask_model", "publication_gate"] "") jevLatest world packet
  case r2 of
    Left e -> check c ("packet: " ++ show e) False
    Right resp -> do
      let a = answers resp
      checkEq c "packet: resolved model kept" "stub-1.0" (resolvedModel resp)
      checkEq c "packet: inline handler list ran the chosen branch" "handback preference"
        (caseOf a.next (#use_witness (\(Witness w) -> "located " ++ T.unpack w) .| #ask_model (\(Handoff h) -> "handback " ++ T.unpack h) .| onMany (\e -> "follow " ++ T.unpack (elementKey e))))
      checkEq c "packet: reusable handler list" "handback preference" (caseOf a.next (handlers (\(Handoff h) -> "handback " ++ T.unpack h)))
      checkEq c "packet: accept then handle a contender" (Right "follow publication_gate")
        (case [s | (_, s) <- ranked a.next, "publication_gate" == keyOf s] of
          s : _ -> Right (handle s (handlers (const "no")))
          [] -> Left ())
      check c "packet: near-tie is structured doubt" (case accept (Policy 0 0.7 0) a.next of Left (NearTie _ _) -> True; _ -> False)
      check c "packet: lenient accepts the winner" (either (const False) (\s -> keyOf s == "ask_model") (accept lenient a.next))
      checkEq c "packet: noul vocabulary" True (yesAbove 0.7 a.enough)
      checkEq c "packet: rubric expectation" 1.5 (expectation a.urgency)
      checkEq c "packet: typed rubric index" 0.5 (massAtOrAbove #blocked a.urgency)
      checkEq c "packet: nearest level rounds the expectation" "blocked" (levelOf a.urgency)
      check c "packet: each answers are a transparent keyed list" (case lookup "e.2" a.children of
        Just sub -> probabilityYes sub.useful == 0.8
        Nothing -> False)
      check c "packet: group answers are the transparent sub-packet" (probabilityYes a.evidence.gap == 0.8)
      check c "packet: payload-independent Show" (length (show a) > 50)

  -- append, and structured descriptions on bare alternatives with a describe override
  let front = #mechanism := choice @Mechanisms "Which mechanism explains the second callback?"
                (#retry_redelivery (object ["what" .= ("retry redelivers m42" :: Text)], Command "just test-target actor retry")
                .| #double_admission ("The inbox admitted two records", Command "just test-target node inbox")
                .| #unknown (Null, ()))
              :& Nil
      back = #check := given "the mechanism is retry redelivery" (choice @Routes "Which check verifies?" (#use_witness (Witness "w") .| describe (object ["why" .= ("override" :: Text)]) (#ask_model (Handoff "h")) .| many edges))
           :& Nil
      both = front ++. back
  case prepare jevLatest world both of
    Left e -> check c ("append: " ++ show e) False
    Right prepared -> do
      let qs = questionsOf (requestValue prepared)
      checkEq c "append: both fragments present" ["check", "mechanism"] (sort (map fst qs))
      checkEq c "append: structured bare description" (Just (object ["what" .= ("retry redelivers m42" :: Text)]))
        (lookup "mechanism" qs >>= field "criteria" >>= field "retry_redelivery")
      checkEq c "append: null description admitted" (Just Null) (lookup "mechanism" qs >>= field "criteria" >>= field "unknown")
      checkEq c "append: describe overrides the typed description" (Just (object ["why" .= ("override" :: Text)]))
        (lookup "check" qs >>= field "criteria" >>= field "ask_model")
      checkEq c "append: premise wraps the instruction"
        (Just (object ["premise" .= ("the mechanism is retry redelivery" :: Text), "instructions" .= ("Which check verifies?" :: Text)]))
        (lookup "check" qs >>= field "instructions")
  r3 <- roundTrip (stubSplit ["retry_redelivery"] "double_admission") jevLatest world both
  case r3 of
    Left e -> check c ("append: " ++ show e) False
    Right resp -> do
      let a = answers resp
          live = [s | (m, s) <- ranked a.mechanism, m > 0.3]
      checkEq c "append: near-tie keeps two mechanisms alive" 2 (length live)
      checkEq c "append: contenders eliminate through the same handlers" ["just test-target actor retry", "just test-target node inbox"]
        [handle s (#retry_redelivery (\(Command x) -> x) .| #double_admission (\(Command x) -> x) .| #unknown (\() -> "")) | s <- live]

  -- pools: name from the cell label; references used in a separately built fragment
  let probes = pool #probes [("run_retry_fixture", "Retries m42 and counts callbacks", Command "just test-target actor retry"), ("read.gate", "Reads publish_if_active", Command "sed -n 30,60p x.rs")]
      relevance r = #useful := askAbout r "Does this probe help answer the inquiry?" :& Nil
      pooledPacket = #probes := probes
                  :& #best := choice @(Many Command) "Which probe first?" (manyFrom probes)
                  :& #per := eachIn probes relevance
                  :& Nil
  case prepare jevLatest world pooledPacket of
    Left e -> checkEq c "pools: plain state is rejected when pools are declared" PoolsRequirePooledState e
    Right _ -> check c "pools: plain state is rejected when pools are declared" False
  case prepare jevLatest (pooled world) pooledPacket of
    Left e -> check c ("pools: " ++ show e) False
    Right prepared -> do
      let req = requestValue prepared
          qs = questionsOf req
      checkEq c "pools: explicit envelope with context and pools"
        (Just (object ["run_retry_fixture" .= ("Retries m42 and counts callbacks" :: Text), "read.gate" .= ("Reads publish_if_active" :: Text)]))
        (field "state" req >>= field "pools" >>= field "probes")
      checkEq c "pools: context keeps the author's state" (Just (String "Where can cancellation drop a computed reply?"))
        (field "state" req >>= field "context" >>= field "inquiry")
      checkEq c "pools: pooled choice sends null descriptions" (Just Null) (lookup "best" qs >>= field "criteria" >>= field "read.gate")
      checkEq c "pools: askAbout addresses by structured fields"
        (Just (object ["question" .= ("Does this probe help answer the inquiry?" :: Text), "pool" .= ("probes" :: Text), "key" .= ("read.gate" :: Text)]))
        (lookup "per.read\\.gate.useful" qs >>= field "instructions")
      checkEq c "pools: no question emitted for the declaration" 3 (length qs)
  r4 <- roundTrip (stub "read.gate") jevLatest (pooled world) pooledPacket
  case r4 of
    Left e -> check c ("pools: " ++ show e) False
    Right resp -> do
      let a = answers resp
      checkEq c "pools: chosen element keeps the local description" (Just "Reads publish_if_active")
        (caseOf a.best (onMany (\e -> Just (elementDescription e))))
      checkEq c "pools: payload lookup after the fact" (Just (Command "sed -n 30,60p x.rs"))
        (lookup "read.gate" [(k, p) | (k, _, p) <- poolEntries a.probes])
  let other = pool #probes [("run_retry_fixture", "different text", Command "x")] :: Q Value (PoolDecl "probes" Command)
      conflicting = #probes := probes :& #best := choice @(Many Command) "?" (manyFrom other) :& Nil
  checkEq c "pools: a use whose contents differ from the declaration is a conflict" (Left (ConflictingPool "probes"))
    (fmap (const ()) (prepare jevLatest (pooled world) conflicting))
  checkEq c "pools: an undeclared pool use is rejected" (Left (UndeclaredPool "probes"))
    (fmap (const ()) (prepare jevLatest (pooled world) (#best := choice @(Many Command) "?" (manyFrom probes) :& Nil)))
  checkEq c "pools: a packet of only pools has no questions" (Left EmptyQuestionMap)
    (fmap (const ()) (prepare jevLatest (pooled world) (#probes := probes :& Nil)))

  -- the ordinary-sum seam: same endpoint, case elimination
  r5 <- jev1 (stub "read_source") jevLatest world
          (choice @(Sum Next) "What next?" (sumOffer [("rerun the check", Rerun (Command "c")), ("read the source", ReadSource "x.rs"), ("ask", AskModel (Handoff "h"))]))
  check c "sum: constructor names are wire keys and case eliminates" (case r5 of
    Right a -> (case chosen a of SelSum _ (ReadSource s) -> s == "x.rs"; _ -> False) && sort (map fst (alternatives a)) == ["ask_model", "read_source", "rerun"]
    Left _ -> False)

  -- preparation errors, every builder total
  let prepErr :: (Endpoint Value e, CellOk "value" e) => Q Value e -> Maybe PrepError
      prepErr q = either Just (const Nothing) (prepare jevLatest world (#value := q :& Nil))
  checkEq c "prepare: empty runtime group with nothing else is an empty offer" (Just (EmptyOffer "value"))
    (prepErr (choice @(Many Edge) "?" (many [])))
  checkEq c "prepare: duplicate runtime keys" (Just (DuplicateKeys "value" ["a"]))
    (prepErr (choice @(Many Edge) "?" (many [("a", Null, Edge "x"), ("a", Null, Edge "y")])))
  checkEq c "prepare: runtime key colliding with a static label" (Just (KeyCollidesWithLabel "value" "use_witness"))
    (prepErr (choice @Routes "?" (#use_witness (Witness "w") .| #ask_model (Handoff "h") .| many [("use_witness", Null, Edge "e")])))
  checkEq c "prepare: two runtime groups colliding" (Just (KeyCollidesWithLabel "value" "k"))
    (prepErr (choice @(Many Edge :|: Many Command) "?" (many [("k", Null, Edge "e")] .| many [("k", Null, Command "c")])))
  checkEq c "prepare: bare-number description rejected" (Just (BadDescription "value" "retry_redelivery"))
    (prepErr (choice @Mechanisms "?" (#retry_redelivery (Number 1, Command "") .| #double_admission (Null, Command "") .| #unknown (Null, ()))))
  checkEq c "prepare: bare-boolean instructions rejected" (Just (BadInstructions "value"))
    (prepErr (noulWith (Instructions (Bool True)) noCriteria))
  checkEq c "prepare: duplicate structured instruction key" (Just (DuplicateInstructionKey "value" "question"))
    (prepErr (noulOn (about "q" [("question", "again")]) noCriteria))
  checkEq c "prepare: bare rubric without runtime descriptions" (Just (RubricMismatch "value"))
    (prepErr (score @Breadth "?"))
  checkEq c "prepare: runtime descriptions must match the rubric labels" (Just (RubricMismatch "value"))
    (prepErr (scoreWith @Breadth (question "?") [("localized", "a"), ("adjacent", "b")]))
  checkEq c "prepare: null level rejected" (Just (BadLevel "value" 0))
    (prepErr (scale (question "?") (levelsOf [Null, "b"])))
  checkEq c "prepare: eleven runtime levels rejected" (Just (BadLevelCount "value" 11))
    (prepErr (scale (question "?") (levelsOf (map (String . T.pack . show) [1 :: Int .. 11]))))
  checkEq c "prepare: bare-number state rejected" (Just BadStateShape)
    (either Just (const Nothing) (prepare jevLatest (stateOf (Number 1)) (#value := noul "?" :& Nil)))
  checkEq c "prepare: empty exact key rejected" (Just (EmptyQuestionKey ""))
    (either Just (const Nothing) (prepare jevLatest world (exact [("", someQ (noul "?"))])))

  -- malformed responses are decode errors, never values
  let groups = choice @(Many Command) "?" (many [("run_retry_fixture", "r", Command "a"), ("read_publish_gate", "p", Command "b")])
      expectDecode :: (Endpoint Value e, CellOk "value" e) => String -> Value -> Q Value e -> (DecodeError -> Bool) -> IO ()
      expectDecode name resp q want = do
        r <- jev1 (fixed resp) jevLatest world q
        check c name (case r of Left (Decode e) -> want e; _ -> False)
      choiceAnswer sel ms = object ["type" .= ("choice" :: Text), "choice" .= sel, "confidence" .= (0.5 :: Double), "probabilities" .= object [Key.fromText k .= p | (k, p) <- ms]]
  expectDecode "decode: unknown selection rejected" (answerMap [("value", choiceAnswer ("alien" :: Text) [("alien", 1 :: Double)])]) groups (\case UnknownSelection _ "alien" -> True; _ -> False)
  expectDecode "decode: wrong answer kind rejected" (answerMap [("value", object ["type" .= ("noul" :: Text), "noul" .= (0.5 :: Double)])]) groups (\case WrongKind _ -> True; _ -> False)
  expectDecode "decode: probability key outside the submitted set rejected"
    (answerMap [("value", choiceAnswer ("run_retry_fixture" :: Text) [("run_retry_fixture", 0.5 :: Double), ("read_publish_gate", 0.3), ("ghost", 0.2)])]) groups (\case ExtraMass _ "ghost" -> True; _ -> False)
  expectDecode "decode: missing mass rejected"
    (answerMap [("value", choiceAnswer ("run_retry_fixture" :: Text) [("run_retry_fixture", 1 :: Double)])]) groups (\case MissingMass _ "read_publish_gate" -> True; _ -> False)
  let rubric = levelsOf ["no risk", "adjacent cases", "crosses a contract"]
      scoreAnswer lg = object ["type" .= ("score" :: Text), "score" .= (1 :: Double), "confidence" .= (0.5 :: Double), "legend" .= object lg
        , "probabilities" .= object ["0" .= (0.3 :: Double), "1" .= (0.4 :: Double), "2" .= (0.3 :: Double)]]
  expectDecode "decode: altered legend rejected" (answerMap [("value", scoreAnswer ["0" .= ("no risk" :: Text), "1" .= ("altered" :: Text), "2" .= ("crosses a contract" :: Text)])])
    (scale (question "?") rubric) (\case LegendMismatch _ -> True; _ -> False)
  expectDecode "decode: extra legend key rejected" (answerMap [("value", scoreAnswer ["0" .= ("no risk" :: Text), "1" .= ("adjacent cases" :: Text), "2" .= ("crosses a contract" :: Text), "3" .= ("x" :: Text)])])
    (scale (question "?") rubric) (\case LegendMismatch _ -> True; _ -> False)
  expectDecode "decode: typed rubric legend must match the typed descriptions"
    (answerMap [("value", object ["type" .= ("score" :: Text), "score" .= (1 :: Double), "confidence" .= (0.5 :: Double)
      , "legend" .= object ["0" .= ("No current action depends on this" :: Text), "1" .= ("altered" :: Text), "2" .= ("A worker cannot take its next action" :: Text), "3" .= ("Continuing would invalidate ongoing work" :: Text)]
      , "probabilities" .= object ["0" .= (0.25 :: Double), "1" .= (0.25 :: Double), "2" .= (0.25 :: Double), "3" .= (0.25 :: Double)]])])
    (score @Urgency "?") (\case LegendMismatch _ -> True; _ -> False)
  let big n = object ["id" .= Number n]
  expectDecode "decode: legend equality is exact, not Double"
    (answerMap [("value", object ["type" .= ("score" :: Text), "score" .= (0.5 :: Double), "confidence" .= (0.5 :: Double)
      , "legend" .= object ["0" .= big 9007199254740993, "1" .= big 9007199254740992], "probabilities" .= object ["0" .= (0.5 :: Double), "1" .= (0.5 :: Double)]])])
    (scale (question "?") (levelsOf [big 9007199254740992, big 9007199254740993])) (\case LegendMismatch _ -> True; _ -> False)
  expectDecode "decode: out-of-range probability rejected" (answerMap [("value", object ["type" .= ("noul" :: Text), "noul" .= (1.5 :: Double)])]) (noul "?") (\case ValueOutOfRange _ _ -> True; _ -> False)
  expectDecode "decode: unexpected answer key rejected"
    (answerMap [("value", object ["type" .= ("noul" :: Text), "noul" .= (0.5 :: Double)]), ("stray", object ["type" .= ("noul" :: Text), "noul" .= (0.1 :: Double)])]) (noul "?") (\case UnexpectedAnswer "stray" -> True; _ -> False)
  expectDecode "decode: provider rejection surfaces as a parsed Rejection" (object ["detail" .= ("Too many choices." :: Text)]) (noul "?") (\case ProviderRejected (RejectionMessage _) -> True; _ -> False)
  r6 <- jev1 (fixed (answerMap [("value", object ["type" .= ("noul" :: Text), "noul" .= (0.5 :: Double)])])) jevLatest world (rawUnchecked (object ["type" .= ("noul" :: Text)]))
  check c "decode: raw answer is the original parsed JSON" (case r6 of
    Right a -> rawAnswer a == object ["type" .= ("noul" :: Text), "noul" .= (0.5 :: Double)]
    Left _ -> False)
  r7 <- roundTrip (fixed (answerMap [("value", choiceAnswer ("run_retry_fixture" :: Text) [("run_retry_fixture", 0.6 :: Double), ("read_publish_gate", 0.5)])])) jevLatest world (#value := groups :& Nil)
  check c "decode: rounded sum is a diagnostic, not a rejection" (case r7 of
    Right resp -> length (diagnostics resp) == 1
    Left _ -> False)
  where
    keyOf :: Selected Routes -> Text
    keyOf = selectedKey
