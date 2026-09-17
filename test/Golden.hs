{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden requests and decodes against real captures. A hand-authored
-- packet must render exactly the captured request, and decoding the captured
-- response against it must yield the captured answers.
module Golden (goldenChecks, genericChecks) where

import Check
import Control.Monad (forM_, unless)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Vector as V
import Fixtures
import Jev.Operators
import Shape
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension)
import qualified Data.Aeson as Aeson

-- ---------------------------------------------------------------------------
-- Packets for the probe family that shares one state and one Noul
-- ---------------------------------------------------------------------------

probeState :: State 'Plain
probeState = stateObject
  [ ("message", "The worker cannot proceed until the missing configuration is supplied.")
  , ("context", object ["active" .= True, "pending" .= (2 :: Int), "previous" .= Null])
  ]

wakeInstructions :: Instructions
wakeInstructions = Instructions (object ["question" .= ("Does `message` describe a blocker for current work?" :: Text)])

wakeCriteria :: Presence (Maybe (Criteria Value))
wakeCriteria = Present (Just (Criteria
  (Present (object ["means" .= ("Current work cannot proceed" :: Text), "examples" .= ["Missing required input" :: Text]]))
  (Present (object ["means" .= ("Work can proceed without this message" :: Text)]))))

wake :: Q Value Noul -> Packet '["wake" ::= Noul] Questions
wake q = #wake := q :& Nil

route :: [(Text, Value, ())] -> Packet '["route" ::= Choice (Many ())] Questions
route cs = #route := choice "Who owns configuration?" (many cs) :& Nil

count :: Levels -> Packet '["count" ::= Scale] Questions
count ls = #count := scale (question "How many messages are pending in `context.pending`?") ls :& Nil

-- structured-001: the canonical mixed packet. Descriptions are runtime
-- values here (the capture's wording is structured), so the alternatives
-- are bare and described at the offer.
type Owners = "configuration_owner" ::> Text :|: "reviewer" ::> Text :|: "neither" ::> ()
type UrgencyLevels = '[ Lvl "informational", Lvl "blocking" ]

triage :: Packet '[ "route" ::= Choice Owners, "urgency" ::= Score UrgencyLevels, "wake" ::= Noul ] Questions
triage =
     #route := choiceWith (Instructions (object ["question" .= ("Who can resolve the missing configuration?" :: Text), "focus" .= ["Current blocker" :: Text]]))
                 (  #configuration_owner (object ["handles" .= object ["configuration" .= ["missing values" :: Text, "invalid values"]]], "config")
                 .| #reviewer (object ["handles" .= ["completed work" :: Text]], "review")
                 .| #neither (Null, ()) )
  :& #urgency := scoreWith (question "How urgently does this message need attention?")
                 [ ("informational", object ["means" .= ("Useful information, work can continue" :: Text)])
                 , ("blocking", object ["means" .= ("Work cannot continue until someone responds" :: Text)]) ]
  :& #wake := noulWith wakeInstructions wakeCriteria
  :& Nil

-- world-conflict-001: Each, two Groups, static and runtime choices, static
-- levels. Rebuilt from the fixture's own wording, since it is long; the
-- structure is what the packet proves.
type Actions = "continue" ::> () :|: "hold_for_contract" ::> () :|: "repair_or_revalidate" ::> ()
type Readiness = '[ Lvl "unresolved_issue", Lvl "partial_evidence", Lvl "current_applicable" ]
type Kinds = "shared_decision" ::> () :|: "local_repair" ::> () :|: "ship" ::> () :|: "unknown" ::> ()
type Pairs = "o1_o2" ::> () :|: "o3_o4" ::> () :|: "o4_o5" ::> ()

type Branch = Packet '[ "action" ::= Choice Actions, "affected" ::= Noul, "readiness" ::= Score Readiness ]
type Decision = Packet '[ "owner" ::= Choice (Many ()), "kind" ::= Choice Kinds, "witness" ::= Choice Pairs ]
type Evidence = Packet '[ "old_review_applies" ::= Noul, "opinion_overrides" ::= Noul ]
type World = Packet '[ "decision" ::= Group Decision, "branches" ::= Each Branch, "evidence" ::= Group Evidence ]

world :: Value -> World Questions
world req =
     #decision := group
       (  #owner := choiceWith (instr "decision.owner") (many [(k, d, ()) | (k, d) <- crit "decision.owner"])
       :& #kind := choiceWith (instr "decision.kind")
            (  #shared_decision (descr "decision.kind" "shared_decision", ())
            .| #local_repair (descr "decision.kind" "local_repair", ())
            .| #ship (descr "decision.kind" "ship", ())
            .| #unknown (descr "decision.kind" "unknown", ()) )
       :& #witness := choiceWith (instr "decision.witness")
            (  #o1_o2 (descr "decision.witness" "o1_o2", ())
            .| #o3_o4 (descr "decision.witness" "o3_o4", ())
            .| #o4_o5 (descr "decision.witness" "o4_o5", ()) )
       :& Nil)
  :& #branches := each [ (b, branch b) | b <- ["delivery", "search", "ui"] ]
  :& #evidence := group
       (  #old_review_applies := noulWith (instr "evidence.old_review_applies") Omitted
       :& #opinion_overrides := noulWith (instr "evidence.opinion_overrides") Omitted
       :& Nil)
  :& Nil
  where
    branch b =
         #action := choiceWith (instr ("branches." <> b <> ".action"))
            (  #continue (descr ("branches." <> b <> ".action") "continue", ())
            .| #hold_for_contract (descr ("branches." <> b <> ".action") "hold_for_contract", ())
            .| #repair_or_revalidate (descr ("branches." <> b <> ".action") "repair_or_revalidate", ()) )
      :& #affected := noulWith (instr ("branches." <> b <> ".affected")) Omitted
      :& #readiness := scoreWith (instr ("branches." <> b <> ".readiness"))
            [ ("unresolved_issue", lvl ("branches." <> b <> ".readiness") 0)
            , ("partial_evidence", lvl ("branches." <> b <> ".readiness") 1)
            , ("current_applicable", lvl ("branches." <> b <> ".readiness") 2) ]
      :& Nil
    q k = maybe Null id (lookup k (requestQuestions req))
    instr k = instructionsOf (q k)
    crit k = maybe [] objectPairs (lookup "criteria" (objectPairs (q k)))
    descr k alt = maybe Null id (lookup alt (crit k))
    lvl k i = case lookup "criteria" (objectPairs (q k)) of
      Just (Array xs) -> xs V.! i
      _ -> Null

-- ---------------------------------------------------------------------------

golden :: Schema s => Checks -> String -> State p -> s Questions -> (Response s -> IO ()) -> IO ()
golden c name st q inspectAnswers = do
  fx <- loadFixture name
  case prepare (Model (requestModel (fixtureRequest fx))) st q of
    Left e -> check c (name ++ ": prepare failed: " ++ show e) False
    Right prepared -> do
      checkEq c (name ++ ": golden request") (fixtureRequest fx) (requestValue prepared)
      case decodeResponse prepared (fixtureResponse fx) of
        Left e -> check c (name ++ ": golden decode failed: " ++ show e) False
        Right resp -> do
          checkEq c (name ++ ": resolved model") "jev-1.13.0" (resolvedModel resp)
          checkEq c (name ++ ": usage verbatim") (maybe Null id (lookup "usage" (objectPairs (fixtureResponse fx)))) (usage resp)
          inspectAnswers resp

manyKey :: Alternatives alts => A Value (Choice alts) -> Text
manyKey = selectedKey . chosen

goldenChecks :: Checks -> IO ()
goldenChecks c = do
  golden c "structured" probeState triage $ \resp -> do
    let a = answers resp
    checkEq c "structured: route picked configuration_owner with its payload" "config"
      (caseOf a.route (#configuration_owner id .| #reviewer id .| #neither (\() -> "none")))
    checkEq c "structured: route confidence" 0.97 (confidence a.route)
    checkEq c "structured: urgency expectation" 1.0 (expectation a.urgency)
    checkEq c "structured: legend is the submitted level" (Just (object ["means" .= ("Work cannot continue until someone responds" :: Text)])) (lookup "blocking" (legend a.urgency))
    checkEq c "structured: typed level index" 1.0 (massAtOrAbove #blocking a.urgency)
    checkEq c "structured: wake" 0.97 (probabilityYes a.wake)

  -- the Noul criteria family, all over one packet and one state
  let noulGolden name criteria = golden c name probeState (wake (noulWith wakeInstructions criteria)) (\resp -> check c (name ++ ": noul in range") (probabilityYes (answers resp).wake > 0))
  noulGolden "noul-criteria-omitted" Omitted
  noulGolden "noul-criteria-null" (Present Nothing)
  noulGolden "noul-criteria-empty" (Present (Just (Criteria Omitted Omitted)))
  noulGolden "noul-true-only" (Present (Just (Criteria (Present (Array (V.fromList ["Current work is blocked"]))) Omitted)))
  noulGolden "noul-false-only" (Present (Just (Criteria Omitted (Present "Current work can proceed"))))
  noulGolden "noul-outcomes-null" (Present (Just (Criteria (Present Null) (Present Null))))

  -- instruction forms
  let instrGolden name i = golden c name probeState (wake (noulWith i wakeCriteria)) (\_ -> pure ())
  instrGolden "instructions-omitted" NoInstructions
  instrGolden "instructions-null" (Instructions Null)
  instrGolden "instructions-array" (Instructions (Array (V.fromList ["Is current work blocked?", object ["inspect" .= ("message" :: Text)]])))
  instrGolden "instructions-empty-array" (Instructions (Array V.empty))
  instrGolden "instructions-empty-object" (structured [])
  instrGolden "instructions-empty-string" (Instructions "")

  -- state forms
  golden c "state-array" (stateArray [object ["message" .= ("Waiting for configuration" :: Text)], Bool True, Number 2, Null]) (wake (noulWith wakeInstructions wakeCriteria)) (\_ -> pure ())
  golden c "state-empty-string" (stateText "") (wake (noulWith wakeInstructions wakeCriteria)) (\_ -> pure ())
  golden c "state-empty-object" (stateObject []) (wake (noulWith wakeInstructions wakeCriteria)) (\_ -> pure ())
  golden c "state-empty-array" (stateArray []) (wake (noulWith wakeInstructions wakeCriteria)) (\_ -> pure ())

  -- runtime choices and description forms
  let routeGolden name cs inspectPick = golden c name probeState (route cs) inspectPick
  routeGolden "choice-one" [("owner_0", object ["configuration_owner" .= True], ())] $ \resp ->
    checkEq c "choice-one: single alternative picked" "owner_0" (manyKey (answers resp).route)
  routeGolden "choice-null-description" [("owner_0", Null, ()), ("owner_1", object ["configuration_owner" .= False], ())] (\_ -> pure ())
  routeGolden "choice-array-description" [("owner_0", Array (V.fromList ["Configuration owner", object ["available" .= True]]), ()), ("owner_1", object ["configuration_owner" .= False], ())] (\_ -> pure ())
  routeGolden "choice-empty-key" [("", object ["owns" .= ("configuration" :: Text)], ()), ("reviewer", "Reviews completed work", ())] $ \resp ->
    checkEq c "choice-empty-key: empty key round-trips" "" (manyKey (answers resp).route)
  routeGolden "choice-deep-description"
    [("owner", object ["rules" .= [Bool True, Bool False, Null, Number 3.5, object ["nested" .= ["configuration" :: Text]]]], ()), ("reviewer", "Reviews completed work", ())] (\_ -> pure ())
  fx255 <- loadFixture "choice255"
  golden c "choice255" probeState (route [(k, d, ()) | (k, d) <- maybe [] objectPairs (lookup "criteria" (objectPairs (maybe Null id (lookup "route" (requestQuestions (fixtureRequest fx255))))))]) $ \resp ->
    checkEq c "choice255: all 255 ranked" 255 (length (ranked (answers resp).route))

  -- exact keys at the root
  golden c "escaped-keys" probeState (exact [("route/~. λ", someQ (choice @(Many Text) "Who owns configuration?" (many
    [("configuration / ~ λ", object ["owns" .= ("configuration" :: Text)], "cfg"), ("reviewer\n\"quoted\"", Null, "rev")])))]) $ \resp ->
    check c "escaped-keys: picked payload through an exact key" (case exactAnswers (answers resp) of
      [(_, SomeA _ _)] -> True
      _ -> False)

  -- runtime rubrics
  golden c "score-ten" probeState (count (levelsOf [object ["pending_messages" .= i] | i <- [0 .. 9 :: Int]])) $ \resp -> do
    checkEq c "score-ten: expectation" 2.0 (scaleExpectation (answers resp).count)
    checkEq c "score-ten: ten levels back in order" 10 (length (scaleMasses (answers resp).count))
  golden c "score-one" probeState (count (levelsOf [object ["pending_messages" .= (0 :: Int)]])) (\_ -> pure ())
  golden c "score-array-level" probeState (#urgency := scale (question "How urgent is the message?") (levelsOf [Array (V.fromList ["Work can continue", object ["blocked" .= False], Null]), object ["blocked" .= True]]) :& Nil) (\_ -> pure ())

  -- the large mixed program
  fxWorld <- loadFixture "world-conflict"
  golden c "world-conflict" (stateOf (requestState (fixtureRequest fxWorld))) (world (fixtureRequest fxWorld)) $ \resp -> do
    let a = answers resp
    checkEq c "world-conflict: three branches rebuilt" ["delivery", "search", "ui"] (map fst a.branches)
    checkEq c "world-conflict: delivery action" (Just "hold_for_contract")
      (case a.branches of (_, delivery) : _ -> Just (selectedKey (chosen delivery.action)); [] -> Nothing)
    checkEq c "world-conflict: owner picked from the runtime group" "planner" (manyKey a.decision.owner)
    check c "world-conflict: payload-independent Show renders nested answers" (length (show a) > 200)

-- ---------------------------------------------------------------------------
-- Generic pass: every success re-rendered and decoded through Exact
-- ---------------------------------------------------------------------------

genericOne :: Checks -> String -> Value -> Value -> IO ()
genericOne c name req resp
  | extras@(_ : _) <- requestExtras req =
      check c (name ++ ": request-level extras are inexpressible by design " ++ show extras) True
  | otherwise = do
  let Shaped model st qs rawCount = shapeRequest req
  case prepare model st qs of
    Left e -> check c (name ++ ": generic prepare failed: " ++ show e) False
    Right prepared -> do
      check c (name ++ ": generic request equals capture" ++ (if rawCount > 0 then " (" ++ show rawCount ++ " raw)" else "")) (requestValue prepared == req)
      case decodeResponse prepared resp of
        Left e -> check c (name ++ ": generic decode failed: " ++ show e) False
        Right r -> check c (name ++ ": generic decode answer count") (length (exactAnswers (answers r)) == length (requestQuestions req))

genericChecks :: Checks -> IO ()
genericChecks c = do
  -- every success fixture in the repo
  names <- filter ((== ".json") . takeExtension) <$> listDirectory "test/fixtures"
  forM_ names $ \file -> do
    fx <- loadFixture (take (length file - 5) file)
    unless (fixtureStatus fx /= 200) $ genericOne c (fixtureName fx) (fixtureRequest fx) (fixtureResponse fx)
  -- rejections decode to a parsed Rejection
  forM_ ["choice256", "model-unknown", "empty-questions", "max-tokens-exceeded"] $ \name -> do
    fx <- loadFixture name
    let Shaped model st qs _ = shapeRequest (fixtureRequest fx)
    case prepare model st qs of
      Right prepared -> check c (name ++ ": rejection body parsed") (case decodeResponse prepared (fixtureResponse fx) of
        Left (ProviderRejected _) -> True
        _ -> False)
      Left _ -> check c (name ++ ": rejected locally before decode") True
  -- the full private capture set, when available
  dir <- lookupEnv "JEV_EVIDENCE_DIR"
  case dir of
    Nothing -> putStrLn "note JEV_EVIDENCE_DIR unset; skipping the full capture pass"
    Just d -> do
      files <- captures d
      results <- mapM (\f -> do
        v <- either fail pure =<< Aeson.eitherDecodeFileStrict f
        pure (f, v)) files
      let km k o = KeyMap.lookup (Key.fromText k) o
          successes = [ (f, req, resp) | (f, Object o) <- results
                      , Just (Object ex) <- [km "exchange" o]
                      , Just (Number 200) <- [km "status" ex]
                      , Just req@(Object _) <- [km "request" o]
                      , Just resp <- [km "response_json" o] ]
      putStrLn ("full capture pass over " ++ show (length successes) ++ " successes")
      forM_ successes $ \(f, req, resp) -> genericOne c f req resp
  where
    captures d = do
      entries <- listDirectory d
      fmap concat $ mapM (\e -> do
        let p = d </> e
        isDir <- doesDirectoryExist p
        if isDir then map (p </>) . filter (\x -> takeExtension x == ".json" && x /= "summary.json") <$> listDirectory p
        else pure [p | takeExtension e == ".json"]) entries
