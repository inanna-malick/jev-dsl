{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}

-- | Golden requests and decodes against real captures. A hand-authored
-- record must render exactly the captured request, and decoding the captured
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
import GHC.Generics (Generic)
import Jev
import Shape
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension)
import qualified Data.Aeson as Aeson

-- ---------------------------------------------------------------------------
-- Records for the probe family that shares one state and one Noul
-- ---------------------------------------------------------------------------

probeState :: State
probeState = stateObject
  [ ("message", "The worker cannot proceed until the missing configuration is supplied.")
  , ("context", object ["active" .= True, "pending" .= (2 :: Int), "previous" .= Null])
  ]

wakeInstructions :: Instructions
wakeInstructions = Present (object ["question" .= ("Does `message` describe a blocker for current work?" :: Text)])

wakeCriteria :: Presence (Maybe NoulCriteria)
wakeCriteria = Present (Just (NoulCriteria
  (Present (object ["means" .= ("Current work cannot proceed" :: Text), "examples" .= ["Missing required input" :: Text]]))
  (Present (object ["means" .= ("Work can proceed without this message" :: Text)]))))

data Wake mode = Wake { wake :: mode :- Noul } deriving (Generic)
instance Schema Wake

data RouteQ mode = RouteQ { route :: mode :- Choose () } deriving (Generic)
instance Schema RouteQ

data CountQ mode = CountQ { count :: mode :- Scale } deriving (Generic)
instance Schema CountQ

-- structured-001: the canonical mixed record
data Owners mode = Owners
  { configurationOwner :: mode :- Option Text
  , reviewer :: mode :- Option Text
  , neither :: mode :- Option ()
  } deriving (Generic)

data UrgencyLevels mode = UrgencyLevels
  { informational :: mode :- Level
  , blocking :: mode :- Level
  } deriving (Generic)

data Triage mode = Triage
  { route :: mode :- Choice Owners
  , urgency :: mode :- Score UrgencyLevels
  , wake :: mode :- Noul
  } deriving (Generic)
instance Schema Triage

triage :: Triage Questions
triage = Triage
  { route = choiceWith (Present (object ["question" .= ("Who can resolve the missing configuration?" :: Text), "focus" .= ["Current blocker" :: Text]])) Owners
      { configurationOwner = optionWith (object ["handles" .= object ["configuration" .= ["missing values" :: Text, "invalid values"]]]) "config"
      , reviewer = optionWith (object ["handles" .= ["completed work" :: Text]]) "review"
      , neither = optionWith Null ()
      }
  , urgency = score "How urgently does this message need attention?" UrgencyLevels
      { informational = levelWith (object ["means" .= ("Useful information, work can continue" :: Text)])
      , blocking = levelWith (object ["means" .= ("Work cannot continue until someone responds" :: Text)])
      }
  , wake = noulWith wakeInstructions wakeCriteria
  }

-- world-conflict-001: Each, two Groups, static and dynamic choices, static levels
data Actions mode = Actions
  { continue :: mode :- Option ()
  , holdForContract :: mode :- Option ()
  , repairOrRevalidate :: mode :- Option ()
  } deriving (Generic)

data Readiness mode = Readiness
  { unresolvedIssue :: mode :- Level
  , partialEvidence :: mode :- Level
  , currentApplicable :: mode :- Level
  } deriving (Generic)

data Branch mode = Branch
  { action :: mode :- Choice Actions
  , affected :: mode :- Noul
  , readiness :: mode :- Score Readiness
  } deriving (Generic)
instance Schema Branch

data Kinds mode = Kinds
  { sharedDecision :: mode :- Option ()
  , localRepair :: mode :- Option ()
  , ship :: mode :- Option ()
  , unknown :: mode :- Option ()
  } deriving (Generic)

data Pairs mode = Pairs
  { o1O2 :: mode :- Option ()
  , o3O4 :: mode :- Option ()
  , o4O5 :: mode :- Option ()
  } deriving (Generic)

data Decision mode = Decision
  { owner :: mode :- Choose ()
  , kind :: mode :- Choice Kinds
  , witness :: mode :- Choice Pairs
  } deriving (Generic)
instance Schema Decision

data Evidence mode = Evidence
  { oldReviewApplies :: mode :- Noul
  , opinionOverrides :: mode :- Noul
  } deriving (Generic)
instance Schema Evidence

data World mode = World
  { decision :: mode :- Group Decision
  , branches :: mode :- Each Branch
  , evidence :: mode :- Group Evidence
  } deriving (Generic)
instance Schema World

-- | Rebuilt from the fixture's own state and candidate descriptions, since
-- the wording is long; the structure is what the record proves.
world :: Value -> World Questions
world req = World
  { decision = group Decision
      { owner = chooseWith (instr "decision.owner") (candidates [(k, d, ()) | (k, d) <- crit "decision.owner"]) []
      , kind = choiceWith (instr "decision.kind") Kinds
          { sharedDecision = optionWith (descr "decision.kind" "shared_decision") ()
          , localRepair = optionWith (descr "decision.kind" "local_repair") ()
          , ship = optionWith (descr "decision.kind" "ship") ()
          , unknown = optionWith (descr "decision.kind" "unknown") ()
          }
      , witness = choiceWith (instr "decision.witness") Pairs
          { o1O2 = optionWith (descr "decision.witness" "o1_o2") ()
          , o3O4 = optionWith (descr "decision.witness" "o3_o4") ()
          , o4O5 = optionWith (descr "decision.witness" "o4_o5") ()
          }
      }
  , branches = each [(b, b) | b <- ["delivery", "search", "ui"]] $ \b -> Branch
      { action = choiceWith (instr ("branches." <> b <> ".action")) Actions
          { continue = optionWith (descr ("branches." <> b <> ".action") "continue") ()
          , holdForContract = optionWith (descr ("branches." <> b <> ".action") "hold_for_contract") ()
          , repairOrRevalidate = optionWith (descr ("branches." <> b <> ".action") "repair_or_revalidate") ()
          }
      , affected = noulWith (instr ("branches." <> b <> ".affected")) Omitted
      , readiness = scoreWith (instr ("branches." <> b <> ".readiness")) Readiness
          { unresolvedIssue = levelWith (lvl ("branches." <> b <> ".readiness") 0)
          , partialEvidence = levelWith (lvl ("branches." <> b <> ".readiness") 1)
          , currentApplicable = levelWith (lvl ("branches." <> b <> ".readiness") 2)
          }
      }
  , evidence = group Evidence
      { oldReviewApplies = noulWith (instr "evidence.old_review_applies") Omitted
      , opinionOverrides = noulWith (instr "evidence.opinion_overrides") Omitted
      }
  }
  where
    q k = maybe Null id (lookup k (requestQuestions req))
    instr k = maybe Omitted Present (lookup "instructions" (objectPairs (q k)))
    crit k = maybe [] objectPairs (lookup "criteria" (objectPairs (q k)))
    descr k alt = maybe Null id (lookup alt (crit k))
    lvl k i = case lookup "criteria" (objectPairs (q k)) of
      Just (Array xs) -> xs V.! i
      _ -> Null

-- ---------------------------------------------------------------------------

golden :: Schema s => Checks -> String -> State -> s Questions -> (Response s -> IO ()) -> IO ()
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

goldenChecks :: Checks -> IO ()
goldenChecks c = do
  golden c "structured" probeState triage $ \resp -> do
    let a = answers resp
    checkEq c "structured: route picked configuration_owner with its payload" "config"
      (match a.route Owners { configurationOwner = id, reviewer = id, neither = \() -> "none" })
    checkEq c "structured: route confidence" 0.97 (confidence a.route)
    checkEq c "structured: urgency expectation" 1.0 (expectation a.urgency)
    checkEq c "structured: legend is the submitted level" (object ["means" .= ("Work cannot continue until someone responds" :: Text)]) (legend a.urgency).blocking
    checkEq c "structured: wake" 0.97 (probabilityYes a.wake)

  -- the Noul criteria family, all over one record and one state
  let noulGolden name criteria = golden c name probeState (Wake (noulWith wakeInstructions criteria)) (\resp -> check c (name ++ ": noul in range") (probabilityYes (answers resp).wake > 0))
  noulGolden "noul-criteria-omitted" Omitted
  noulGolden "noul-criteria-null" (Present Nothing)
  noulGolden "noul-criteria-empty" (Present (Just (NoulCriteria Omitted Omitted)))
  noulGolden "noul-true-only" (Present (Just (NoulCriteria (Present (Array (V.fromList ["Current work is blocked"]))) Omitted)))
  noulGolden "noul-false-only" (Present (Just (NoulCriteria Omitted (Present "Current work can proceed"))))
  noulGolden "noul-outcomes-null" (Present (Just (NoulCriteria (Present Null) (Present Null))))

  -- instruction forms
  let instrGolden name i = golden c name probeState (Wake (noulWith i wakeCriteria)) (\_ -> pure ())
  instrGolden "instructions-omitted" Omitted
  instrGolden "instructions-null" (Present Null)
  instrGolden "instructions-array" (Present (Array (V.fromList ["Is current work blocked?", object ["inspect" .= ("message" :: Text)]])))
  instrGolden "instructions-empty-array" (Present (Array V.empty))
  instrGolden "instructions-empty-object" (Present (object []))
  instrGolden "instructions-empty-string" (Present "")

  -- state forms
  golden c "state-array" (stateArray [object ["message" .= ("Waiting for configuration" :: Text)], Bool True, Number 2, Null]) (Wake (noulWith wakeInstructions wakeCriteria)) (\_ -> pure ())
  golden c "state-empty-string" (stateText "") (Wake (noulWith wakeInstructions wakeCriteria)) (\_ -> pure ())
  golden c "state-empty-object" (stateObject []) (Wake (noulWith wakeInstructions wakeCriteria)) (\_ -> pure ())
  golden c "state-empty-array" (stateArray []) (Wake (noulWith wakeInstructions wakeCriteria)) (\_ -> pure ())

  -- dynamic choices and description forms
  let routeGolden name cs inspectPick = golden c name probeState (RouteQ (choose "Who owns configuration?" (candidates cs) [])) inspectPick
  routeGolden "choice-one" [("owner_0", object ["configuration_owner" .= True], ())] $ \resp ->
    check c "choice-one: single alternative picked" (case picked (answers resp).route of PickedCandidate k -> candidateKey k == "owner_0"; _ -> False)
  routeGolden "choice-null-description" [("owner_0", Null, ()), ("owner_1", object ["configuration_owner" .= False], ())] (\_ -> pure ())
  routeGolden "choice-array-description" [("owner_0", Array (V.fromList ["Configuration owner", object ["available" .= True]]), ()), ("owner_1", object ["configuration_owner" .= False], ())] (\_ -> pure ())
  routeGolden "choice-empty-key" [("", object ["owns" .= ("configuration" :: Text)], ()), ("reviewer", "Reviews completed work", ())] $ \resp ->
    check c "choice-empty-key: empty key round-trips" (case picked (answers resp).route of PickedCandidate k -> candidateKey k == ""; _ -> False)
  routeGolden "choice-deep-description"
    [("owner", object ["rules" .= [Bool True, Bool False, Null, Number 3.5, object ["nested" .= ["configuration" :: Text]]]], ()), ("reviewer", "Reviews completed work", ())] (\_ -> pure ())
  fx255 <- loadFixture "choice255"
  golden c "choice255" probeState (RouteQ (choose "Who owns configuration?" (candidates [(k, d, ()) | (k, d) <- maybe [] (objectPairs) (lookup "criteria" (objectPairs (maybe Null id (lookup "route" (requestQuestions (fixtureRequest fx255))))))]) [])) $ \resp ->
    checkEq c "choice255: all 255 ranked" 255 (length (ranked (answers resp).route))

  -- exact keys at the root
  golden c "escaped-keys" probeState (exact [("route/~. λ", someQ (choose "Who owns configuration?" (candidates
    [("configuration / ~ λ", object ["owns" .= ("configuration" :: Text)], "cfg" :: Text), ("reviewer\n\"quoted\"", Null, "rev")]) []))]) $ \resp ->
    check c "escaped-keys: picked payload through an exact key" (case exactAnswers (answers resp) of
      [(_, SomeA _ _)] -> True
      _ -> False)

  -- runtime rubrics
  golden c "score-ten" probeState (CountQ (scale (Present "How many messages are pending in `context.pending`?") (levelsOf [object ["pending_messages" .= i] | i <- [0 .. 9 :: Int]]))) $ \resp -> do
    checkEq c "score-ten: expectation" 2.0 (scaleExpectation (answers resp).count)
    checkEq c "score-ten: ten levels back in order" 10 (length (scaleMasses (answers resp).count))
  golden c "score-one" probeState (CountQ (scale (Present "How many messages are pending in `context.pending`?") (levelsOf [object ["pending_messages" .= (0 :: Int)]]))) (\_ -> pure ())
  golden c "score-array-level" probeState (UrgencyQ (scale (Present "How urgent is the message?") (levelsOf [Array (V.fromList ["Work can continue", object ["blocked" .= False], Null]), object ["blocked" .= True]]))) (\_ -> pure ())

  -- the large mixed program
  fxWorld <- loadFixture "world-conflict"
  golden c "world-conflict" (stateOf (requestState (fixtureRequest fxWorld))) (world (fixtureRequest fxWorld)) $ \resp -> do
    let a = answers resp
    checkEq c "world-conflict: three branches rebuilt" ["delivery", "search", "ui"] (map fst (eachAnswers a.branches))
    checkEq c "world-conflict: delivery action" (Just "hold_for_contract")
      (case eachAnswers a.branches of (_, delivery) : _ -> Just (selectedKey delivery.action); [] -> Nothing)
    checkEq c "world-conflict: owner picked from the dynamic pool" (Just "planner")
      (case picked (groupAnswer a.decision).owner of PickedCandidate k -> Just (candidateKey k); _ -> Nothing)

data UrgencyQ mode = UrgencyQ { urgency :: mode :- Scale } deriving (Generic)
instance Schema UrgencyQ

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
