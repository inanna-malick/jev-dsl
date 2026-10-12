{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Composition (compositionChecks) where

import Check
import Control.Lens (over, toListOf, traverseOf, traversed, _2)
import Control.Selective (select)
import Data.Aeson (Value)
import Data.Functor.Identity (Identity (..))
import Data.IORef
import Data.Profunctor (Star (..), lmap)
import Data.Text (Text)
import qualified Data.Text as T
import Jev.Aeson ()
import qualified Jev.Core as J
import Test.QuickCheck (Args (..), quickCheckWithResult, stdArgs, isSuccess, forAll, chooseInt)

-- A scripted provider interprets the actual prepared wire request, not a
-- separately reconstructed question roster. It also runs with the Mini backend.
answer :: J.JsonValue v => v -> v
answer req = J.jObject
  [ ("model", J.jString "fixture-1")
  , ("usage", J.jObject [("input_tokens", J.jNumber 7)])
  , ("answers", J.jObject [(k, response spec) | (k, spec) <- fields "questions" req]) ]
  where
    response spec = case J.lookupKey "type" spec >>= J.viewText of
      Just "choice" -> let ks = map fst (fields "criteria" spec) in J.jObject
        [("type", J.jString "choice"), ("choice", J.jString (case ks of k : _ -> k; [] -> "")),
         ("confidence", J.jNumber 0.9), ("probabilities", J.jObject
          [(k, J.jNumber (1 / fromIntegral (length ks))) | k <- reverse ks])]
      _ -> J.jObject [("type", J.jString "noul"), ("noul", J.jNumber 0.8)]
    fields k v = maybe [] id (J.lookupKey k v >>= J.viewObject)

world :: Int -> J.State Value ("evidence" J.::= Int)
world n = J.state (#evidence J.:= n)

q :: Int -> J.Query Value Double
q n = J.yes <$> J.question (world n) #supported (J.noul "Supported?")

sess :: J.Session Identity Value Text
sess = J.session (pure . Right . answer) J.jevLatest

eval :: J.Query Value a -> J.Evaluation Value Text a
eval = runIdentity . J.runQuery sess

ready :: J.Query Value a -> J.PreparedQuery Value a
ready = either (error . show) id . J.prepareQuery J.jevLatest

requests :: J.Query Value a -> [J.RequestGroup Value]
requests = J.queryGroups . ready

compositionChecks :: Checks -> IO ()
compositionChecks c = do
  let independent = traverse q [1, 2, 1]
      ev = eval independent
  checkEq c "query: exact-state groups preserve authored order" [2, 1]
    (map (length . J.groupOccurrences) (requests independent))
  checkEq c "query: order survives grouping" (Right [0.8, 0.8, 0.8]) (J.result ev)
  checkEq c "query: actual observations per group" 2 (length (J.observations ev))
  checkEq c "query: empty traversal is zero calls" (0, Right ([] :: [Double]))
    (length (J.observations (eval (traverse q []))), J.result (eval (traverse q [])))
  checkEq c "query: reused value has two hygienic occurrences" [2]
    (map (length . J.groupOccurrences) (requests ((,) <$> q 0 <*> q 0)))
  checkEq c "query: map shares one observation" [1]
    (map (length . J.groupOccurrences) (requests ((\a -> (a,a)) <$> q 0)))
  checkEq c "query: scopes cannot collide with auto occurrence ids" ["0.same.supported", "1.same.supported"]
    (concatMap J.occurrenceQuestions (concatMap J.groupOccurrences
      (requests ((,) <$> J.scope "same" (q 0) <*> J.scope "same" (q 0)))))
  let assess = J.assessment world (\w -> J.yes <$> J.question w #supported (J.noul "Supported?"))
  checkEq c "assessment: lmap and an ordinary traversal retain pairs" (Right [("x" :: Text,0.8),("y",0.8)])
    (J.result (eval (traverseOf (traversed . _2) (runStar (lmap length assess)) [("x","a" :: String),("y","ab")])) )
  calls <- newIORef (0 :: Int)
  let counted body = modifyIORef' calls (+1) >> pure (Right (answer body) :: Either Text Value)
      bad = J.question (J.rawState (J.jBool True :: Value)) #bad (J.noul "bad")
  refused <- J.runQuery (J.session counted J.jevLatest) ((,) <$> q 0 <*> bad)
  checkEq c "query: entire plan validated before transport" 0 =<< readIORef calls
  check c "query: typed preparation failure" (case J.result refused of Left (J.CallFailed (J.Prepare J.BadStateShape)) -> True; _ -> False)
  attempts <- newIORef (0 :: Int)
  let failSecond :: Value -> IO (Either Text Value)
      failSecond body = do
        modifyIORef' attempts (+1)
        n <- readIORef attempts
        pure (if n == 2 then Left "offline" else Right (answer body))
  partial <- J.runQuery (J.session failSecond J.jevLatest) (traverse q [1,2,3])
  checkEq c "query: stops after failed call, retains attempted prefix" (Left (J.CallFailed (J.Transport "offline")),2)
    (J.result partial, length (J.observations partial))
  checkEq c "replay: complete structural request matches" (J.result ev)
    (J.result (J.replayQuery (ready independent) (J.recordings ev)))
  check c "replay: changed evidence refused" (case J.result (J.replayQuery (ready (q 99)) (take 1 (J.recordings ev))) of Left (J.ReplayFailed (J.ChangedRequest 0)) -> True; _ -> False)
  check c "replay: missing call refused" (case J.result (J.replayQuery (ready independent) (take 1 (J.recordings ev))) of Left (J.ReplayFailed (J.MissingRequest 1)) -> True; _ -> False)
  check c "replay: extra call refused" (case J.result (J.replayQuery (ready independent) (J.recordings ev ++ J.recordings ev)) of Left (J.ReplayFailed (J.ExtraRequests 2)) -> True; _ -> False)
  checkEq c "replay: failed transport reproduces partial evidence" (J.result partial)
    (J.result (J.replayQuery (ready (traverse q [1,2,3])) (J.recordings partial)))
  let action n = J.taken <$> J.question (world 0) #act
        (J.choice "Choose" (J.alt #only "Only" ((+ n) :: Int -> Int)))
      actionEv = eval ((,) <$> action 1 <*> action 9)
  checkEq c "query: same wire shape keeps distinct original closures" (Right (11,19))
    (fmap (\(f,g) -> (f 10,g 10)) (J.result actionEv))
  checkEq c "replay: changed payload, identical wire uses new decoder" (Right 52)
    (fmap ($ 10) (J.result (J.replayQuery (ready (action 42)) (J.recordings (eval (action 1))))))
  let lazyEv = fmap (const (error "mapped action demanded" :: IO ())) ev
  checkEq c "evaluation: evidence inspection does not demand mapped action" 2 (length (J.observations lazyEv))
  checkEq c "evaluation: journal does not demand mapped actions"
    (J.evaluationPreview J.jString ev) (J.evaluationPreview J.jString lazyEv)
  let sameState = J.question (J.rawState (J.jObject [("a",J.jNumber 1),("b",J.jNumber 2)] :: Value)) #x (J.noul "Ready?")
      reorderedState = J.question (J.rawState (J.jObject [("b",J.jNumber 2),("a",J.jNumber 1)] :: Value)) #x (J.noul "Ready?")
  checkEq c "query: grouping uses structural state equality" 1
    (length (requests ((,) <$> sameState <*> reorderedState)))
  checkEq c "query: optional empty leaf makes no call" (Right Nothing,0)
    (fmap (fmap J.yes) (J.result (eval (J.question (world 0) #absent (J.optional (Nothing :: Maybe (J.Q Value J.Noul)))))),
     length (requests (J.question (world 0) #absent (J.optional (Nothing :: Maybe (J.Q Value J.Noul))))))
  let offers = J.alt #exit (J.jString "exit" :: Value) False J..|
        J.many #rows fst (J.jString . fst) [("a",1 :: Int),("b",2)]
      mapped = over (J.payloads #rows) (\(k,n) -> (k,show n)) offers
  checkEq c "optic: type-changing runtime group" [("a","1"),("b","2")]
    (toListOf (J.payloads #rows) mapped)
  checkEq c "optic: payload edits leave provider request unchanged" (J.offered offers) (J.offered mapped)
  checkEq c "optic: other branch remains intact" [False] (toListOf (J.payloads #exit) mapped)
  let repertoire = J.uniform (J.alt #first (J.jString "first" :: Value) (1 :: Int)
        J..| J.many #rest (T.pack . show) (const (J.jString "rest")) [2,3])
  checkEq c "uniform: traverse keeps shape and authored order" [2,3,4]
    (foldr (:) [] (runIdentity (traverse (Identity . (+1)) repertoire)))
  let dist = J.normalize (J.weights [(2,"a" :: Text),(2,"b"),(1,"c")])
  checkEq c "weights: preserve raw scale" 5 (J.totalWeight (J.weights [(2,()),(2,()),(1,())]))
  checkEq c "distribution: stable tied coverage" (Right [(0.4,"a"),(0.4,"b")]) (dist >>= J.covering 0.7)
  checkEq c "distribution: explicit expected value" (Right 0.8) (J.expected (\a -> if a == "c" then 0 else 1) <$> dist)
  checkEq c "distribution: zero total refused" (Left (J.InvalidTotal 0)) (J.normalize (J.weights [(0,())]))
  check c "distribution: negative weight refused" (case J.normalize (J.weights [(-1,())]) of Left (J.InvalidWeight 0 _) -> True; _ -> False)
  check c "distribution: invalid coverage refused" (case dist >>= J.covering 1.1 of Left (J.InvalidCoverage _) -> True; _ -> False)
  let choice = J.question (world 0) #route (J.choice "Tie"
        (J.alt #z (J.jString "z" :: Value) (1 :: Int) J..| J.alt #a (J.jString "a") 2))
  checkEq c "choice distribution: authored order despite reversed provider keys" (Right [(0.5,1),(0.5,2)])
    (fmap (J.weighted . J.carriedWeights) (J.result (eval choice)))
  let condition = J.liftQuery (Right <$> q 0) :: J.Program Value (Either () Double)
      unneeded = J.liftQuery ((\_ () -> error "branch payload demanded") <$> q 0)
      program = select condition unneeded
      skipped = runIdentity (J.runProgram J.SkipUnused sess program)
      batched = runIdentity (J.runProgram J.BatchKnown sess program)
      questionCount = sum . map (sum . map (length . J.occurrenceQuestions) . J.recordedOccurrences) . J.recordings
  checkEq c "selective: unneeded branch skipped" (Right 0.8,1) (J.result skipped, questionCount skipped)
  checkEq c "selective: speculation observes branch without forcing its payload" (Right 0.8,2,1)
    (J.result batched, questionCount batched,length (J.observations batched))
  checkEq c "selective: lazy replay" (J.result skipped)
    (J.result (J.replayProgram J.SkipUnused J.jevLatest program (J.recordings skipped)))
  checkEq c "selective: eager replay" (J.result batched)
    (J.result (J.replayProgram J.BatchKnown J.jevLatest program (J.recordings batched)))
  let needed = select (J.liftQuery ((const (Left ())) <$> q 0)) (J.liftQuery ((\x () -> x + 1) <$> q 1))
      neededEv = runIdentity (J.runProgram J.SkipUnused sess needed)
  checkEq c "selective: selected frontier executed after condition" (Right 1.8,2)
    (J.result neededEv, length (J.observations neededEv))
  let invalidBranch = J.liftQuery ((\_ () -> 2 :: Int) <$> bad)
      skippedInvalid = select (pure (Right (1 :: Int))) invalidBranch
      activeInvalid = select (J.liftQuery (const (Left ()) <$> q 0)) invalidBranch
      runP strategy p = runIdentity (J.runProgram strategy sess p)
  checkEq c "selective: unused malformed branch is never prepared" (Right 1)
    (J.result (runP J.SkipUnused skippedInvalid))
  check c "selective: batch validates speculative branches before transport"
    (case runP J.BatchKnown skippedInvalid of
      J.Evaluation (Left (J.CallFailed (J.Prepare J.BadStateShape))) [] -> True; _ -> False)
  check c "selective: active later refusal keeps earlier observation"
    (case runP J.SkipUnused activeInvalid of
      J.Evaluation (Left (J.CallFailed (J.Prepare J.BadStateShape))) [_] -> True; _ -> False)
  let malformedSession = J.session (\_ -> pure (Right J.jNull :: Either Text Value)) J.jevLatest
      malformed = runIdentity (J.runQuery malformedSession (q 0))
  check c "query: decoding failure records the actual malformed body"
    (case J.result malformed of
      Left (J.CallFailed (J.Decode _)) -> map J.recordedOutcome (J.recordings malformed) == [Right J.jNull]
      _ -> False)
  law c "applicative laws and request association" $ forAll (chooseInt (0, 30)) $ \n ->
    let a = q n; b = q (n+1); d = q n
        lhs = pure (.) <*> ((+) <$> a) <*> ((*) <$> b) <*> d
        rhs = ((+) <$> a) <*> (((*) <$> b) <*> d)
    in requests lhs == requests rhs && J.result (eval lhs) == J.result (eval rhs)
      && requests (pure id <*> a) == requests a
      && requests (id <$> a) == requests a
  law c "payload traversal identity and composition" $ forAll (chooseInt (-100,100)) $ \n ->
    let os = J.alt #x (J.jString "x" :: Value) n J..| J.alt #y (J.jString "y") True
        values = toListOf (J.payloads #x)
    in values (over (J.payloads #x) id os) == values os
      && values (over (J.payloads #x) ((*2) . (+1)) os)
       == values (over (J.payloads #x) (*2) (over (J.payloads #x) (+1) os))
  law c "normalization expected value matches independent arithmetic" $ forAll (chooseInt (1,1000)) $ \n ->
    let raw = J.weights [(fromIntegral n,2 :: Double),(1,5)]
    in case J.normalize raw of
      Left _ -> False
      Right d -> abs (J.expected id d - (2 * fromIntegral n + 5) / (fromIntegral n + 1)) < 1e-12
  where
    law checks name property = do
      outcome <- quickCheckWithResult stdArgs { maxSuccess = 200 } property
      check checks name (isSuccess outcome)
