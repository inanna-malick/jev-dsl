{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Evidence-owning, applicative observations. Composition never merges states.
module Jev.Core.Query
  ( Query, question, packet, scope, at, assessment
  , PreparedQuery, prepareQuery, queryGroups, RequestGroup (..), Occurrence (..)
  , runQuery, executeQuery, replayQuery
  , Evaluation (..), EvaluationError (..), Observation (..), Recording (..)
  , ReplayError (..), recordings, evaluationPreview
  ) where

import Data.List (partition)
import Data.Profunctor (Star (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol)
import Jev.Core.Contract
import Jev.Core.Json
import Jev.Core.Schema

-- | An ordinary free applicative. Each leaf closes over its own state and
-- decoder, including payloads that cannot be compared or serialized.
data Query v a where
  Pure :: a -> Query v a
  Ap :: Query v (b -> a) -> Query v b -> Query v a
  Leaf :: v -> (Path -> Either PrepError [(Text, WireQuestion v)])
       -> (Path -> [(Text, v)] -> Either DecodeError a)
       -> (a -> v) -> Query v a
  Scope :: Text -> Query v a -> Query v a

instance Functor (Query v) where
  fmap f q = Ap (Pure f) q

instance Applicative (Query v) where
  pure = Pure
  (<*>) = Ap

question :: (KnownSymbol k, Endpoint v e)
  => State v t -> Label k -> Q v e -> Query v (Answers v :- e)
question world (_ :: Label k) q =
  unwrapA <$> Leaf (stateValue world)
    (\p -> compileQ (extend p (labelText @k)) q)
    (\p -> decodeA (extend p (labelText @k)) q) previewA

packet :: Schema v s => State v t -> s (Questions v) -> Query v (s (Answers v))
packet world q = Leaf (stateValue world) (`compileSchema` q)
  (\p -> decodeSchema p q) previewSchema

-- | Descriptive path segments; uniqueness comes from occurrence numbers.
scope :: Text -> Query v a -> Query v a
scope = Scope

at :: KnownSymbol k => Label k -> Query v a -> Query v a
at (_ :: Label k) = scope (labelText @k)

-- | A standard profunctor: use 'lmap', 'dimap', 'first'', or ordinary optics.
assessment :: (input -> State v t) -> (State v t -> Query v output)
  -> Star (Query v) input output
assessment project build = Star (build . project)

data Occurrence = Occurrence
  { occurrenceIndex :: Int
  , occurrenceScopes :: [Text]
  , occurrenceQuestions :: [Text]
  } deriving (Eq, Show)

data RequestGroup v = RequestGroup
  { groupRequest :: v
  , groupOccurrences :: [Occurrence]
  } deriving (Eq, Show)

data CompiledLeaf v = CompiledLeaf v Occurrence [(Text, WireQuestion v)]
  ([(Text, v)] -> Either DecodeError v)

data PreparedGroup v = PreparedGroup (RequestGroup v) (Prepared v [(Text, v)])
data PreparedQuery v a = PreparedQuery [PreparedGroup v]
  ([(Text, v)] -> Either DecodeError a)

instance Functor (PreparedQuery v) where
  fmap f (PreparedQuery gs result) = PreparedQuery gs (fmap f . result)

queryGroups :: PreparedQuery v a -> [RequestGroup v]
queryGroups (PreparedQuery gs _) = [g | PreparedGroup g _ <- gs]

-- | Validate the complete independent computation before any transport call.
-- Pure computations and empty traversals need no requests.
prepareQuery :: JsonValue v => Model -> Query v a -> Either PrepError (PreparedQuery v a)
prepareQuery model q = do
  (_, leaves, result) <- compile 0 [] q
  groups <- traverse prepareGroup (groupStates leaves)
  pure (PreparedQuery groups result)
  where
    prepareGroup [] = Left EmptyQuestionMap -- groupStates only produces nonempty groups
    prepareGroup ls@(CompiledLeaf world _ _ _ : _) = do
      let qs = concat [ws | CompiledLeaf _ _ ws _ <- ls]
          validate ws = traverse (\(CompiledLeaf _ o _ check) ->
            (,) (T.pack (show (occurrenceIndex o))) <$> check ws) ls
          decodeGroup ws = (ws,) <$> validate ws
      prepared <- prepareCompiled model world qs decodeGroup (jObject . snd)
      pure (PreparedGroup (RequestGroup (request prepared)
        [o | CompiledLeaf _ o _ _ <- ls]) (fst <$> prepared))

compile :: JsonValue v => Int -> [Text] -> Query v a
  -> Either PrepError (Int, [CompiledLeaf v], [(Text, v)] -> Either DecodeError a)
compile n _ (Pure a) = Right (n, [], const (Right a))
compile n path (Ap f a) = do
  (n', fs, df) <- compile n path f
  (n'', as, da) <- compile n' path a
  pure (n'', fs ++ as, \ws -> df ws <*> da ws)
compile n path (Scope name q) = compile n (path ++ [name]) q
compile n names (Leaf world encode readAnswer preview) = do
  checkState world
  let path = Segments (T.pack (show n) : names)
  qs <- encode path
  let decodeLeaf = readAnswer path
      occurrence = Occurrence n names (map fst qs)
      leafPlan = CompiledLeaf world occurrence qs (fmap preview . decodeLeaf)
  pure (n + 1, [leafPlan | not (null qs)], decodeLeaf)

-- Stable first-occurrence grouping, with the JsonValue owner's equality.
groupStates :: JsonValue v => [CompiledLeaf v] -> [[CompiledLeaf v]]
groupStates [] = []
groupStates (l@(CompiledLeaf world _ _ _) : ls) =
  let (same, rest) = partition (\(CompiledLeaf other _ _ _) -> jEqual world other) ls
  in (l : same) : groupStates rest

data Recording v err = Recording
  { recordedRequest :: v
  , recordedOccurrences :: [Occurrence]
  , recordedOutcome :: Either err v
  } deriving (Eq, Show)

data Observation v err = Observation
  { recording :: Recording v err
  , observationResult :: Either (JevError err) (Response v ())
  }

data ReplayError
  = MissingRequest Int
  | ChangedRequest Int
  | ExtraRequests Int
  deriving (Eq, Show)

data EvaluationError err = CallFailed (JevError err) | ReplayFailed ReplayError
  deriving (Eq, Show)

-- | Zero or many calls. The observation list includes failed attempts and every
-- successful prefix. Mapping a result never changes or demands its evidence.
data Evaluation v err a = Evaluation
  { result :: Either (EvaluationError err) a
  , observations :: [Observation v err]
  }

instance Functor (Evaluation v err) where
  fmap f (Evaluation r os) = Evaluation (fmap f r) os

recordings :: Evaluation v err a -> [Recording v err]
recordings = map recording . observations

runQuery :: (Monad m, JsonValue v) => Session m v err -> Query v a -> m (Evaluation v err a)
runQuery sess q = case prepareQuery (sessionModel sess) q of
  Left err -> pure (Evaluation (Left (CallFailed (Prepare err))) [])
  Right ready -> executeQuery sess ready

executeQuery :: Monad m => Session m v err -> PreparedQuery v a -> m (Evaluation v err a)
executeQuery sess (PreparedQuery groups resultDecoder) = go [] [] groups
  where
    go os ws [] = pure (Evaluation (either (Left . CallFailed . Decode) Right (resultDecoder ws)) (reverse os))
    go os ws (PreparedGroup g p : gs) = do
      raw <- sendRequest sess (groupRequest g)
      let (observation, decoded) = observe g p raw
      case decoded of
        Left err -> pure (Evaluation (Left (CallFailed err)) (reverse (observation : os)))
        Right answer -> go (observation : os) (ws ++ answer) gs

observe :: RequestGroup v -> Prepared v a -> Either err v
  -> (Observation v err, Either (JevError err) a)
observe g p raw =
  let decoded = case raw of
        Left err -> Left (Transport err)
        Right body -> either (Left . Decode) Right (decode p body)
  in (Observation (Recording (groupRequest g) (groupOccurrences g) raw)
        (fmap (fmap (const ())) decoded), fmap answers decoded)

-- | Exact request equality, through the new plan's original decoders. Recording
-- occurrence metadata is descriptive; it never substitutes for request identity.
replayQuery :: JsonValue v => PreparedQuery v a -> [Recording v err] -> Evaluation v err a
replayQuery (PreparedQuery groups resultDecoder) = go 0 [] [] groups
  where
    go _ os ws [] [] = Evaluation
      (either (Left . CallFailed . Decode) Right (resultDecoder ws)) (reverse os)
    go _ os _ [] extra = Evaluation (Left (ReplayFailed (ExtraRequests (length extra)))) (reverse os)
    go n os _ (_ : _) [] = Evaluation (Left (ReplayFailed (MissingRequest n))) (reverse os)
    go n os ws (PreparedGroup g p : gs) (r : rs)
      | not (jEqual (groupRequest g) (recordedRequest r)) =
          Evaluation (Left (ReplayFailed (ChangedRequest n))) (reverse os)
      | otherwise =
          let (observation, decoded) = observe g p (recordedOutcome r)
          in case decoded of
            Left err -> Evaluation
              (Left (if null rs then CallFailed err else ReplayFailed (ExtraRequests (length rs))))
              (reverse (observation : os))
            Right answer -> go (n + 1) (observation : os) (ws ++ answer) gs rs


-- | A payload-free journal view. The host chooses how to render its transport
-- error; typed errors remain in the original evaluation for control flow.
evaluationPreview :: JsonValue v => (err -> v) -> Evaluation v err a -> v
evaluationPreview transportError evaluation = jObject
  [("outcome", either failure (const (jString "completed")) (result evaluation)),
   ("observations", jArray (map observationView (observations evaluation)))]
  where
    failure (CallFailed err) = callFailure err
    failure (ReplayFailed err) = jObject [("replay", jString (T.pack (show err)))]
    callFailure (Transport err) = jObject [("transport", transportError err)]
    callFailure (Prepare err) = jObject [("prepare", jString (T.pack (show err)))]
    callFailure (Decode err) = jObject [("decode", jString (T.pack (show err)))]
    observationView observation =
      let retained = recording observation
      in jObject
        [("request", recordedRequest retained),
         ("occurrences", jArray (map occurrenceView (recordedOccurrences retained))),
         ("rawOutcome", either transportError id (recordedOutcome retained)),
         ("response", either callFailure responseView (observationResult observation))]
    occurrenceView occurrence = jObject
      [("index", jNumber (fromIntegral (occurrenceIndex occurrence))),
       ("scopes", jArray (map jString (occurrenceScopes occurrence))),
       ("questions", jArray (map jString (occurrenceQuestions occurrence)))]
    responseView response = jObject
      [("answers", responsePreview response), ("model", jString (responseModel response)),
       ("usage", rawUsage response),
       ("diagnostics", jArray [jObject [("question", jString k), ("distributionTotal", jNumber total)]
          | DistributionDrift k total <- diagnostics response])]
