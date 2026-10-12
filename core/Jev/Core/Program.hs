{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Selective observations, interpreted with an explicit speculation policy.
-- Branches carry values, never host effects. Run a chosen continuation yourself.
module Jev.Core.Program
  ( Program, liftQuery, Strategy (..), runProgram, replayProgram
  , programFrontier
  ) where

import Control.Selective (Selective (..))
import qualified Data.Text as T
import Jev.Core.Json
import Jev.Core.Query
import Jev.Core.Schema (Session, Model, JevError (Prepare))

data Program v a where
  Done :: a -> Program v a
  Lift :: Query v a -> Program v a
  Apply :: Program v (b -> a) -> Program v b -> Program v a
  Select :: Program v (Either a b) -> Program v (a -> b) -> Program v b

instance Functor (Program v) where
  fmap f = Apply (Done f)
instance Applicative (Program v) where
  pure = Done
  (<*>) = Apply
instance Selective (Program v) where
  select = Select

liftQuery :: Query v a -> Program v a
liftQuery = Lift

data Strategy = SkipUnused | BatchKnown deriving (Eq, Show)

-- | The next independent frontier, or a completed value. SkipUnused admits
-- only the condition of a pending select, but both arms of an applicative.
-- BatchKnown exposes all statically known observations in one query.
programFrontier :: Strategy -> Program v a -> Either a (Query v (Program v a))
programFrontier _ (Done a) = Left a
programFrontier BatchKnown p = Right (Done <$> speculate p)
programFrontier SkipUnused p = frontier p

frontier :: Program v a -> Either a (Query v (Program v a))
frontier (Done a) = Left a
frontier (Lift q) = Right (Done <$> q)
frontier (Apply f a) = case (frontier f, frontier a) of
  (Left g, Left b) -> Left (g b)
  (Left g, Right qs) -> Right (fmap (fmap g) qs)
  (Right qs, Left b) -> Right (fmap (fmap ($ b)) qs)
  (Right fs, Right as) -> Right (Apply <$> fs <*> as)
frontier (Select x f) = case frontier x of
  Left (Right b) -> Left b
  Left (Left a) -> frontier (($ a) <$> f)
  Right qs -> Right ((`Select` f) <$> qs)

speculate :: Program v a -> Query v a
speculate (Done a) = pure a
speculate (Lift q) = q
speculate (Apply f a) = speculate f <*> speculate a
speculate (Select x f) = (\e g -> either g id e) <$> speculate x <*> speculate f

-- The final Done uses frontier directly: BatchKnown itself always returns one
-- frontier, including the useful zero-call case.
runProgram :: (Monad m, JsonValue v)
  => Strategy -> Session m v err -> Program v a -> m (Evaluation v err a)
runProgram strategy sess p = case strategy of
  BatchKnown -> runQuery sess (speculate p)
  SkipUnused -> go 0 [] p
  where
    go n prior current = case frontier current of
      Left a -> pure (Evaluation (Right a) prior)
      Right q -> do
        ev <- runQuery sess (scope ("frontier" <> T.pack (show (n :: Int))) q)
        let allObservations = prior ++ observations ev
        case result ev of
          Left err -> pure (Evaluation (Left err) allObservations)
          Right next -> go (n + 1) allObservations next

replayProgram :: JsonValue v
  => Strategy -> Model -> Program v a -> [Recording v err] -> Evaluation v err a
replayProgram strategy model p rs = case strategy of
  BatchKnown -> case prepareQuery model (speculate p) of
    Left err -> Evaluation (Left (CallFailed (Prepare err))) []
    Right ready -> replayQuery ready rs
  SkipUnused -> go 0 [] p rs
  where
    go n prior current remaining = case frontier current of
      Left a -> Evaluation
        (if null remaining then Right a else Left (ReplayFailed (ExtraRequests (length remaining)))) prior
      Right q -> case prepareQuery model (scope ("frontier" <> T.pack (show (n :: Int))) q) of
        Left err -> Evaluation (Left (CallFailed (Prepare err))) prior
        Right ready ->
          let (used, rest) = splitAt (length (queryGroups ready)) remaining
              ev = replayQuery ready used
              allObservations = prior ++ observations ev
          in case result ev of
            Left err -> Evaluation
              (Left (if null rest then err else ReplayFailed (ExtraRequests (length rest)))) allObservations
            Right next -> go (n + 1) allObservations next rest
