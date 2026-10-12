{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Reported weights and explicitly normalized distributions over local values.
-- Mapping never coalesces equal payloads: alternatives retain their identity.
module Jev.Core.Distribution
  ( Weights, weights, weighted, totalWeight
  , Distribution, normalize, probabilities, expected, covering
  , DistributionError (..)
  ) where

import Data.List (sortOn)

newtype Weights a = Weights [(Double, a)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Distribution a = Distribution [(Double, a)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

data DistributionError
  = InvalidWeight Int Double
  | InvalidTotal Double
  | InvalidCoverage Double
  deriving (Eq, Show)

-- | Raw values, deliberately unnormalized. 'normalize' validates them.
weights :: [(Double, a)] -> Weights a
weights = Weights

weighted :: Weights a -> [(Double, a)]
weighted (Weights rows) = rows

totalWeight :: Weights a -> Double
totalWeight = sum . map fst . weighted

normalize :: Weights a -> Either DistributionError (Distribution a)
normalize w = do
  mapM_ check (zip [0..] (weighted w))
  let total = totalWeight w
  if not (finite total) || total <= 0 then Left (InvalidTotal total)
  else Right (Distribution [(p / total, a) | (p, a) <- weighted w])
  where
    check (i, (p, _))
      | finite p && p >= 0 = Right ()
      | otherwise = Left (InvalidWeight i p)

probabilities :: Distribution a -> [(Double, a)]
probabilities (Distribution rows) = rows

expected :: (a -> Double) -> Distribution a -> Double
expected f = sum . map (\(p, a) -> p * f a) . probabilities

-- | Smallest descending-mass prefix reaching the requested coverage. Ties keep
-- authored order. Zero selects nothing; one includes all positive mass.
covering :: Double -> Distribution a -> Either DistributionError [(Double, a)]
covering target d
  | not (finite target) || target < 0 || target > 1 = Left (InvalidCoverage target)
  | target == 0 = Right []
  | target == 1 = Right [row | row@(p, _) <- ordered, p > 0]
  | otherwise = Right (go 0 ordered)
  where
    ordered = sortOn (negate . fst) (probabilities d)
    go _ [] = []
    go total (row@(p, _) : rest)
      | total >= target = []
      | otherwise = row : go (total + p) rest

finite :: Double -> Bool
finite x = not (isNaN x || isInfinite x)
