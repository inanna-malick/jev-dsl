{-# LANGUAGE OverloadedLabels #-}
-- expect: Jev: a cell holds a question or a nested packet; this is Int
module RejectOptionalNonQuestion where

import Jev.Operators

bad :: Either JevError ()
bad = () <$ request jevLatest (state (#ready := True))
  (#extra := optional (Just (1 :: Int)))
