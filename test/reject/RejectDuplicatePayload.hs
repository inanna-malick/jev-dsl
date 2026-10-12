{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
-- expect: Jev: duplicate label #x
module RejectDuplicatePayload where
import Data.Functor.Identity
import Jev.Operators
bad = runIdentity (payloads #x (Identity . show)
  (alt #x "First" (1 :: Int) .| alt #x "Second" (2 :: Int)))
