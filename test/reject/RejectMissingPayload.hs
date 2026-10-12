{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
-- expect: #missing has no alternative
module RejectMissingPayload where
import Data.Functor.Identity
import Jev.Operators
bad = runIdentity (payloads #missing (Identity . show)
  (alt #present "Present" (1 :: Int)))
