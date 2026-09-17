{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- expect: field `followCaller` of an alternatives record has type
module RejectNonOptionField where

import GHC.Generics (Generic)
import Jev

newtype Edge = Edge Int

-- followCaller forgot `mode :- Option`.
data Routes mode = Routes { followCaller :: Edge, stop :: mode :- Option () } deriving (Generic)

data Bad mode = Bad { next :: mode :- Choice Routes } deriving (Generic)
instance Schema Bad
