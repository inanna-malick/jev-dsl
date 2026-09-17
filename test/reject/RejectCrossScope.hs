{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Couldn't match type ‘scope
module RejectCrossScope where

import GHC.Generics (Generic)
import Jev

data Routes mode = Routes { follow :: mode :- Option Int, stop :: mode :- Option () } deriving (Generic)

-- Must fail: a selection from one result applied to another result's masses.
bad :: A (Choice Routes) -> A (Choice Routes) -> Double
bad first second = withChoice first $ \selected _ ->
  withChoice second $ \_ distribution -> probabilityOf selected distribution
