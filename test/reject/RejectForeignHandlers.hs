{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module RejectForeignHandlers where

import GHC.Generics (Generic)
import Jev

data Routes mode = Routes { follow :: mode :- Option Int, stop :: mode :- Option () } deriving (Generic)
data Other mode = Other { go :: mode :- Option Int, halt :: mode :- Option () } deriving (Generic)

-- Must fail: a handler record for a different alternatives record.
bad :: A (Choice Routes) -> String
bad answer = match answer Other { go = show, halt = \() -> "stop" }
