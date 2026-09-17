{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module RejectWrongPayload where

import GHC.Generics (Generic)
import Jev

data Routes mode = Routes { follow :: mode :- Option Int, stop :: mode :- Option () } deriving (Generic)

-- Must fail: follow's payload is Int, not String.
bad :: A (Choice Routes) -> String
bad answer = match answer Routes { follow = \s -> s ++ "!", stop = \() -> "stop" }
