{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}
-- expect: Fields of ‘Routes’ not initialised
module RejectMissingHandler where

import GHC.Generics (Generic)
import Jev

data Routes mode = Routes { follow :: mode :- Option Int, stop :: mode :- Option () } deriving (Generic)

-- Must fail under the warning-as-error policy: stop has no handler.
bad :: A (Choice Routes) -> String
bad answer = match answer Routes { follow = show }
