{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module RejectBadEndpoint where

import GHC.Generics (Generic)
import Jev

-- Must fail with the focused message: Level is not a question endpoint.
data Bad mode = Bad { oops :: mode :- Level, fine :: mode :- Noul } deriving (Generic)
instance Schema Bad
