{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- expect: has no Option fields
module RejectEmptyChoice where

import GHC.Generics (Generic)
import Jev

data Nothing' mode = Nothing' deriving (Generic)

-- Must fail: a static Choice with no alternatives.
data Bad mode = Bad { pick :: mode :- Choice Nothing' } deriving (Generic)
instance Schema Bad
