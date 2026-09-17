{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- expect: declares 11 levels; Jev permits 1 to 10
module RejectElevenLevels where

import GHC.Generics (Generic)
import Jev

data Eleven mode = Eleven
  { l0 :: mode :- Level, l1 :: mode :- Level, l2 :: mode :- Level, l3 :: mode :- Level
  , l4 :: mode :- Level, l5 :: mode :- Level, l6 :: mode :- Level, l7 :: mode :- Level
  , l8 :: mode :- Level, l9 :: mode :- Level, l10 :: mode :- Level
  } deriving (Generic)

-- Must fail at compile time: eleven levels.
data Bad mode = Bad { severity :: mode :- Score Eleven } deriving (Generic)
instance Schema Bad
