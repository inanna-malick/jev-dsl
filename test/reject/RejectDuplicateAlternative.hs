{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Jev: duplicate alternative #rerun
module RejectDuplicateAlternative where

import Data.Aeson (Value (..))
import Jev.Operators

st :: State 'Plain
st = stateText "s"

-- Two alternatives with one label would share one wire key.
type Next = "rerun" ::> () :|: "rerun" ::> ()

bad :: Q Value (Choice Next)
bad = choice "?" (#rerun (Null, ()) .| #rerun (Null, ()))
