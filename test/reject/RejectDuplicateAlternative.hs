{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Jev: duplicate label #rerun
module RejectDuplicateAlternative where

import Data.Aeson (Value (..))
import Jev.Operators

-- Two alternatives with one label would share one wire key.
bad :: Q Value (Choice ("rerun" ::> () :|: "rerun" ::> ()))
bad = choice "?" (alt #rerun "a" () .| alt #rerun "b" ())
