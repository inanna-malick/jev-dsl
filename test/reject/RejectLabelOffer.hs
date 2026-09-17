{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: offers are written alt #rerun wording payload
module RejectLabelOffer where

import Data.Aeson (Value (..))
import Jev.Operators

-- A bare label builds a handler; an offer needs its wording.
bad :: Q Value (Choice ("rerun" ::> ()))
bad = choice "?" (#rerun (Null, ()))
