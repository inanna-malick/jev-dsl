{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Jev: duplicate rubric level #blocked
module RejectDuplicateLevel where

import Data.Aeson (Value (..))
import Jev.Operators

st :: State 'Plain
st = stateText "s"

type Urgency = '[ "background" :? "No current action depends on this", Lvl "blocked", Lvl "blocked" ]

bad :: Either PrepError (Prepared (Packet '["urgency" ::= Score Urgency]))
bad = prepare jevLatest st (#urgency := score "?" :& Nil)
