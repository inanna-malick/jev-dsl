{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Jev: a rubric declares 11 levels; Jev permits 1 to 10
module RejectRubricCount where

import Data.Aeson (Value (..))
import Jev.Operators

st :: State 'Plain
st = stateText "s"

type Eleven = '[ Lvl "l0", Lvl "l1", Lvl "l2", Lvl "l3", Lvl "l4", Lvl "l5", Lvl "l6", Lvl "l7", Lvl "l8", Lvl "l9", Lvl "l10" ]

bad :: Either PrepError (Prepared (Packet '["count" ::= Score Eleven]))
bad = prepare jevLatest st (#count := score "?" :& Nil)
