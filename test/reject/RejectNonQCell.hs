{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: with actual type ‘Int’
module RejectNonQCell where

import Data.Aeson (Value (..))
import Jev.Operators

st :: State 'Plain
st = stateText "s"

-- A cell holds a question, never a bare value.
bad :: Either PrepError (Prepared (Packet '["enough" ::= Noul]))
bad = prepare jevLatest st (#enough := (5 :: Int) :& Nil)
