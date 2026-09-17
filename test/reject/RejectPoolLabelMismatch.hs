{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Jev: pool #probes placed under label #edges
module RejectPoolLabelMismatch where

import Data.Aeson (Value (..))
import Jev.Operators

st :: State 'Plain
st = stateText "s"

-- A pool is declared under the label that is its name; anything else misleads the reader.
bad :: Either PrepError (Prepared (Packet '["edges" ::= PoolDecl "probes" ()]))
bad = prepare jevLatest (pooled st) (#edges := pool #probes [("k", Null, ())] :& Nil)
