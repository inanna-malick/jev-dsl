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

-- A pool is declared under the label that is its name; anything else misleads the reader.
bad :: Either JevError Value
bad = request jevLatest (state "s") (#edges := pool #probes [("k", Null, ())] :& Nil)
