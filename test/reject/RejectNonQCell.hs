{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Jev: a cell holds a question or a nested packet; this is Int
module RejectNonQCell where

import Data.Aeson (Value)
import Data.Text (Text)
import Jev.Operators

-- A cell holds a question or a nested packet, never a bare value.
bad :: Either JevError Value
bad = request jevLatest (state (#s := ("x" :: Text))) (#enough := (5 :: Int))
