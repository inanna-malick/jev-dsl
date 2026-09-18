{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: #ask_model is written where the alternative #rerun stands
module RejectMisorderedHandler where

import Data.Aeson (Value (..))
import Jev.Operators

-- Handlers follow declaration order, so a reader can check them against the type.
type Next = "rerun" ::> () :|: "ask_model" ::> ()

bad :: Chosen Next -> String
bad a = handle a (#ask_model (\() -> "ask") .| #rerun (\() -> "rerun"))
