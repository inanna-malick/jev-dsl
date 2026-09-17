{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: use many/manyFrom for offers and onMany for handlers
module RejectLabelOnMany where

import Data.Aeson (Value (..))
import Jev.Operators

st :: State 'Plain
st = stateText "s"

-- A runtime group has no static label to handle by.
type Next = "rerun" ::> () :|: Many Int

bad :: A Value (Choice Next) -> String
bad a = caseOf a (#rerun (\() -> "rerun") .| #edge (\(_ :: Int) -> "edge"))
