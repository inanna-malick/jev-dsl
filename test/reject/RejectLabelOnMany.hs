{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: #edge is written where the runtime group (Many) of this disjunction stands; use onMany
module RejectLabelOnMany where

import Data.Aeson (Value (..))
import Jev.Operators

-- A runtime group has no static label to handle by.
type Next = "rerun" ::> () :|: Many Int

bad :: Chosen Next -> String
bad a = handle a (#rerun (\() -> "rerun") .| #edge (\(_ :: Int) -> "edge"))
