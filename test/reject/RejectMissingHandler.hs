{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: #rerun stands alone where the disjunction continues; chain handlers with .|
module RejectMissingHandler where

import Data.Aeson (Value (..))
import Jev.Operators

-- Every alternative needs a handler; a lone handler cannot stand for two.
type Next = "rerun" ::> () :|: "ask_model" ::> ()

bad :: A Value (Choice Next) -> String
bad a = handle a (#rerun (\() -> "rerun"))
