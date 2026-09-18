{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Jev: this state has no #failure; it has #source, #checks
module RejectMissingStateField where

import Data.Text (Text)
import Jev.Operators

-- Wording names a state field through the compiler, not through a string
-- that happens to match.

st :: State ("source" ::= Text :& "checks" ::= [Text])
st = state (#source := ("x" :: Text) :& #checks := (["a"] :: [Text]))

bad :: Text
bad = field #failure st
