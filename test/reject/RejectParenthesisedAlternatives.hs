{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: chain alternatives with .| (right-associated, without parentheses)
module RejectParenthesisedAlternatives where

import Data.Aeson (Value (..))
import Jev.Operators

st :: State 'Plain
st = stateText "s"

type Next = "a" ::> () :|: "b" ::> () :|: "c" ::> ()

bad :: Q Value (Choice Next)
bad = choice "?" ((#a (Null, ()) .| #b (Null, ())) .| #c (Null, ()))
