{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: .| associates to the right, so write a .| b .| c without parentheses
module RejectParenthesisedAlternatives where

import Data.Aeson (Value (..))
import Jev.Operators

-- A grouped chain is a mistake the type would otherwise absorb silently.
bad = choice "?" ((alt #a Null () .| alt #b Null ()) .| alt #c Null ())
