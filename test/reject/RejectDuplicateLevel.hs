{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Jev: duplicate label #blocked
module RejectDuplicateLevel where

import Data.Aeson (Value (..))
import Jev.Operators

-- Two levels with one label could not be told apart in an answer.
bad :: Q Value (Score () ("background" :|: "blocked" :|: "blocked"))
bad = score "?" (level #background "" () .| level #blocked "" () .| level #blocked "" ())
