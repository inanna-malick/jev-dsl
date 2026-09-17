{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: results continue past the end of the rubric
module RejectExtraLevelHandler where

import Data.Aeson (Value (..))
import Jev.Operators

-- A result for a level the rubric does not have is a mistake, not dead code.
type Urgency = "background" :|: "checkpoint" :|: "blocked"

bad :: A Value (Score Urgency) -> String
bad a = grade 0.5 a
  (level #background "quiet" .| level #checkpoint "soon" .| level #blocked "now" .| level #invalidating "too late")
