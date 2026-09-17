{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: #background is written where the level #checkpoint stands
module RejectMisorderedLevelHandler where

import Data.Aeson (Value (..))
import Jev.Operators

-- Results follow level order, so a reader can check them against the rubric.
type Urgency = "background" :|: "checkpoint" :|: "blocked"

bad :: A Value (Score Urgency) -> String
bad a = grade 0.5 a (level #checkpoint "soon" .| level #background "quiet" .| level #blocked "now")
