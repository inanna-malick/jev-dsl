{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: results stop after #checkpoint
module RejectMissingLevelHandler where

import Data.Aeson (Value (..))
import Jev.Operators

-- Every level needs a result; a rubric is never dispatched on by its labels,
-- so a level left out is a compile error rather than a silent fallthrough.
type Urgency = "background" :|: "checkpoint" :|: "blocked"

bad :: A Value (Score Urgency) -> String
bad a = grade 0.5 a (level #background "quiet" .| level #checkpoint "soon")
