{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Jev: no level #critical in this rubric
module RejectMissingLevel where

import Data.Aeson (Value (..))
import Jev.Operators

bad :: A Value (Score () ("background" :|: "blocked")) -> Double
bad = massAtOrAbove #critical
