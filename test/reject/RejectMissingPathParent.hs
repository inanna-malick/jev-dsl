{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
-- expect: Jev: this state has no #missing; it has #gate
module RejectMissingPathParent where

import Data.Text (Text)
import Jev.Operators

bad :: Text
bad = field (#missing :/ #posters) (state (#gate := (#posters := True)))
