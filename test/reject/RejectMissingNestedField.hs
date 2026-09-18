{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
-- expect: Jev: this state has no #missing; it has #posters, #city
module RejectMissingNestedField where

import Data.Text (Text)
import Jev.Operators

bad :: Text
bad = field (#gate :/ #missing)
  (state (#gate := (#posters := True :& #city := ("Greyhaven" :: Text))))
