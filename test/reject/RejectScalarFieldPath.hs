{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
-- expect: Jev: cannot descend through #city; expected a nested state packet, found Text
module RejectScalarFieldPath where

import Data.Text (Text)
import Jev.Operators

bad :: Text
bad = field (#gate :/ #city :/ #name)
  (state (#gate := (#city := ("Greyhaven" :: Text))))
