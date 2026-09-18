{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
-- expect: Jev: this packet has no #missing; it has #source, #checks
module RejectMissingStateDot where

import Data.Text (Text)
import Jev.Operators

-- Reading a field a state does not have, the way a question is built from
-- one, names the fields it does have.
bad :: Text
bad = (state (#source := ("evidence" :: Text) :& #checks := True)).missing
