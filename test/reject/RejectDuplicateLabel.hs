{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Jev: duplicate packet label #enough
module RejectDuplicateLabel where

import Data.Aeson (Value (..))
import Jev.Operators

st :: State 'Plain
st = stateText "s"

-- Two cells with one label: the second would be unreachable.
bad :: Either PrepError (Prepared (Packet '["enough" ::= Noul, "enough" ::= Noul]))
bad = prepare jevLatest st (#enough := noul "?" :& #enough := noul "?" :& Nil)
