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
import Jev.Transport (request)

-- Two cells with one label: the second would be unreachable.
bad :: Either JevError Value
bad = request jevLatest (state "s") (#enough := noul "?" :& #enough := noul "?" :& Nil)
