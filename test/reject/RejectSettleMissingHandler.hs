{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: #rerun stands alone where the disjunction continues
module RejectSettleMissingHandler where

import Data.Aeson (Value (..))
import Jev.Operators

-- settle gives no result without a handler per alternative: a confident
-- "missing" cannot read as a pass by omission.
type Next = "rerun" ::> () :|: "missing" ::> ()

bad :: Chosen Next -> Either Doubt String
bad a = settle routing a (#rerun (\() -> "rerun"))
