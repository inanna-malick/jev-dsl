{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: no handler for #missing
module RejectSettleMissingHandler where

import Jev.Operators

-- settle gives no result without a handler per alternative: a confident
-- "missing" cannot read as a pass by omission.
type Next = "rerun" ::> () :|: "missing" ::> ()

bad :: Chosen Next -> Either Doubt (Settled Lenient String)
bad a = settle lenient a (#rerun (\() -> "rerun"))
