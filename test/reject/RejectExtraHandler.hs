{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: #more has no alternative; the alternatives are #rerun, #ask_model
module RejectExtraHandler where

import Jev.Operators

-- A handler for an alternative that was never offered is a mistake, not dead code.
type Next = "rerun" ::> () :|: "ask_model" ::> ()

bad :: Chosen Next -> String
bad a = handle a (#rerun (\() -> "rerun") .| #ask_model (\() -> "ask") .| #more (\() -> "more"))
