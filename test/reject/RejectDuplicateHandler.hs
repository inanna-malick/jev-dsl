{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: duplicate handler #rerun
module RejectDuplicateHandler where

import Jev.Operators

-- One alternative, answered twice: the second handler could never run.
type Next = "rerun" ::> () :|: "ask_model" ::> ()

bad :: Chosen Next -> String
bad a = handle a (#rerun (\() -> "rerun") .| #ask_model (\() -> "ask") .| #rerun (\() -> "again"))
