{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: no handler for #ask_model
module RejectMissingHandler where

import Jev.Operators

-- Every alternative needs a handler; a lone handler cannot stand for two.
type Next = "rerun" ::> () :|: "ask_model" ::> ()

bad :: Chosen Next -> String
bad a = handle a (#rerun (\() -> "rerun"))
