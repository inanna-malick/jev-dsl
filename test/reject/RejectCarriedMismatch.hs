{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Carries ("ask_model" ::> Handoff) ()
module RejectCarriedMismatch where

import Jev.Operators

-- taken reads the payload only when every alternative carries the same
-- kind of thing; a chain of mixed payloads has nothing to hand back.
-- This one is GHC's own message rather than the library's: it names the
-- alternative and the type that does not fit, which is enough.
newtype Handoff = Handoff String
type Next = "rerun" ::> () :|: "ask_model" ::> Handoff

bad :: Chosen Next -> ()
bad = taken
