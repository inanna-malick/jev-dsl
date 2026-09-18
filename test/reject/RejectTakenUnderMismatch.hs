{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Carries ("ask_model" ::> Handoff) ()
module RejectTakenUnderMismatch where

import Jev.Operators

newtype Handoff = Handoff String
type Next = "rerun" ::> () :|: "ask_model" ::> Handoff

bad :: Chosen Next -> Either Doubt (Settled Careful ())
bad = takenUnder careful
