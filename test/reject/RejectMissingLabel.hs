{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- expect: Jev: this packet has no #urgency; it has #enough, #more
module RejectMissingLabel where

import Jev.Operators

-- Accessing a label the packet does not have lists the labels it has.
bad :: Packet ("enough" ::= Noul :& "more" ::= Noul) Answers -> Double
bad a = yes a.urgency
