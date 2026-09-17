{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | The operation, when a program carries the JSON itself: build a request
-- body, hand it to anything that posts JSON, decode the body that comes
-- back against the same packet.
--
-- 'Jev.Operators.ask' and 'Jev.Operators.ask1' are the same round trip in
-- one call and are what authoring code normally writes; these are here for
-- a transport that inspects, records, or replays the JSON.
module Jev.Transport
  ( request, decode, roundTrip, jev1
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Jev.Core as Core
import Jev.Operators (Answers, JevError, Model, Q, Questions, Response, Schema, State, type (:-))

-- | The request body, without sending it.
request :: Schema s => Model -> State -> s Questions -> Either JevError Value
request = Core.request

-- | A response body against the packet that produced the request.
decode :: Schema s => s Questions -> Value -> Either JevError (Response s)
decode = Core.decode

-- | 'request', the transport, then 'decode'. The same operation as
-- 'Jev.Operators.ask'.
roundTrip :: (Monad m, Schema s) => (Value -> m (Either Text Value)) -> Model -> State -> s Questions -> m (Either JevError (Response s))
roundTrip = Core.roundTrip

-- | One question, one answer. The same operation as 'Jev.Operators.ask1'.
jev1 :: (Monad m, Core.Endpoint Value e, Core.CellOk "value" e) => (Value -> m (Either Text Value)) -> Model -> State -> Q Value e -> m (Either JevError (Answers :- e))
jev1 = Core.jev1
