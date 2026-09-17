{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- The aeson instances for the answer types belong beside the monomorphic
-- front, not beside the polymorphic core that must not depend on aeson.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The authoring surface, over aeson's 'Value'. One import.
--
-- A packet is written once from its questions and its type is inferred:
--
-- > r <- ask transport jevLatest world
-- >    ( #next    := choice "Most useful next step?"
-- >                    (alt #rerun "Rerun the focused check" c .| alt #ask_model "Needs judgment" h .| many edgeKey edgeText edges)
-- >   :& #enough  := noul "Do the diagnostics establish the mechanism?"
-- >   :& Nil )
--
-- Answers come back under the same labels as plain records, and a policy
-- turns them into an action or a doubt:
--
-- > let a = answers r
-- > settle spawning a.next (#rerun (\c -> …) .| #ask_model (\h -> …) .| onMany (\k e -> …))
-- > judge merging a.enough
-- > grade 0.5 a.breadth (level #localized r1 .| level #adjacent r2 .| level #contract r3)
-- > explain spawning a.next       -- the line a log or a planner reads
-- > a.next.key, a.next.margin, a.enough.yes
--
-- Labels are wire ids verbatim. Duplicate labels, a missing label on
-- access, and a handler list that does not match its alternatives are
-- compile errors in these words. Wording, runtime candidates and level
-- counts are checked when the request is built.
--
-- No network: 'ask' and 'ask1' take a transport. 'request' and 'decode'
-- are the same operation split, for recording and replay.
module Jev.Operators
  ( -- * Packets
    Cell ((:=)), Packet ((:&), Nil)
    -- * Questions
  , noul, choice, score, each
    -- * Alternatives
  , alt, many, (.|), onMany
    -- * Rubrics
  , level, massAtOrAbove
    -- * Answers, as fields: @a.next.key@, @a.enough.yes@
    -- ('A' carries them: @yes@; @chosen@, @key@, @mass@, @margin@,
    -- @confidence@, @masses@; @expectation@.)
  , A (..)
    -- * Acting on answers
  , settle, judge, grade, explain, handle, contenders
  , Policy (..), routing, spawning, merging, Doubt (..), Weighed
    -- * Asking
  , ask, ask1, jevLatest, answers, usage, Usage (..), resolvedModel, diagnostics
  , JevError (..), PrepError (..), DecodeError (..), Rejection (..), ValidationIssue (..)
    -- * Recording and replay: the same operation split
  , request, decode
    -- * Types, for signatures only
  , type (::=), type (::>), type (:|:), Many, Offers, Handlers, Rubric, Levels
  , Noul, Choice, Score, Each, Group, Selected
  , Q, Questions, Answers, type (:-), State, state, Model, Response
  , Schema, Alternatives
  ) where

import Data.Aeson (Value, ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import GHC.TypeLits (KnownNat, KnownSymbol)
import Jev.Aeson ()
import qualified Jev.Core as Core
import Jev.Core
  ( A (..), Alternatives, Choice, DecodeError (..), Doubt (..), Each, Group, JevError (..), Label, Many, Model, Noul, Weighed
  , Packet (..), Cell (..), PrepError (..), Q, Score, Selected, type (:-), type (::=), type (::>), type (:|:), Policy (..)
  , Rejection (..), ValidationIssue (..)
  )

type Questions = Core.Questions Value
type Answers = Core.Answers Value
type State = Core.State Value
type Response = Core.Response Value

-- | Offers for a disjunction: @alt #k wording payload .| many key wording rows@.
type Offers alts = Core.Alts (Core.Offer Value) alts
-- | Handlers for a disjunction, in declaration order, each taking its
-- alternative's payload: @#k (\p -> …) .| onMany (\key p -> …)@.
type Handlers r alts = Core.Alts (Core.Handler Value r) alts
-- | Levels of a rubric, in order: @level #low "…" .| level #high "…"@.
type Rubric levels = Core.Alts (Core.Level Value) levels
-- | One result per level, in level order, for 'grade'. The same 'level'
-- builds it.
type Levels r levels = Core.Alts (Core.Level r) levels
type Schema s = Core.Schema Value s

(.|) :: Core.Single x => Core.Alts f x -> Core.Alts f rest -> Core.Alts f (x :|: rest)
(.|) = (Core..|)
infixr 4 .|

alt :: KnownSymbol k => Label k -> Value -> p -> Offers (k ::> p)
alt = Core.alt

-- | A runtime group: a wire key and a wording per row; the row is the
-- payload the handler receives.
many :: (a -> Text) -> (a -> Value) -> [a] -> Offers (Many a)
many = Core.many

onMany :: (Text -> p -> r) -> Handlers r (Many p)
onMany = Core.onMany

-- | One level: its label, and either its wording when asking or its result
-- when grading an answer. Which one is fixed by where it is written.
level :: KnownSymbol l => Label l -> r -> Levels r l
level = Core.level

-- Questions
noul :: Text -> Q Value Noul
noul = Core.noul

choice :: Core.AltsOk alts => Text -> Offers alts -> Q Value (Choice alts)
choice = Core.choice

score :: Core.RubricOk levels => Text -> Rubric levels -> Q Value (Score levels)
score = Core.score

-- | A sub-packet per item, keyed at runtime: the per-item battery.
each :: [(Text, s Questions)] -> Q Value (Each s)
each = Core.each

state :: Value -> State
state = Core.state

-- Acting on answers

-- | The winner under a policy through a handler per alternative, or
-- structured doubt. The only way to consume a choice.
settle :: (Alternatives alts, Core.Match hs alts, hs ~ alts) => Policy -> A Value (Choice alts) -> Handlers r hs -> Either Doubt r
settle = Core.settle

-- | A proposition under a policy: yes, no, or doubt.
judge :: Policy -> A Value Noul -> Either Doubt Bool
judge = Core.judge

-- | The result for the level a score landed on: the highest level whose
-- mass at or above it clears the floor, or the lowest when none does. At a
-- floor of 0.5 that is the median level. A missing, extra, or misordered
-- level is a compile error naming it, so a rubric is never dispatched on by
-- its label strings.
grade :: (Core.Rubric hs, Core.MatchLevels hs levels)
      => Double -> A Value (Score levels) -> Levels r hs -> r
grade = Core.grade

-- | One line saying why the policy settled or doubted the answer, with the
-- numbers behind it. Works on a choice or a Noul.
explain :: Weighed e => Policy -> A Value e -> Text
explain = Core.explain

-- | A selection through a handler per alternative, with no policy. For the
-- contenders, or when the program follows the winner regardless.
handle :: (Alternatives alts, Core.Match hs alts, hs ~ alts) => Selected Value alts -> Handlers r hs -> r
handle = Core.handle

-- | Every alternative at or above a mass floor, best first, as selections
-- the same handlers eliminate.
contenders :: Double -> A Value (Choice alts) -> [(Double, Selected Value alts)]
contenders = Core.contenders

-- | Read-only choices: which file, which skill.
routing :: Policy
routing = Policy 0.40 0.08 0.50

-- | Starting a worker, or choosing an approach.
spawning :: Policy
spawning = Policy 0.55 0.20 0.70

-- | Merging, stopping, anything with a receipt.
merging :: Policy
merging = Policy 0.70 0.40 0.85

massAtOrAbove :: KnownNat (Core.Index l levels) => Label l -> A Value (Score levels) -> Double
massAtOrAbove = Core.massAtOrAbove

-- | An answer is a ledger row: @toJSON a.next@.
instance ToJSON (A Value Noul) where toJSON = Core.previewAnswer
instance Alternatives alts => ToJSON (A Value (Choice alts)) where toJSON = Core.previewAnswer
instance Core.Rubric levels => ToJSON (A Value (Score levels)) where toJSON = Core.previewAnswer

-- | A whole answers packet is a ledger row too: @toJSON (answers resp)@.
instance (Core.Unique fs, Core.PacketSchema Value fs) => ToJSON (Packet fs Answers) where
  toJSON = Core.previewSchema

-- The operation
jevLatest :: Model
jevLatest = Core.jevLatest

-- | Send a packet through a transport and read back its typed 'Response'.
-- @transport :: Value -> m (Either Text Value)@ is anything that posts JSON
-- and hands the body back.
ask :: (Monad m, Schema s) => (Value -> m (Either Text Value)) -> Model -> State -> s Questions -> m (Either JevError (Response s))
ask = Core.roundTrip

-- | One question, one answer, under the label @value@.
ask1 :: (Monad m, Core.Endpoint Value e) => (Value -> m (Either Text Value)) -> Model -> State -> Q Value e -> m (Either JevError (Answers :- e))
ask1 = Core.jev1

-- | The request body, without sending it.
request :: Schema s => Model -> State -> s Questions -> Either JevError Value
request = Core.request

-- | A response body against the packet that produced the request.
decode :: Schema s => s Questions -> Value -> Either JevError (Response s)
decode = Core.decode

answers :: Response s -> s Answers
answers = Core.answers

-- | Token counts for one call. Missing or non-numeric fields read as 0.
data Usage = Usage { inputTokens :: Int, outputTokens :: Int } deriving (Show, Eq)

usage :: Response s -> Usage
usage r = Usage (field "input_tokens") (field "output_tokens")
  where
    field :: Text -> Int
    field k = case Core.usage r of
      Aeson.Object o -> case KeyMap.lookup (Key.fromText k) o of
        Just (Aeson.Number n) -> round n
        _ -> 0
      _ -> 0

-- | The model the request resolved to, as reported by the response envelope.
resolvedModel :: Response s -> Text
resolvedModel = Core.responseModel

-- | Distributions that did not sum to one, and the like. Worth a log line.
diagnostics :: Response s -> [Text]
diagnostics = Core.diagnostics
