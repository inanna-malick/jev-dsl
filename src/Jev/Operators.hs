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
-- A packet is written once from its questions and its type is inferred. A
-- cell is a packet of one and two packets join, so there is nothing to
-- terminate and a shared set of questions is an ordinary value:
--
-- > r <- ask sess world
-- >    ( #next    := choice "Most useful next step?"
-- >                    (alt #rerun "Rerun the focused check" c .| alt #ask_model "Needs judgment" h .| many #edges edgeKey edgeText edges)
-- >   :& #enough  := noul "Do the diagnostics establish the mechanism?"
-- >   :& #breadth := score "How far would the fix reach?" (level #local "One check" here .| level #wide "Other callers" there) )
--
-- The response reads by the same labels, and a policy turns an answer into
-- a verdict or a doubt:
--
-- > settle careful r.next (#rerun (\c -> …) .| #ask_model (\h -> …) .| #edges (\k e -> …))
-- > judge strict r.enough
-- > grade 0.5 r.breadth            -- the result written beside the level's wording
-- > explain careful r.next         -- the line a log or a planner reads
-- > r.next.key, r.next.margin, r.enough.yes
--
-- Handlers are matched by label, so they may be written in any order and
-- an alternative added in the middle breaks nothing. Labels are wire ids
-- verbatim. A duplicate label, a missing label on access, a missing or
-- extra handler, and a state field that wording names but the state lacks
-- are compile errors in these words. Wording, runtime candidates and level
-- counts are checked when the request is built.
--
-- No network: 'ask' and 'ask1' take a 'Session'. 'request' and 'decode'
-- are the same operation split, for recording and replay.
module Jev.Operators
  ( -- * Packets
    Packet ((:=), (:&))
    -- * Questions
  , noul, choice, score, each, optional
    -- * Alternatives
  , alt, many, (.|), offered
    -- * Rubrics
  , level, massAtOrAbove
    -- * State
  , state, field, State, Field (toField), FieldPath ((:/)), StatePath
    -- * Answers, as fields: @a.next.key@, @a.enough.yes@. The fields are
    -- all there is: an answer cannot be built or matched, and what it
    -- decides is reached through 'settle', 'judge', 'grade', 'taken'
    -- and 'takenUnder'.
  , Yes (yes), Chosen (key, mass, margin, confidence, masses), Scored (expectation, confidence, masses)
    -- * Acting on answers
  , settle, takenUnder, judge, holds, grade, graded, explain, handle, contenders, taken
  , Policy (..), lenient, careful, strict, Lenient, Careful, Strict
  , Settled (..), Doubt (..), Cause (..), Weighed
    -- * Uniform payloads: the continuation is the payload
  , Carries (mapCarried), Retarget, Uniform, uniform, mapUniform, withUniform, branches
    -- * Asking
  , ask, ask1, session, Session, jevLatest, answers, usage, Usage (..), resolvedModel, diagnostics
  , JevError (..), PrepError (..), DecodeError (..), Rejection (..), ValidationIssue (..)
    -- * Recording and replay: the same operation split
  , request, decode
    -- * Types, for signatures only
  , type (::=), type (:&), type (::>), type (::*), type (:|:), Offers, Handlers, Handles, Rubric
  , Noul, Choice, Score, Each, Optional, Group
  , Q, Questions, Answers, Fields, type (:-), Model, Response
  , Schema, Unique, Label, AltsOk, RubricOk
  ) where

import Data.Aeson (Value (String), ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Kind (Type)
import Data.Text (Text)
import GHC.TypeLits (KnownNat, KnownSymbol)
import Jev.Aeson ()
import qualified Jev.Core as Core
import Jev.Core
  ( Yes (yes), Chosen (key, mass, margin, confidence, masses), Scored (expectation, confidence, masses)
  , Alternatives, AltsOk, RubricOk, Carries (mapCarried), Cause (..), Choice, DecodeError (..), Doubt (..), Each, Optional, Field, Group
  , FieldPath ((:/)), StatePath
  , Handles, JevError (..), Label, Lenient, Careful, Strict, Model, Noul, Packet (..), Retarget, Schema, Settled (..)
  , uniform, mapUniform
  , PrepError (..), Q, Score, Unique, Weighed, type (:-), type (::=), type (:&), type (::>), type (::*), type (:|:), Policy (..)
  , Rejection (..), ValidationIssue (..)
  )

type Questions = Core.Questions Value
type Answers = Core.Answers Value
type Fields = Core.Fields Value
type State t = Core.State Value t
type Response s = Core.Response Value s
type Session m = Core.Session m Value

-- | A disjunction whose alternatives all carry the same kind of thing,
-- with the chain itself kept out of sight. 'withUniform' opens one.
type Uniform r = Core.Uniform Value r

-- | Offers for a disjunction: @alt #k wording payload .| many #g key wording rows@.
type Offers alts = Core.Alts (Core.Offer Value) alts

-- | Handlers for a disjunction, one per alternative, each taking that
-- alternative's payload. A signature names them in declaration order; a
-- list written inline may be in any order, because each is found by its
-- label.
type Handlers r alts = Core.Alts Core.HandlerT (HandlersFor alts r)
type family HandlersFor (alts :: Type) (r :: Type) :: Type where
  HandlersFor (k ::> p) r = k Core.:-> (p -> r)
  HandlersFor (k ::* p) r = k Core.:-> (Text -> p -> r)
  HandlersFor (a :|: b) r = HandlersFor a r :|: HandlersFor b r

-- | A rubric: its levels in order, each with the wording the score sends
-- and the result 'grade' returns when the score lands on it.
type Rubric p levels = Core.Alts (Core.Level Value p) levels

(.|) :: Core.Alts f x -> Core.Alts f rest -> Core.Alts f (x :|: rest)
(.|) = (Core..|)
infixr 4 .|

-- | One alternative: its label, its wording for the provider, its payload
-- for the program.
alt :: KnownSymbol k => Label k -> Text -> p -> Offers (k ::> p)
alt l w p = Core.alt l (String w) p

-- | A runtime group: its label, then a wire key and a wording per row. The
-- row is the payload the handler receives.
many :: KnownSymbol k => Label k -> (a -> Text) -> (a -> Text) -> [a] -> Offers (k ::* a)
many l key wording rows = Core.many l key (String . wording) rows

-- | The keys and wording an offer would send, without building a request.
offered :: Alternatives alts => Offers alts -> [(Text, Text)]
offered o = [(k, w) | (k, String w) <- Core.offered o]

-- | Every branch of a uniform chain: its key, its wording, and what it
-- carries. What 'offered' gives, with the payload beside it.
branches :: Uniform r -> [(Text, Text, r)]
branches u = [(k, w, c) | (k, String w, c) <- Core.branches u]

-- | Open a uniform chain to build a question from it. Its alternatives are
-- existential, so they are named only inside, and the question built there
-- gets every check a written-out chain gets.
withUniform :: Uniform r -> (forall alts. (AltsOk alts, Carries alts r) => Offers alts -> x) -> x
withUniform = Core.withUniform

-- | One level: its label, its wording for the provider, and the result
-- 'grade' returns when the score lands on it. What 'alt' takes, in the same
-- order.
level :: KnownSymbol l => Label l -> Text -> p -> Rubric p l
level l w p = Core.level l (String w) p

-- Questions
noul :: Text -> Q Value Noul
noul = Core.noul

-- | A disjunction. Duplicate labels are a compile error naming the label.
choice :: AltsOk alts => Text -> Offers alts -> Q Value (Choice alts)
choice = Core.choice

-- | An ordered rubric of one to ten levels, lowest first.
score :: RubricOk levels => Text -> Rubric p levels -> Q Value (Score p levels)
score = Core.score

-- | One question per row, keyed at runtime: the per-item battery, written
-- as 'many' is. Each row comes back beside its answer, so there is nothing
-- to look up. Takes a question or a nested packet, exactly as a cell does.
each :: (Core.ToQ x, Core.NestedQ x Value, Core.QJson x ~ Value) => (a -> Text) -> (a -> x) -> [a] -> Q Value (Each a (Core.QKind x))
each = Core.each

-- | An optional question or nested packet. Absence sends nothing and
-- reads back as 'Nothing'; presence reads as 'Just' its answer.
optional :: (Core.ToQ x, Core.NestedQ x Value, Core.QJson x ~ Value) => Maybe x -> Q Value (Optional (Core.QKind x))
optional = Core.optional

-- | The shared input to every question, written the way a packet is. Its
-- fields keep their Haskell types, so a row the state carries is the row a
-- question is built from: @each fst (…) st.posters@.
state :: Unique t => Packet t Fields -> State t
state = Core.state

-- | A checked reference in wording: @field #source st@ or
-- @field (#gate :/ #posters) st@. Renders the path in backticks.
field :: StatePath ks t => FieldPath ks -> State t -> Text
field = Core.field

-- Acting on answers

-- | The winner under a policy through the handler its label names, or a
-- structured doubt. A choice gives no result without a handler for every
-- alternative, and the verdict carries the policy that reached it.
settle :: Handles hs alts r => Policy p -> Chosen alts -> Handlers' hs -> Either Doubt (Settled p r)
settle = Core.settle

-- | 'taken' under a policy, with no handlers to repeat when every
-- alternative already carries the same type of result.
takenUnder :: Carries alts r => Policy p -> Chosen alts -> Either Doubt (Settled p r)
takenUnder = Core.takenUnder

-- | A proposition under a policy: yes, no, or doubt.
judge :: Policy p -> Yes -> Either Doubt (Settled p Bool)
judge = Core.judge

-- | Whether a proposition holds under a policy: a settled yes and nothing
-- else. A doubt is not a no, so both read as 'False'; a caller that must
-- tell them apart uses 'judge' and keeps the line that says why.
holds :: Policy p -> Yes -> Bool
holds = Core.holds

-- | The payload the winner was offered with, when every alternative
-- carries the same kind of thing. Having them all is exhaustiveness by
-- construction, so there is no handler list to write.
taken :: Carries alts r => Chosen alts -> r
taken = Core.taken

-- | The result for the level a score landed on: the highest level whose
-- mass at or above it clears the floor, or the lowest when none does. At a
-- floor of 0.5 that is the median level. The result was written beside the
-- level's wording, so a rubric is never dispatched on by its label strings
-- and there is no list to keep in step.
grade :: Double -> Scored p levels -> p
grade = Core.grade

-- | 'grade', with the label of the level the score landed on, for a ledger
-- line that names it.
graded :: Double -> Scored p levels -> (Text, p)
graded = Core.graded

-- | One line saying why the policy settled the answer, with the numbers
-- behind it. A doubt already carries its own line as @why@.
explain :: Weighed a => Policy p -> a -> Text
explain = Core.explain

-- | The winner through the handler its label names, with no policy: for
-- when the program follows whatever came back.
handle :: Handles hs alts r => Chosen alts -> Handlers' hs -> r
handle = Core.handle

-- | Every alternative at or above a mass floor, best first, each already
-- through the same handlers.
contenders :: Handles hs alts r => Double -> Chosen alts -> Handlers' hs -> [(Double, r)]
contenders = Core.contenders

-- A handler list as written: found by label, so its own order is its type.
type Handlers' hs = Core.Alts Core.HandlerT hs

-- | Read-only choices: which file, which skill.
lenient :: Policy Lenient
lenient = Policy 0.40 0.08 0.50

-- | Starting a worker, or choosing an approach.
careful :: Policy Careful
careful = Policy 0.55 0.20 0.70

-- | Merging, stopping, anything with a receipt.
strict :: Policy Strict
strict = Policy 0.70 0.40 0.85

massAtOrAbove :: KnownNat (Core.Index l levels) => Label l -> Scored p levels -> Double
massAtOrAbove = Core.massAtOrAbove

-- | An answer is a ledger row: @toJSON a.next@.
instance ToJSON Yes where toJSON = preview . Core.NoulA
instance Alternatives alts => ToJSON (Chosen alts) where toJSON = preview . Core.ChoiceA
instance Core.Levels levels => ToJSON (Scored p levels) where toJSON = preview . Core.ScoreA

preview :: Core.Endpoint Value e => Core.A Value e -> Value
preview = Core.previewA

-- | A whole answers packet is a ledger row too: @toJSON (answers resp)@.
instance (Unique t, Schema Value (Packet t)) => ToJSON (Packet t Answers) where
  toJSON = Core.previewSchema

-- The operation
jevLatest :: Model
jevLatest = Core.jevLatest

-- | A transport and the model it is asked for, bound once.
-- @transport :: Value -> m (Either Text Value)@ is anything that posts JSON
-- and hands the body back.
session :: (Value -> m (Either Text Value)) -> Model -> Session m
session = Core.session

-- | Send a packet through a session and read back its typed 'Response'.
ask :: (Monad m, Schema Value s) => Session m -> State t -> s Questions -> m (Either JevError (Response s))
ask = Core.roundTrip

-- | One question, one answer, under the label @value@.
ask1 :: (Monad m, Core.Endpoint Value e) => Session m -> State t -> Q Value e -> m (Either JevError (Answers :- e))
ask1 = Core.jev1

-- | The request body, without sending it.
request :: Schema Value s => Model -> State t -> s Questions -> Either JevError Value
request = Core.request

-- | A response body against the packet that produced the request.
decode :: Schema Value s => s Questions -> Value -> Either JevError (Response s)
decode = Core.decode

-- | The packet, under 'Answers'. A response already reads by its packet's
-- labels, so this is for handing the whole packet to a function.
answers :: Response s -> s Answers
answers = Core.answers

-- | Token counts for one call. Missing or non-numeric fields read as 0.
data Usage = Usage { inputTokens :: Int, outputTokens :: Int } deriving (Show, Eq)

usage :: Response s -> Usage
usage r = Usage (field' "input_tokens") (field' "output_tokens")
  where
    field' :: Text -> Int
    field' k = case Core.usage r of
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
