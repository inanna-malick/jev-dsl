{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | FOR AGENT USE AND REVIEW. The type-operator front over aeson's 'Value'.
--
-- A packet is written once from its questions and its type is inferred:
--
-- > a <- roundTrip transport jevLatest world
-- >    ( #next   := choice @Next "Most useful next step?" (#rerun c .| #ask_model h .| many edges)
-- >   :& #enough := noul "Do the diagnostics establish the mechanism?"
-- >   :& #breadth := score @Breadth "How broadly would the fix alter behavior?"
-- >   :& Nil )
-- > caseOf a.next (#rerun (\c -> …) .| #ask_model (\h -> …) .| onMany (\e -> …))
-- > massAtOrAbove #adjacent a.breadth
--
-- with @type Next = "rerun" ::> Check :? "Rerun the focused check" :|: "ask_model" ::> Handoff :? "…" :|: Many Edge@
-- and @type Breadth = '["localized" :? "…", "adjacent" :? "…", "contract" :? "…"]@.
--
-- Labels are wire ids verbatim. Duplicate labels, a missing label on
-- access, a pool cell whose label is not its name, rubric bounds and
-- duplicate levels are compile errors in these words. Everything about
-- runtime candidates, descriptions, and pool correspondence is checked by
-- 'prepare'. Builders are total; only a 'Prepared' value renders or decodes.
--
-- No network: hand 'requestValue' to any transport and give the body back
-- to 'decodeResponse', or use 'roundTrip'.
module Jev.Operators
  ( -- * Packets
    Packet (..), Cell (..), type (::=), Label (..), (++.)
  , Questions, Answers, Q, A
    -- * Alternatives
  , type (::>), type (:?), type (:|:), Many, Lvl, Offers, Handlers, Alts, (.|), many, manyFrom, onMany, describe, Element (..), Selected
  , Sum, sumOffer, sumOfferKeyed
    -- * Endpoints
  , Noul, Choice, Score, Scale, Each, Group, Dynamic, Raw, PoolDecl
    -- * Instructions and criteria
  , Instructions, pattern NoInstructions, pattern Instructions, pattern Structured, question, structured, about, Presence (..), Criteria (..), noCriteria, yesOnly, noOnly, bothSides
    -- * State
  , State, PoolMode (..), stateOf, stateText, stateObject, stateArray, pooled, stateValue
    -- * Builders
  , noul, noulOn, noulAbout, noulWith, choice, choiceWith, score, scoreWith, scale
  , each, group, dynamic, rawUnchecked, pool, refs, eachIn, askAbout, given
  , Ref, refKey, refDescription, refPayload, Pool, poolEntries, Levels, levelsOf
  , SomeQ, someQ, SomeA, pattern SomeA
    -- * Reading answers
  , probabilityYes, yesAbove, noBelow, unsure
  , chosen, pattern SelSum, selectedKey, ranked, confidence, alternatives, handle, caseOf, accept, acceptOr, Doubt (..), Policy (..), lenient
  , expectation, masses, scoreConfidence, legend, massAtOrAbove, levelOf
  , scaleExpectation, scaleMasses, scaleConfidence
  , rawAnswer, dynamicAnswers
    -- * The operation
  , Model (..), jevLatest
  , Prepared, prepare, requestValue, preview, preparedQuestions, preparedModel
  , Response, answers, resolvedModel, usage, diagnostics
  , decodeResponse, roundTrip, jev1
  , Exact, exact, exactAnswers
    -- * Errors
  , PrepError (..), DecodeError (..), Rejection (..), ValidationIssue (..), JevError (..)
    -- * Constraints that may appear in signatures
  , Schema, Endpoint, Alternatives, Rubric, CellOk, ConName, type (:-)
  ) where

import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import GHC.TypeLits (KnownNat, KnownSymbol)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Jev.Aeson ()
import qualified Jev.Core as Core
import Jev.Core
  ( A, Alternatives, Cell (..), Choice, Criteria (..), DecodeError (..), Doubt (..), Dynamic, Each, PoolMode (..)
  , Element (..), Endpoint, Group, JevError (..), Label (..), Lvl, Many, Model (..), Noul, Packet (..)
  , PoolDecl, Presence (..), PrepError (..), Q, Raw, Rejection (..), Rubric, Scale, Score, Sum, sumOffer, sumOfferKeyed, CellOk, ConName
  , ValidationIssue (..), lenient, type (:-), type (::=), type (::>), type (:?), type (:|:), (++.)
  , Policy (..)
  )

type Questions = Core.Questions Value
type Answers = Core.Answers Value
type Instructions = Core.Instructions Value

pattern NoInstructions :: Instructions
pattern NoInstructions = Core.NoInstructions

pattern Instructions :: Value -> Instructions
pattern Instructions v = Core.Instructions v

pattern Structured :: [(Text, Value)] -> Instructions
pattern Structured kv = Core.Structured kv

{-# COMPLETE NoInstructions, Instructions, Structured #-}
type State p = Core.State p Value
type SomeQ = Core.SomeQ Value
type SomeA = Core.SomeA Value
type Selected = Core.Selected Value
type Ref n = Core.Ref Value n
type Pool n = Core.Pool Value n
type Levels = Core.Levels Value
type Prepared = Core.Prepared Value
type Response = Core.Response Value

-- | Offers for a disjunction: @#k payload .| #k2 payload .| many […]@.
type Offers alts = Alts (Core.Offer Value) alts
-- | Handlers for a disjunction, in declaration order, each taking its
-- alternative's payload: @#k (\p -> …) .| onMany (\e -> …)@.
type Handlers r alts = Alts (Core.Handler Value r) alts
type Alts f alts = Core.Alts f alts
type Exact = Core.Exact
type Schema s = Core.Schema Value s

pattern SomeA :: () => Endpoint Value e => Q Value e -> A Value e -> SomeA
pattern SomeA q a = Core.SomeA q a

someQ :: Endpoint Value e => Q Value e -> SomeQ
someQ = Core.SomeQ

(.|) :: Core.Single x => Alts f x -> Alts f rest -> Alts f (x :|: rest)
(.|) = (Core..|)
infixr 4 .|

many :: [(Text, Value, p)] -> Offers (Many p)
many = Core.many

manyFrom :: KnownSymbol n => Q Value (PoolDecl n a) -> Offers (Many a)
manyFrom = Core.manyFrom

onMany :: (Element Value p -> r) -> Handlers r (Many p)
onMany = Core.onMany

-- Instructions and criteria
question :: Text -> Instructions
question = Core.question

structured :: [(Text, Value)] -> Instructions
structured = Core.structured

about :: Text -> [(Text, Value)] -> Instructions
about = Core.about

noCriteria :: Presence (Maybe (Criteria Value))
noCriteria = Core.noCriteria

yesOnly, noOnly :: Value -> Presence (Maybe (Criteria Value))
yesOnly = Core.yesOnly
noOnly = Core.noOnly

bothSides :: Value -> Value -> Presence (Maybe (Criteria Value))
bothSides = Core.bothSides

-- State
stateOf :: Value -> State 'Core.Plain
stateOf = Core.stateOf

stateText :: Text -> State 'Core.Plain
stateText = Core.stateText

stateObject :: [(Text, Value)] -> State 'Core.Plain
stateObject = Core.stateObject

stateArray :: [Value] -> State 'Core.Plain
stateArray = Core.stateArray

pooled :: State 'Core.Plain -> State 'Core.Pooled
pooled = Core.pooled

stateValue :: State p -> Value
stateValue = Core.stateValue

-- Builders
noul :: Text -> Q Value Noul
noul = Core.noul

noulOn :: Instructions -> Presence (Maybe (Criteria Value)) -> Q Value Noul
noulOn = Core.noulOn

noulAbout :: Text -> Value -> Q Value Noul
noulAbout = Core.noulAbout

noulWith :: Instructions -> Presence (Maybe (Criteria Value)) -> Q Value Noul
noulWith = Core.noulWith

choice :: forall alts. Core.AltsOk alts => Text -> Offers alts -> Q Value (Choice alts)
choice = Core.choice

choiceWith :: forall alts. Core.AltsOk alts => Instructions -> Offers alts -> Q Value (Choice alts)
choiceWith = Core.choiceWith

score :: forall levels. Text -> Q Value (Score levels)
score = Core.score

scoreWith :: forall levels. Instructions -> [(Text, Value)] -> Q Value (Score levels)
scoreWith = Core.scoreWith

scale :: Instructions -> Levels -> Q Value Scale
scale = Core.scale

levelsOf :: [Value] -> Levels
levelsOf = Core.levelsOf

each :: [(Text, s Questions)] -> Q Value (Each s)
each = Core.each

group :: s Questions -> Q Value (Group s)
group = Core.group

dynamic :: [(Text, SomeQ)] -> Q Value Dynamic
dynamic = Core.dynamic

rawUnchecked :: Value -> Q Value Raw
rawUnchecked = Core.rawUnchecked

pool :: Label n -> [(Text, Value, a)] -> Q Value (PoolDecl n a)
pool = Core.pool

refs :: KnownSymbol n => Q Value (PoolDecl n a) -> [Ref n a]
refs = Core.refs

eachIn :: KnownSymbol n => Q Value (PoolDecl n a) -> (Ref n a -> s Questions) -> Q Value (Each s)
eachIn = Core.eachIn

askAbout :: KnownSymbol n => Ref n a -> Text -> Q Value Noul
askAbout = Core.askAbout

refKey :: Ref n a -> Text
refKey = Core.refKey

refDescription :: Ref n a -> Value
refDescription = Core.refDescription

refPayload :: Ref n a -> a
refPayload = Core.refPayload

poolEntries :: Pool n a -> [(Text, Value, a)]
poolEntries = Core.poolEntries

given :: Core.Premised e => Text -> Q Value e -> Q Value e
given = Core.given

-- Reading answers
probabilityYes :: A Value Noul -> Double
probabilityYes = Core.probabilityYes

yesAbove, noBelow, unsure :: Double -> A Value Noul -> Bool
yesAbove = Core.yesAbove
noBelow = Core.noBelow
unsure = Core.unsure

chosen :: A Value (Choice alts) -> Selected alts
chosen = Core.chosen

pattern SelSum :: Text -> t -> Selected (Sum t)
pattern SelSum k t = Core.SelSum k t

ranked :: A Value (Choice alts) -> [(Double, Selected alts)]
ranked = Core.ranked

confidence :: A Value (Choice alts) -> Double
confidence = Core.confidence

alternatives :: A Value (Choice alts) -> [(Text, Double)]
alternatives = Core.alternatives

selectedKey :: Alternatives alts => Selected alts -> Text
selectedKey = Core.selectedKey

handle :: Alternatives alts => Selected alts -> Handlers r alts -> r
handle = Core.handle

caseOf :: Alternatives alts => A Value (Choice alts) -> Handlers r alts -> r
caseOf = Core.caseOf

describe :: Value -> Offers ((k ::> p) :? d) -> Offers ((k ::> p) :? d)
describe = Core.describe

accept :: Alternatives alts => Policy -> A Value (Choice alts) -> Either Doubt (Selected alts)
accept = Core.accept

acceptOr :: Alternatives alts => (Doubt -> r) -> Policy -> A Value (Choice alts) -> Handlers r alts -> r
acceptOr = Core.acceptOr

expectation :: A Value (Score levels) -> Double
expectation = Core.expectation

masses :: A Value (Score levels) -> [(Text, Double)]
masses = Core.masses

scoreConfidence :: A Value (Score levels) -> Double
scoreConfidence = Core.scoreConfidence

legend :: A Value (Score levels) -> [(Text, Value)]
legend = Core.legend

massAtOrAbove :: KnownNat (Core.Index l levels) => Label l -> A Value (Score levels) -> Double
massAtOrAbove = Core.massAtOrAbove

levelOf :: A Value (Score levels) -> Text
levelOf = Core.levelOf

scaleExpectation :: A Value Scale -> Double
scaleExpectation = Core.scaleExpectation

scaleMasses :: A Value Scale -> [(Value, Double)]
scaleMasses = Core.scaleMasses

scaleConfidence :: A Value Scale -> Double
scaleConfidence = Core.scaleConfidence

rawAnswer :: A Value Raw -> Value
rawAnswer (Core.RawA v) = v

dynamicAnswers :: A Value Dynamic -> [(Text, SomeA)]
dynamicAnswers (Core.DynamicA xs) = xs

-- The operation
jevLatest :: Model
jevLatest = Core.jevLatest

prepare :: Schema s => Model -> State p -> s Questions -> Either PrepError (Prepared s)
prepare = Core.prepare

requestValue :: Prepared s -> Value
requestValue = Core.requestValue

-- | The serialized request, pretty-printed. The object's member order is
-- the encoder's; the question list under 'preparedWire' is declaration order.
preview :: Prepared s -> Text
preview = TL.toStrict . TLE.decodeUtf8 . encodePretty . requestValue

preparedQuestions :: Prepared s -> s Questions
preparedQuestions = Core.preparedQuestions

preparedModel :: Prepared s -> Model
preparedModel = Core.preparedModel

answers :: Response s -> s Answers
answers = Core.answers

resolvedModel :: Response s -> Text
resolvedModel = Core.resolvedModel

usage :: Response s -> Value
usage = Core.usage

diagnostics :: Response s -> [Text]
diagnostics = Core.diagnostics

decodeResponse :: Schema s => Prepared s -> Value -> Either DecodeError (Response s)
decodeResponse = Core.decodeResponse

roundTrip :: (Monad m, Schema s) => (Value -> m (Either Text Value)) -> Model -> State p -> s Questions -> m (Either JevError (Response s))
roundTrip = Core.roundTrip

jev1 :: (Monad m, Endpoint Value e, Core.CellOk "value" e) => (Value -> m (Either Text Value)) -> Model -> State p -> Q Value e -> m (Either JevError (Answers :- e))
jev1 = Core.jev1

exact :: [(Text, SomeQ)] -> Exact Questions
exact = Core.exact

exactAnswers :: Exact Answers -> [(Text, SomeA)]
exactAnswers = Core.exactAnswers
