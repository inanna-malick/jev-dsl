{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Typed records for TypeSafe's Jev, over aeson's 'Value'.
--
-- Author one record with a mode parameter. Under 'Questions' its fields are
-- the questions: wording, structured criteria, local payloads. 'prepare' turns
-- it into the exact request JSON; 'decodeResponse' turns the response JSON
-- back into the same record under 'Answers', with every selection carrying
-- the payload it was declared with and every distribution intact. Labels
-- resolve only against the alternatives this request sent.
--
-- No network. Hand 'requestValue' to any transport and give the body back to
-- 'decodeResponse', or use 'roundTrip' with a @Value -> m (Either Text Value)@.
--
-- This module fixes the JSON type to aeson. "Jev.Core" is the same library
-- polymorphic over a 'JsonValue' class, for environments with another value
-- type.
module Jev
  ( -- * Modes and the operator
    Questions, Answers, Masses, Legend, Handlers, type (:-)
    -- * Endpoints
  , Noul, Choice, Choose, Score, Scale, Each, Group, Many, Raw, Option, Level
    -- * Leaves and schemas
  , Q, A, Schema, Only (..), Exact, exact, exactAnswers, SomeQ, someQ, SomeA, pattern SomeA
    -- * Positions
  , Presence (..), Instructions, Description, NoulCriteria, pattern NoulCriteria, yes, no
  , State, stateOf, stateText, stateObject, stateArray, stateValue
    -- * Builders (total; shapes are checked at 'prepare')
  , noul, noulWith, choice, choiceWith, choose, chooseWith, score, scoreWith, scale
  , each, group, many, rawUnchecked, option, optionWith, optionKeyed, level, levelWith
  , Candidates, candidates, Candidate, candidateKey, candidateDescription, candidatePayload
  , Exit, exit, exitKey, exitDescription, noMatch, deferToModel
  , Levels, levelsOf, given
    -- * Reading answers
  , probabilityYes, yesAbove, noBelow, unsure
  , Matchable, match, Selected, Distribution, withChoice, probabilityOf, selectedKey, masses, confidence
  , Picked, pattern PickedCandidate, pattern PickedExit, picked, pickOr, ranked, chooseRanked, exitMass, chooseConfidence
  , expectation, levelMasses, legend, scoreConfidence
  , scaleExpectation, scaleMasses, scaleConfidence
  , eachAnswers, groupAnswer, manyAnswers, rawAnswer
    -- * The operation
  , Model (..), jevLatest
  , Prepared, prepare, requestValue, preparedQuestions, preparedModel, preparedState
  , Response, answers, resolvedModel, usage, diagnostics
  , decodeResponse, roundTrip, jev1
    -- * Errors
  , PrepError (..), DecodeError (..), Rejection (..), ValidationIssue (..), JevError (..)
  ) where

import Data.Aeson (Value)
import Data.Kind (Constraint)
import Data.Text (Text)
import GHC.Generics (Generic, Rep)
import Jev.Aeson ()
import qualified Jev.Core as Core
import Jev.Core
  ( Choice, Choose, DecodeError (..), Each, Group, Handlers, JevError (..), Level, Many, Masses
  , Model (..), Noul, Only (..), Option, PrepError (..), Presence (..), Raw, Rejection (..), Scale
  , Schema, Score, ValidationIssue (..), type (:-)
  )

-- Type synonyms hide the JSON parameter.
type Questions = Core.Questions Value
type Answers = Core.Answers Value
type Legend = Core.Legend Value
type Q = Core.Q Value
type A = Core.A Value
type SomeQ = Core.SomeQ Value
type SomeA = Core.SomeA Value
type Exact = Core.Exact
type Instructions = Core.Instructions Value
type Description = Core.Description Value
type NoulCriteria = Core.NoulCriteria Value
type State = Core.State Value
type Candidates = Core.Candidates Value
type Candidate = Core.Candidate Value
type Exit = Core.Exit Value
type Levels = Core.Levels Value
type Picked = Core.Picked Value
type Prepared = Core.Prepared Value
type Response = Core.Response Value
type Selected scope = Core.Selected scope Value
type Distribution = Core.Distribution

-- | An alternatives record with an exhaustive handler record for it.
type Matchable opts r =
  ( Generic (opts Questions), Generic (opts (Handlers r))
  , Core.GApply Value (Rep (opts Questions)) (Rep (opts (Handlers r))) r ) :: Constraint

-- Constructors users match on, as pattern synonyms.
pattern SomeA :: () => Core.Endpoint Value e => Q e -> A e -> SomeA
pattern SomeA q a = Core.SomeA q a

pattern PickedCandidate :: Candidate a -> Picked a
pattern PickedCandidate c = Core.PickedCandidate c

pattern PickedExit :: Exit -> Picked a
pattern PickedExit e = Core.PickedExit e
{-# COMPLETE PickedCandidate, PickedExit #-}

pattern NoulCriteria :: Presence Description -> Presence Description -> NoulCriteria
pattern NoulCriteria y n = Core.NoulCriteria y n
{-# COMPLETE NoulCriteria #-}

someQ :: Core.Endpoint Value e => Q e -> SomeQ
someQ = Core.SomeQ

exact :: [(Text, SomeQ)] -> Exact Questions
exact = Core.exact

exactAnswers :: Exact Answers -> [(Text, SomeA)]
exactAnswers = Core.exactAnswers

-- Positions
yes, no :: NoulCriteria -> Presence Description
yes = Core.yes
no = Core.no

stateOf :: Value -> State
stateOf = Core.stateOf

stateText :: Text -> State
stateText = Core.stateText

stateObject :: [(Text, Value)] -> State
stateObject = Core.stateObject

stateArray :: [Value] -> State
stateArray = Core.stateArray

stateValue :: State -> Value
stateValue = Core.stateValue

-- Builders
noul :: Text -> Q Noul
noul = Core.noul

noulWith :: Instructions -> Presence (Maybe NoulCriteria) -> Q Noul
noulWith = Core.noulWith

choice :: Text -> opts Questions -> Q (Choice opts)
choice = Core.choice

choiceWith :: Instructions -> opts Questions -> Q (Choice opts)
choiceWith = Core.choiceWith

choose :: Text -> Candidates a -> [Exit] -> Q (Choose a)
choose = Core.choose

chooseWith :: Instructions -> Candidates a -> [Exit] -> Q (Choose a)
chooseWith = Core.chooseWith

score :: Text -> ls Questions -> Q (Score ls)
score = Core.score

scoreWith :: Instructions -> ls Questions -> Q (Score ls)
scoreWith = Core.scoreWith

scale :: Instructions -> Levels -> Q Scale
scale = Core.scale

each :: [(Text, x)] -> (x -> s Questions) -> Q (Each s)
each = Core.each

group :: s Questions -> Q (Group s)
group = Core.group

many :: [(Text, SomeQ)] -> Q Many
many = Core.many

rawUnchecked :: Value -> Q Raw
rawUnchecked = Core.rawUnchecked

option :: Text -> a -> Q (Option a)
option = Core.option

optionWith :: Description -> a -> Q (Option a)
optionWith = Core.optionWith

optionKeyed :: Text -> Description -> a -> Q (Option a)
optionKeyed = Core.optionKeyed

level :: Text -> Q Level
level = Core.level

levelWith :: Value -> Q Level
levelWith = Core.levelWith

candidates :: [(Text, Description, a)] -> Candidates a
candidates = Core.candidates

candidateKey :: Candidate a -> Text
candidateKey = Core.candidateKey

candidateDescription :: Candidate a -> Description
candidateDescription = Core.candidateDescription

candidatePayload :: Candidate a -> a
candidatePayload = Core.candidatePayload

exit :: Text -> Description -> Exit
exit = Core.Exit

exitKey :: Exit -> Text
exitKey = Core.exitKey

exitDescription :: Exit -> Description
exitDescription = Core.exitDescription

noMatch :: Text -> Exit
noMatch = Core.noMatch

deferToModel :: Text -> Exit
deferToModel = Core.deferToModel

levelsOf :: [Value] -> Levels
levelsOf = Core.levelsOf

given :: Core.Premised e => Text -> Q e -> Q e
given = Core.given

-- Reading answers
probabilityYes :: A Noul -> Double
probabilityYes = Core.probabilityYes

yesAbove :: Double -> A Noul -> Bool
yesAbove = Core.yesAbove

noBelow :: Double -> A Noul -> Bool
noBelow = Core.noBelow

unsure :: Double -> A Noul -> Bool
unsure = Core.unsure

match :: Matchable opts r => A (Choice opts) -> opts (Handlers r) -> r
match = Core.match

withChoice :: A (Choice opts) -> (forall scope. Selected scope opts -> Distribution scope opts -> r) -> r
withChoice = Core.withChoice

probabilityOf :: Selected scope opts -> Distribution scope opts -> Double
probabilityOf = Core.probabilityOf

selectedKey :: A (Choice opts) -> Text
selectedKey = Core.selectedKey

masses :: A (Choice opts) -> opts Masses
masses = Core.masses

confidence :: A (Choice opts) -> Double
confidence = Core.confidence

picked :: A (Choose a) -> Picked a
picked = Core.picked

pickOr :: (Exit -> m r) -> A (Choose a) -> (a -> m r) -> m r
pickOr = Core.pickOr

ranked :: A (Choose a) -> [(Text, Double)]
ranked = Core.ranked

chooseRanked :: A (Choose a) -> [(Candidate a, Double)]
chooseRanked = Core.chooseRanked

exitMass :: A (Choose a) -> [(Text, Double)]
exitMass = Core.exitMass

chooseConfidence :: A (Choose a) -> Double
chooseConfidence = Core.chooseConfidence

expectation :: A (Score ls) -> Double
expectation = Core.expectation

levelMasses :: A (Score ls) -> ls Masses
levelMasses = Core.levelMasses

legend :: A (Score ls) -> ls Legend
legend = Core.legend

scoreConfidence :: A (Score ls) -> Double
scoreConfidence = Core.scoreConfidence

scaleExpectation :: A Scale -> Double
scaleExpectation = Core.scaleExpectation

scaleMasses :: A Scale -> [(Value, Double)]
scaleMasses = Core.scaleMasses

scaleConfidence :: A Scale -> Double
scaleConfidence = Core.scaleConfidence

eachAnswers :: A (Each s) -> [(Text, s Answers)]
eachAnswers = Core.eachAnswers

groupAnswer :: A (Group s) -> s Answers
groupAnswer = Core.groupAnswer

manyAnswers :: A Many -> [(Text, SomeA)]
manyAnswers = Core.manyAnswers

rawAnswer :: A Raw -> Value
rawAnswer = Core.rawAnswer

-- The operation
jevLatest :: Model
jevLatest = Core.jevLatest

prepare :: Schema s => Model -> State -> s Questions -> Either PrepError (Prepared s)
prepare = Core.prepare

requestValue :: Prepared s -> Value
requestValue = Core.requestValue

preparedQuestions :: Prepared s -> s Questions
preparedQuestions = Core.preparedQuestions

preparedModel :: Prepared s -> Model
preparedModel = Core.preparedModel

preparedState :: Prepared s -> State
preparedState = Core.preparedState

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

roundTrip
  :: (Monad m, Schema s)
  => (Value -> m (Either Text Value)) -> Model -> State -> s Questions
  -> m (Either JevError (Response s))
roundTrip = Core.roundTrip

jev1
  :: (Monad m, Schema (Only e))
  => (Value -> m (Either Text Value)) -> Model -> State -> Q e
  -> m (Either JevError (A e))
jev1 = Core.jev1
