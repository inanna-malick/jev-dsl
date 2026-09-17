{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The provider's observed request and response contract as position-
-- specific types over an abstract JSON value. Boot packages only. Nothing
-- here knows about records or modes; it is the wire vocabulary the schema
-- layer targets.
--
-- Positions take the author's JSON value directly. Where the provider
-- restricts the outer shape (state, instructions, descriptions, levels) the
-- restriction is checked at preparation and reported with the question key.
module Jev.Core.Contract
  ( -- * Positions
    Presence (..)
  , Instructions
  , Description
  , NoulCriteria (..)
  , State
  , stateOf
  , stateText
  , stateObject
  , stateArray
  , stateValue
  , checkState
  , checkInstructions
  , checkDescription
  , checkLevel
    -- * Wire questions
  , WireQuestion (..)
  , questionValue
    -- * Errors
  , PrepError (..)
  , DecodeError (..)
  , Rejection (..)
  , ValidationIssue (..)
    -- * Wire answers and the response envelope
  , NoulAnswer (..)
  , ChoiceAnswer (..)
  , ScoreAnswer (..)
  , parseNoul
  , parseChoice
  , parseScore
  , Envelope (..)
  , parseEnvelope
  , unit
  , distribution
  , driftOf
  ) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Jev.Core.Json

-- ---------------------------------------------------------------------------
-- Positions
-- ---------------------------------------------------------------------------

-- | Omission is not representable in JSON, and the provider distinguishes an
-- omitted instruction or criteria block from an explicit null.
data Presence a = Omitted | Present a deriving (Eq, Show)

-- | Instructions: omitted, or a value that is null, a string, an object, or
-- an array at the outer level.
type Instructions v = Presence v

-- | A Choice alternative, exit, or Noul side description: null, string,
-- object, or array at the outer level.
type Description v = v

data NoulCriteria v = NoulCriteria
  { yes :: Presence (Description v)
  , no :: Presence (Description v)
  } deriving (Eq, Show)

-- | The shared input to every question. String, object, or array; never
-- null, a bare boolean, or a bare number.
newtype State v = State v

-- | Total; the outer shape is checked at preparation.
stateOf :: v -> State v
stateOf = State

checkState :: JsonValue v => State v -> Either PrepError ()
checkState (State v) = case jView v of
  VString _ -> Right ()
  VObject _ -> Right ()
  VArray _ -> Right ()
  _ -> Left BadStateShape

stateText :: JsonValue v => Text -> State v
stateText = State . jString

stateObject :: JsonValue v => [(Text, v)] -> State v
stateObject = State . jObject

stateArray :: JsonValue v => [v] -> State v
stateArray = State . jArray

stateValue :: State v -> v
stateValue (State v) = v

structured :: JsonValue v => v -> Bool
structured v = case jView v of
  VNull -> True
  VString _ -> True
  VObject _ -> True
  VArray _ -> True
  _ -> False

checkInstructions :: JsonValue v => Text -> Instructions v -> Either PrepError ()
checkInstructions key = \case
  Omitted -> Right ()
  Present v -> if structured v then Right () else Left (BadInstructions key)

checkDescription :: JsonValue v => Text -> Text -> Description v -> Either PrepError ()
checkDescription key alt v = if structured v then Right () else Left (BadDescription key alt)

checkLevel :: JsonValue v => Text -> Int -> v -> Either PrepError ()
checkLevel key ix v = case jView v of
  VString _ -> Right ()
  VObject _ -> Right ()
  VArray _ -> Right ()
  _ -> Left (BadLevel key ix)

-- ---------------------------------------------------------------------------
-- Wire questions
-- ---------------------------------------------------------------------------

data WireQuestion v
  = WNoul (Instructions v) (Presence (Maybe (NoulCriteria v)))
  | WChoice (Instructions v) [(Text, Description v)]
  | WScore (Instructions v) [v]
  | WRaw v

instructionsField :: Instructions v -> [(Text, v)]
instructionsField = \case
  Omitted -> []
  Present v -> [("instructions", v)]

questionValue :: JsonValue v => WireQuestion v -> v
questionValue = \case
  WNoul i c -> jObject ([("type", jString "noul")] ++ instructionsField i ++ criteria c)
  WChoice i alts -> jObject ([("type", jString "choice")] ++ instructionsField i ++ [("criteria", jObject alts)])
  WScore i ls -> jObject ([("type", jString "score")] ++ instructionsField i ++ [("criteria", jArray ls)])
  WRaw v -> v
  where
    criteria = \case
      Omitted -> []
      Present Nothing -> [("criteria", jNull)]
      Present (Just (NoulCriteria y n)) -> [("criteria", jObject (side "true" y ++ side "false" n))]
    side k = \case
      Omitted -> []
      Present d -> [(k, d)]

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Preparation failures. Every question-level error names the flattened
-- question key.
data PrepError
  = EmptyCandidates Text
  | DuplicateKeys Text [Text]
  | ExitCollidesWithCandidate Text Text
  | TooManyAlternatives Text Int
  | BadLevelCount Text Int
  | DuplicateWireKey Text Text
  | DuplicateQuestionPath Text
  | EmptyQuestionMap
  | EmptyQuestionKey Text
  | BadStateShape
  | BadInstructions Text
  | BadDescription Text Text
  | BadLevel Text Int
  deriving (Show, Eq)

-- | A provider rejection, parsed from the observed 400 and 422 bodies.
data Rejection
  = RejectionMessage Text                          -- {"detail": "..."}
  | RejectionError Text (Maybe Text)               -- {"detail": {"error_type", "message"}}
  | RejectionValidation [ValidationIssue]           -- {"detail": [{loc, msg, type}]}
  | RejectionOther
  deriving (Show, Eq)

data ValidationIssue = ValidationIssue
  { issueLocation :: [Text]
  , issueMessage :: Text
  , issueType :: Text
  } deriving (Show, Eq)

-- | Decoding failures. The response is untrusted until every check passes.
data DecodeError
  = ResponseShape Text
  | ProviderRejected Rejection
  | MissingAnswer Text
  | UnexpectedAnswer Text
  | DuplicateAnswer Text
  | WrongKind Text
  | Malformed Text Text
  | UnknownSelection Text Text
  | MissingMass Text Text
  | ExtraMass Text Text
  | LegendMismatch Text
  | ValueOutOfRange Text Text
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Wire answers
-- ---------------------------------------------------------------------------

newtype NoulAnswer = NoulAnswer { noulYes :: Double } deriving (Eq, Show)

data ChoiceAnswer = ChoiceAnswer
  { choiceSelected :: Text
  , choiceMasses :: [(Text, Double)]
  , choiceConfidence :: Double
  } deriving (Eq, Show)

data ScoreAnswer v = ScoreAnswer
  { wireExpectation :: Double
  , wireLegend :: [(Text, v)]
  , wireMasses :: [(Text, Double)]
  , wireConfidence :: Double
  }

field :: JsonValue v => Text -> Text -> v -> Either DecodeError v
field key name v = maybe (Left (Malformed key ("missing " <> name))) Right (lookupKey name v)

numberField :: JsonValue v => Text -> Text -> v -> Either DecodeError Double
numberField key name v = field key name v >>= \x ->
  maybe (Left (Malformed key (name <> " is not a number"))) Right (viewNumber x)

textField :: JsonValue v => Text -> Text -> v -> Either DecodeError Text
textField key name v = field key name v >>= \x ->
  maybe (Left (Malformed key (name <> " is not a string"))) Right (viewText x)

numberMap :: JsonValue v => Text -> Text -> v -> Either DecodeError [(Text, Double)]
numberMap key name v = field key name v >>= \x -> case viewObject x of
  Just kv -> mapM (\(k, n) -> maybe (Left (Malformed key (name <> "." <> k <> " is not a number"))) (Right . (,) k) (viewNumber n)) kv
  Nothing -> Left (Malformed key (name <> " is not an object"))

kind :: JsonValue v => Text -> Text -> v -> Either DecodeError ()
kind key expected v = textField key "type" v >>= \t ->
  if t == expected then Right () else Left (WrongKind key)

parseNoul :: JsonValue v => Text -> v -> Either DecodeError NoulAnswer
parseNoul key v = do
  kind key "noul" v
  p <- numberField key "noul" v
  unit key "noul" p
  Right (NoulAnswer p)

parseChoice :: JsonValue v => Text -> v -> Either DecodeError ChoiceAnswer
parseChoice key v = do
  kind key "choice" v
  ChoiceAnswer <$> textField key "choice" v <*> numberMap key "probabilities" v <*> numberField key "confidence" v

parseScore :: JsonValue v => Text -> v -> Either DecodeError (ScoreAnswer v)
parseScore key v = do
  kind key "score" v
  legend <- field key "legend" v >>= \x ->
    maybe (Left (Malformed key "legend is not an object")) Right (viewObject x)
  ScoreAnswer <$> numberField key "score" v <*> pure legend <*> numberMap key "probabilities" v <*> numberField key "confidence" v

-- ---------------------------------------------------------------------------
-- Response envelope
-- ---------------------------------------------------------------------------

-- | A successful evaluation, or a provider rejection carried whole.
data Envelope v
  = Evaluated { envelopeModel :: Text, envelopeUsage :: v, envelopeAnswers :: [(Text, v)] }
  | Rejected Rejection

parseEnvelope :: JsonValue v => v -> Either DecodeError (Envelope v)
parseEnvelope v = case lookupKey "answers" v of
  Just answersValue -> do
    answers <- maybe (Left (ResponseShape "answers is not an object")) Right (viewObject answersValue)
    model <- maybe (Left (ResponseShape "model is not a string")) Right (lookupKey "model" v >>= viewText)
    let usage = maybe jNull id (lookupKey "usage" v)
    Right (Evaluated model usage answers)
  Nothing
    | Just d <- lookupKey "detail" v -> Right (Rejected (rejection d))
    | Just _ <- lookupKey "error_type" v -> Right (Rejected (rejection v))
    | otherwise -> Left (ResponseShape "neither an evaluation nor a recognizable rejection")
  where
    rejection d = case jView d of
      VString m -> RejectionMessage m
      VObject _ | Just t <- lookupKey "error_type" d >>= viewText ->
        RejectionError t (lookupKey "message" d >>= viewText)
      VArray issues -> RejectionValidation [ ValidationIssue (locOf i) (textOr "msg" i) (textOr "type" i) | i <- issues ]
      _ -> RejectionOther
    textOr k i = maybe "" id (lookupKey k i >>= viewText)
    locOf i = case lookupKey "loc" i >>= viewObjectOrArray of
      Just parts -> [ segment x | x <- parts ]
      Nothing -> []
    viewObjectOrArray x = case jView x of
      VArray xs -> Just xs
      _ -> Nothing
    segment x = case jView x of
      VString t -> t
      VNumber n -> T.pack (show (round n :: Integer))
      _ -> "?"

-- ---------------------------------------------------------------------------
-- Value checks shared by the schema layer
-- ---------------------------------------------------------------------------

unit :: Text -> Text -> Double -> Either DecodeError ()
unit key what x
  | isNaN x || isInfinite x || x < 0 || x > 1 = Left (ValueOutOfRange key what)
  | otherwise = Right ()

-- | Probability keys must equal the submitted key set exactly; every value
-- and the confidence in [0,1]. Sum drift is a diagnostic, not a rejection.
distribution :: Text -> [Text] -> [(Text, Double)] -> Double -> Either DecodeError ()
distribution key expected ms conf = do
  unit key "confidence" conf
  mapM_ (\k -> if k `elem` map fst ms then Right () else Left (MissingMass key k)) expected
  mapM_ (\(k, x) -> if k `elem` expected then unit key k x else Left (ExtraMass key k)) ms
  if length (nub (map fst ms)) /= length ms then Left (ExtraMass key "duplicate") else Right ()

-- | The sum of a raw answer's probabilities, when it has any.
driftOf :: JsonValue v => v -> Maybe Double
driftOf v = do
  ps <- lookupKey "probabilities" v >>= viewObject
  Just (sum [n | (_, x) <- ps, Just n <- [viewNumber x]])
