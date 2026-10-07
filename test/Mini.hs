{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | A second JSON type, to keep the core honest.
--
-- "Jev.Operators" is one facade over "Jev.Core", and aeson's 'Value' was
-- the only value type the abstraction had ever seen. This is a hand-rolled
-- one with a different shape and no library behind it, driven through the
-- core's own polymorphic verbs with no facade at all. If anything in the
-- core reaches for aeson, or assumes a representation, this stops
-- compiling: @jev-core@ does not depend on aeson, so it cannot.
module Mini (miniChecks) where

import Check
import Data.Coerce (coerce)
import Data.String (IsString (..))
import Data.Text (Text)

import qualified Data.Text as T
import Jev.Core

-- | Structurally different from aeson's: an association list rather than a
-- hash map, and a plain 'Double' rather than a 'Scientific'.
data Mini
  = MNull
  | MBool Bool
  | MNum Double
  | MStr Text
  | MArr [Mini]
  | MObj [(Text, Mini)]
  deriving (Eq, Show)

-- Wording is written as a bare literal under OverloadedStrings, so a value
-- type needs this for the authoring surface to read the way it does with
-- aeson. It is the one thing beyond 'JsonValue' a backend has to supply.
instance IsString Mini where fromString = MStr . T.pack

-- Nothing here is derived or generated. Six constructors and a view.
instance JsonValue Mini where
  jNull = MNull
  jBool = MBool
  jNumber = MNum
  jString = MStr
  jArray = MArr
  jObject = MObj
  jView = \case
    MNull -> VNull
    MBool b -> VBool b
    MNum n -> VNumber n
    MStr s -> VString s
    MArr xs -> VArray xs
    MObj kv -> VObject kv
  -- jEqual is left to the class default, so 'structuralEqual' is exercised
  -- too: object member order must not matter.

newtype Room = Room Text deriving (Eq, Show)

-- A packet with one of each question kind, a runtime group, a static exit,
-- and a per-item battery. No type annotations beyond the domain payload.
inspect :: [Room] -> Packet
  (    "next" ::= Choice ("stop" ::> () :|: "rooms" ::* Room)
   :&  "ready" ::= Noul
   :&  "risk" ::= Score Text ("low" :|: "high")
   :&  "each_room" ::= Each Room Noul ) (Questions Mini)
inspect rooms =
     #next := choice "Which room to search next?"
                (alt #stop "Every room has been searched" () .| many #rooms (\(Room r) -> r) (\(Room r) -> MStr r) rooms)
  :& #ready := noul "Is the search ready to stop?"
  :& #risk := score "How risky is continuing?" (level #low (MStr "safe") "low" .| level #high (MStr "dangerous") "high")
  :& #each_room := each (\(Room r) -> r) (\(Room r) -> noul ("Has " <> r <> " been searched?")) rooms

-- A transport in Mini, answering from the request it was handed.
stub :: Mini -> IO (Either Text Mini)
stub req = pure (Right (MObj
  [ ("model", MStr "mini-1.0")
  , ("usage", MObj [("input_tokens", MNum 11), ("output_tokens", MNum 2)])
  , ("answers", MObj [(k, answer q) | (k, q) <- questionsOf req])
  ]))
  where
    questionsOf v = case lookupKey "questions" v >>= viewObject of
      Just kv -> kv
      Nothing -> []
    answer q = case lookupKey "type" q >>= viewText of
      Just "choice" ->
        let keys = maybe [] (map fst) (lookupKey "criteria" q >>= viewObject)
            best = take 1 (filter (/= "stop") keys) ++ keys
            sel = case best of { k : _ -> k; [] -> "" }
        in MObj [ ("type", MStr "choice"), ("choice", MStr sel), ("confidence", MNum 0.9)
                , ("probabilities", MObj [(k, MNum (if k == sel then 0.8 else 0.2 / fromIntegral (max 1 (length keys - 1)))) | k <- keys]) ]
      Just "score" ->
        let ls = maybe [] id (lookupKey "criteria" q >>= \c -> case jView c of { VArray xs -> Just xs; _ -> Nothing })
        in MObj [ ("type", MStr "score"), ("score", MNum 1), ("confidence", MNum 0.6)
                , ("legend", MObj [(T.pack (show i), l) | (i, l) <- zip [0 :: Int ..] ls])
                , ("probabilities", MObj [(T.pack (show i), MNum 0.5) | i <- [0 .. length ls - 1]]) ]
      _ -> MObj [("type", MStr "noul"), ("noul", MNum 0.9)]

miniChecks :: Checks -> IO ()
miniChecks c = do
  let rooms = [Room "cellar", Room "attic"]
      packet = inspect rooms
  case prepare jevLatest (rawState (MObj [("house", MStr "Greyhaven")])) packet of
    Left e -> check c ("mini: request failed: " ++ show e) False
    Right prepared -> do
      let req = request prepared
      check c "mini: the request is built in a value type the core never saw"
        (case lookupKey "questions" req >>= viewObject of
          Just kv -> length kv == 5
          Nothing -> False)
      check c "mini: a runtime key reaches the wire under its own label"
        ((lookupKey "questions" req >>= lookupKey "next" >>= lookupKey "criteria" >>= lookupKey "cellar") == Just (MStr "cellar"))
      check c "mini: the battery flattens to one question per item"
        ((lookupKey "questions" req >>= lookupKey "each_room.attic") /= Nothing)
  r <- roundTrip (session stub jevLatest) (rawState (MObj [("house", MStr "Greyhaven")])) packet
  case r of
    Left e -> check c ("mini: round trip failed: " ++ show e) False
    Right resp -> do
      let a = answers resp
      checkEq c "mini: the chosen row is the payload the program offered"
        (Room "cellar") (handle a.next (#stop (\() -> Room "none") .| #rooms (\_ room -> room)))
      checkEq c "mini: a noul decodes" 0.9 a.ready.yes
      checkEq c "mini: a score grades through its levels"
        ("high" :: Text) (grade 0.5 a.risk)
      checkEq c "mini: the battery answers under its runtime keys, in packet order"
        ["cellar", "attic"] [name | (Room name, _) <- a.each_room]
      checkEq c "mini: usage and model come back" "mini-1.0" (responseModel resp)
  -- The new authoring forms also run without the aeson facade.
  let world = state (#house := (#rooms := (["cellar"] :: [Text])))
      optionalRoom = #next := optional (Just (choice
        ("Which of " <> field (#house :/ #rooms) world <> " next?")
        (alt #cellar "Search the cellar" (Room "cellar"))))
      custom = either (error . show) id (customPolicy 0.55 0.2 0.7)
  rOptional <- roundTrip (session stub jevLatest) world optionalRoom
  checkEq c "mini: optional choice and policy-aware payload consumption"
    (Right (Just (Right (Room "cellar"))))
    (fmap (\resp -> fmap (fmap settledValue . takenUnder custom) resp.next) rOptional)
  -- the class default equality, which aeson overrides with its own
  check c "mini: structural equality ignores object member order"
    (jEqual (MObj [("a", MNum 1), ("b", MArr [MNull])]) (MObj [("b", MArr [MNull]), ("a", MNum 1)]))
  check c "mini: structural equality still distinguishes values"
    (not (jEqual (MObj [("a", MNum 1)]) (MObj [("a", MNum 2)])))
  -- A prepared value owns its original decoder and answer payloads. These
  -- packets have identical wire questions but deliberately different
  -- program-side functions.
  let preparedPacket :: (Int -> Text) -> Packet
        ("route" ::= Choice ("ship" ::> (Int -> Text) :|: "hold" ::> (Int -> Text)))
        (Questions Mini)
      preparedPacket f = #route := choice "Where should this go?"
        (alt #ship "Ship" f .| alt #hold "Hold" (const "hold"))
      preparedReply = MObj
        [ ("model", MStr "prepared-model")
        , ("usage", MObj [("input_tokens", MNum 0)])
        , ("answers", MObj [("route", MObj
            [("type", MStr "choice"), ("choice", MStr "ship"), ("confidence", MNum 0.9)
            , ("probabilities", MObj [("ship", MNum 0.85), ("hold", MNum 0.1)])])])
        ]
  case ( prepare jevLatest world (preparedPacket (const "first"))
       , prepare jevLatest world (preparedPacket (const "second")) ) of
    (Right first, Right second) -> do
      let pairResult = do
            firstResponse <- decode first preparedReply
            secondResponse <- decode second preparedReply
            pure (handle (answers firstResponse).route (#ship (\payload -> payload 1) .| #hold (\payload -> payload 1)),
                  handle (answers secondResponse).route (#ship (\payload -> payload 1) .| #hold (\payload -> payload 1)))
      checkEq c "mini prepared calls retain distinct same-shaped payload functions"
        (Right ("first", "second")) pairResult
      case decode first preparedReply of
        Left e -> check c ("mini prepared metadata decode: " ++ show e) False
        Right response -> do
          let mapped = mapResponse (\a -> handle a.route (#ship (\f -> f 1) .| #hold (\f -> f 1))) response
          check c "mini response reports typed distribution drift" (case diagnostics response of
            [DistributionDrift "route" total] -> abs (total - 0.95) < 1e-9
            _ -> False)
          checkEq c "mini mapResponse keeps envelope model, usage, and raw usage"
            ("prepared-model", Usage (Just 0) Nothing, MObj [("input_tokens", MNum 0)])
            (responseModel mapped, usage mapped, rawUsage mapped)
          checkEq c "mini mapResponse keeps typed diagnostics" (diagnostics response) (diagnostics mapped)
      checkEq c "mini prepared request includes original nested state"
        (Just (MObj [("rooms", MArr [MStr "cellar"])]))
        (lookupKey "state" (request first) >>= lookupKey "house")
    _ -> check c "mini prepared calls prepare successfully" False
  let oneReply = MObj
        [("model", MStr "prepared-model"), ("usage", MObj [("output_tokens", MNum 2)])
        , ("answers", MObj [("value", MObj [("type", MStr "noul"), ("noul", MNum 0.8)])])]
      oneTransport :: Mini -> IO (Either String Mini)
      oneTransport _ = pure (Right oneReply)
  one <- jev1 (session oneTransport jevLatest)
          (rawState (MObj [("house", MStr "Greyhaven")])) (noul "Ready?")
  checkEq c "mini ask1 retains model and partial usage metadata"
    (Right ("prepared-model", Usage Nothing (Just 2)))
    ((\result -> (responseModel result, usage result)) <$> one)
  offline <- roundTrip (session (\_ -> pure (Left "offline" :: Either String Mini)) jevLatest)
              (rawState (MObj [("house", MStr "Greyhaven")])) (#ready := noul "Ready?")
  check c "mini retains typed host transport errors"
    (case offline of Left (Transport "offline") -> True; _ -> False)
  check c "mini distinguishes zero usage from unknown usage" $ case prepare jevLatest world (#ready := noul "Ready?") of
    Left _ -> False
    Right prepared -> case (decode prepared (MObj [("model", MStr "m"), ("usage", MObj [("input_tokens", MNum 0)]), ("answers", MObj [("ready", MObj [("type", MStr "noul"), ("noul", MNum 0.8)])])]),
                           decode prepared (MObj [("model", MStr "m"), ("answers", MObj [("ready", MObj [("type", MStr "noul"), ("noul", MNum 0.8)])])])) of
      (Right zeroResponse, Right unknownResponse) -> usage zeroResponse == Usage (Just 0) Nothing && usage unknownResponse == Usage Nothing Nothing
      _ -> False
  check c "mini custom policies reject invalid floors" $ case customPolicy (-0.1) 0 0 of
    Left _ -> True
    Right _ -> False
  check c "mini custom policies reject non-finite and over-one floors"
    (case (customPolicy 0 (0 / 0) 0, customPolicy 0 0 1.01) of
      (Left _, Left _) -> True
      _ -> False)
  case customPolicy 0 0 0 of
    Left _ -> check c "mini valid custom policy setup" False
    Right policy -> do
      let sameTag :: Policy Custom -> Policy Custom
          sameTag = coerce
      let controlPacket :: (Int -> Text) -> Packet
            ("route" ::= Choice ("ship" ::> (Int -> Text) :|: "hold" ::> (Int -> Text)))
            (Questions Mini)
          controlPacket f = #route := choice "Where should this go?"
            (alt #ship "Ship" f .| alt #hold "Hold" (const "hold"))
      case prepare jevLatest world (controlPacket (const "control")) of
        Left _ -> check c "mini same-tag coercion setup" False
        Right prepared -> case decode prepared preparedReply of
          Left _ -> check c "mini same-tag coercion decode" False
          Right response -> do
            let sameSettled :: Settled Custom (Int -> Text) -> Settled Custom (Int -> Text)
                sameSettled = coerce
                selected = takenUnder policy (answers response).route
            check c "mini same-tag Policy and Settled coercions preserve the output"
              (fmap (\verdict -> settledValue verdict 1) selected
                 == fmap (\verdict -> settledValue verdict 1) (sameSettled <$> takenUnder (sameTag policy) (answers response).route))
