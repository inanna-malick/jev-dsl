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
  '[ "next" ::= Choice ("stop" ::> () :|: Many Room)
   , "ready" ::= Noul
   , "risk" ::= Score ("low" :|: "high")
   , "each_room" ::= Each Noul ] (Questions Mini)
inspect rooms =
     #next := choice "Which room to search next?"
                (alt #stop "Every room has been searched" () .| many (\(Room r) -> r) (\(Room r) -> MStr r) rooms)
  :& #ready := noul "Is the search ready to stop?"
  :& #risk := score "How risky is continuing?" (level #low (MStr "safe") .| level #high (MStr "dangerous"))
  :& #each_room := each [(r, noul ("Has " <> r <> " been searched?")) | Room r <- rooms]
  :& Nil

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
  case request jevLatest (state (MObj [("house", MStr "Greyhaven")])) packet of
    Left e -> check c ("mini: request failed: " ++ show e) False
    Right req -> do
      check c "mini: the request is built in a value type the core never saw"
        (case lookupKey "questions" req >>= viewObject of
          Just kv -> length kv == 5
          Nothing -> False)
      check c "mini: a runtime key reaches the wire under its own label"
        ((lookupKey "questions" req >>= lookupKey "next" >>= lookupKey "criteria" >>= lookupKey "cellar") == Just (MStr "cellar"))
      check c "mini: the battery flattens to one question per item"
        ((lookupKey "questions" req >>= lookupKey "each_room.attic") /= Nothing)
  r <- roundTrip stub jevLatest (state (MObj [("house", MStr "Greyhaven")])) packet
  case r of
    Left e -> check c ("mini: round trip failed: " ++ show e) False
    Right resp -> do
      let a = answers resp
      checkEq c "mini: the chosen row is the payload the program offered"
        (Room "cellar") (handle a.next (#stop (\() -> Room "none") .| onMany (\_ room -> room)))
      checkEq c "mini: a noul decodes" 0.9 a.ready.yes
      checkEq c "mini: a score grades through its levels"
        ("high" :: Text) (grade 0.5 a.risk (level #low "low" .| level #high "high"))
      checkEq c "mini: the battery answers under its runtime keys, in packet order"
        ["cellar", "attic"] (map fst a.each_room)
      checkEq c "mini: usage and model come back" "mini-1.0" (responseModel resp)
  -- the class default equality, which aeson overrides with its own
  check c "mini: structural equality ignores object member order"
    (jEqual (MObj [("a", MNum 1), ("b", MArr [MNull])]) (MObj [("b", MArr [MNull]), ("a", MNum 1)]))
  check c "mini: structural equality still distinguishes values"
    (not (jEqual (MObj [("a", MNum 1)]) (MObj [("a", MNum 2)])))
