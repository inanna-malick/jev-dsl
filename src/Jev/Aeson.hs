{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}

-- | 'JsonValue' for aeson's 'Value'. Numbers cross the boundary as 'Double'
-- only when the DSL itself produces or reads them; author-supplied content
-- keeps its exact 'Scientific'.
module Jev.Aeson () where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Scientific (fromFloatDigits, toRealFloat)
import qualified Data.Vector as V
import Jev.Core.Json

instance JsonValue Aeson.Value where
  jNull = Aeson.Null
  jBool = Aeson.Bool
  jNumber = Aeson.Number . fromFloatDigits
  jString = Aeson.String
  jArray = Aeson.Array . V.fromList
  jObject = Aeson.Object . KeyMap.fromList . map (\(k, v) -> (Key.fromText k, v))
  jView = \case
    Aeson.Null -> VNull
    Aeson.Bool b -> VBool b
    Aeson.Number n -> VNumber (toRealFloat n)
    Aeson.String s -> VString s
    Aeson.Array xs -> VArray (V.toList xs)
    Aeson.Object kv -> VObject [(Key.toText k, v) | (k, v) <- KeyMap.toList kv]
