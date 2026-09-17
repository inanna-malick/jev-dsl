{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Check
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Jev.Aeson ()
import Jev.Core.Json (View (..), jArray, jBool, jEqual, jNull, jNumber, jObject, jString, jView)
import Corpus (corpusChecks)
import Proto (protoChecks)
import Golden (goldenChecks, genericChecks)
import Rejections (rejectionChecks)

main :: IO ()
main = do
  c <- newChecks
  let v = jObject [("b", jNumber 2), ("a", jArray [jString "x", jNull, jBool True])] :: Value
      w = jObject [("a", jArray [jString "x", jNull, jBool True]), ("b", jNumber 2)] :: Value
  check c "json: view/construct round-trip through aeson" (case jView v of
    VObject kv -> length kv == 2 && Aeson.eitherDecode (Aeson.encode v) == Right v
    _ -> False)
  check c "json: jEqual ignores object order" (jEqual v w)
  check c "json: jEqual distinguishes values" (not (jEqual v (jObject [("b", jNumber 3)])))
  protoChecks c
  corpusChecks c
  goldenChecks c
  genericChecks c
  rejectionChecks c
  finish c
