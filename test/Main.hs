{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Check
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Jev

main :: IO ()
main = do
  c <- newChecks
  let v = jObject [("b", jNumber 2), ("a", jArray [jString (T.pack "x"), jNull, jBool True])] :: Value
      w = jObject [("a", jArray [jString (T.pack "x"), jNull, jBool True]), ("b", jNumber 2)] :: Value
  check c "json: view/construct round-trip through aeson" (case jView v of
    VObject kv -> length kv == 2 && (fmap (either (const Nothing) Just . Aeson.eitherDecode . Aeson.encode) (Just v) == Just (Just v))
    _ -> False)
  check c "json: jEqual ignores object order" (jEqual v w)
  check c "json: jEqual distinguishes values" (not (jEqual v (jObject [("b", jNumber 3)])))
  finish c
