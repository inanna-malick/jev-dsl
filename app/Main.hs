{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}

-- | One worked example, both ends of the wire and nothing in between.
--
-- @jev-dsl-example request ...flags@ prints the request JSON for a failing-
-- check triage. @jev-dsl-example decode ...same flags@ reads the response
-- JSON on stdin, decodes it against the same record, and prints what the
-- typed answers say. A transport goes between them; see scripts/example.sh.
module Main (main) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value (..), object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Jev
import qualified Data.Aeson.Key as Key
import Options.Applicative (Parser, ReadM, command, eitherReader, execParser, fullDesc, help, helper, info, long, metavar, progDesc, showDefault, some, strOption, subparser, (<**>))
import qualified Options.Applicative as Opt
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- ---------------------------------------------------------------------------
-- The record: a failing check, triaged in one packet
-- ---------------------------------------------------------------------------

-- Local payloads. None of these are serialized; the model sees descriptions.
data Diagnostic = Diagnostic { diagnosticKey :: Text, diagnosticText :: Text }
newtype Check = Check Text
newtype Handoff = Handoff Text

data Next mode = Next
  { rerunFocusedCheck :: mode :- Option Check
  , readImplicatedSource :: mode :- Option Text
  , askModel :: mode :- Option Handoff      -- the handback is an ordinary alternative
  } deriving (Generic)

data Breadth mode = Breadth
  { localized :: mode :- Level
  , adjacentCallers :: mode :- Level
  , crossesContract :: mode :- Level
  } deriving (Generic)

data Triage mode = Triage
  { explains :: mode :- Choose Diagnostic
  , next :: mode :- Choice Next
  , verify :: mode :- Choose Check
  , sufficient :: mode :- Noul
  , breadth :: mode :- Score Breadth
  } deriving (Generic)
instance Schema Triage

data Inputs = Inputs
  { failure :: Text
  , diagnosticLines :: [(Text, Text)]
  , checks :: [(Text, Text)]
  , model :: Text
  }

triage :: Inputs -> (State, Triage Questions)
triage inputs = (world, questions)
  where
    world = stateObject
      [ ("failure", String inputs.failure)
      , ("diagnostics", object [Key.fromText k .= t | (k, t) <- inputs.diagnosticLines])
      , ("available_checks", object [Key.fromText k .= t | (k, t) <- inputs.checks])
      ]
    questions = Triage
      { explains = choose "Which diagnostic identifies the behavior to investigate, rather than a warning or a downstream consequence?"
          (candidates [(k, String t, Diagnostic k t) | (k, t) <- inputs.diagnosticLines])
          [noMatch "No listed diagnostic explains the failure"]
      , next = choice "What is the most useful next step given only the supplied evidence?" Next
          { rerunFocusedCheck = option "Rerun the single most relevant check to confirm the failure is stable" (Check "rerun")
          , readImplicatedSource = option "Read the source at the location the explaining diagnostic names" "read"
          , askModel = option "Deciding needs judgment beyond the supplied diagnostics and checks" (Handoff "needs judgment")
          }
      , verify = choose "Which available check most directly verifies a fix for the explaining diagnostic?"
          (candidates [(k, String t, Check k) | (k, t) <- inputs.checks])
          [deferToModel "No listed check is a direct verification; choosing needs a design preference"]
      , sufficient = noul "Do `diagnostics` alone establish the mechanism of `failure`?"
      , breadth = score "How broadly would fixing the explaining diagnostic alter established behavior?" Breadth
          { localized = level "Localized to the failing check"
          , adjacentCallers = level "May affect adjacent callers of the same code"
          , crossesContract = level "Crosses a contract other components rely on"
          }
      }

-- ---------------------------------------------------------------------------
-- Command line
-- ---------------------------------------------------------------------------

data Mode = RequestMode | DecodeMode

pair :: ReadM (Text, Text)
pair = eitherReader $ \s -> case break (== '=') s of
  (k, '=' : v) | not (null k) -> Right (T.pack k, T.pack v)
  _ -> Left "expected KEY=TEXT"

inputsP :: Parser Inputs
inputsP = Inputs
  <$> strOption (long "failure" <> metavar "TEXT" <> help "What failed, in one sentence")
  <*> some (Opt.option pair (long "diagnostic" <> metavar "KEY=TEXT" <> help "A diagnostic line, repeatable"))
  <*> some (Opt.option pair (long "check" <> metavar "KEY=TEXT" <> help "An available check and what it asserts, repeatable"))
  <*> strOption (long "model" <> Opt.value "jev-latest" <> showDefault <> help "Model name or alias")

modeP :: Parser Mode
modeP = subparser
  ( command "request" (info (pure RequestMode) (progDesc "Print the request JSON on stdout"))
 <> command "decode" (info (pure DecodeMode) (progDesc "Read the response JSON on stdin and print the typed answers")) )

main :: IO ()
main = do
  (mode, inputs) <- execParser $ info (((,) <$> modeP <*> inputsP) <**> helper)
    (fullDesc <> progDesc "jev-dsl worked example: triage a failing check through one typed record")
  let (world, questions) = triage inputs
  prepared <- case prepare (Model inputs.model) world questions of
    Left e -> die ("prepare: " ++ show e)
    Right p -> pure p
  case mode of
    RequestMode -> BL.putStr (Aeson.encode (requestValue prepared)) >> putStrLn ""
    DecodeMode -> do
      body <- BL.getContents
      response <- either (die . ("response is not JSON: " ++)) pure (Aeson.eitherDecode body)
      resp <- either (die . ("decode: " ++) . show) pure (decodeResponse prepared response)
      report resp

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

-- ---------------------------------------------------------------------------
-- What the typed answers say
-- ---------------------------------------------------------------------------

report :: Response Triage -> IO ()
report resp = do
  let a = answers resp
  TIO.putStrLn ("model: " <> resolvedModel resp)
  TIO.putStrLn ("usage: " <> render (usage resp))
  case picked a.explains of
    PickedCandidate c -> TIO.putStrLn ("explains: " <> (candidatePayload c).diagnosticKey <> "  \"" <> (candidatePayload c).diagnosticText <> "\"")
    PickedExit e -> TIO.putStrLn ("explains: <" <> exitKey e <> ">")
  TIO.putStrLn ("  ranked: " <> T.intercalate ", " [k <> "=" <> showT p | (k, p) <- ranked a.explains])
  TIO.putStrLn ("next: " <> match a.next Next
    { rerunFocusedCheck = \(Check c) -> "rerun check " <> c
    , readImplicatedSource = \what -> what <> " the implicated source"
    , askModel = \(Handoff why) -> "hand back to the model (" <> why <> ")"
    } <> "  confidence " <> showT (confidence a.next))
  case picked a.verify of
    PickedCandidate c -> let Check k = candidatePayload c in TIO.putStrLn ("verify: " <> k)
    PickedExit e -> TIO.putStrLn ("verify: <" <> exitKey e <> ">")
  TIO.putStrLn ("sufficient: " <> showT (probabilityYes a.sufficient)
    <> (if yesAbove 0.7 a.sufficient then "  (yes)" else if noBelow 0.3 a.sufficient then "  (no)" else "  (unsure)"))
  let m = levelMasses a.breadth
  TIO.putStrLn ("breadth: " <> showT (expectation a.breadth)
    <> "  localized " <> showT m.localized <> ", adjacent " <> showT m.adjacentCallers <> ", contract " <> showT m.crossesContract)
  mapM_ (TIO.putStrLn . ("diagnostic: " <>)) (diagnostics resp)
  where
    showT :: Show x => x -> Text
    showT = T.pack . show
    render = TL.toStrict . TLE.decodeUtf8 . Aeson.encode
