{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | One worked example, both ends of the wire and nothing in between.
--
-- @jev-dsl-example request ...flags@ prints the request JSON for a failing-
-- check triage. @jev-dsl-example decode ...same flags@ reads the response
-- JSON on stdin, decodes it against the same packet, and prints what the
-- typed answers say. A transport goes between them; see scripts/example.sh.
--
-- The packet exercises one of everything: a pool of checks declared once
-- and referenced by two questions, a runtime group with an awkward key, a
-- static disjunction with descriptions in the type and a handback, a typed
-- rubric, a per-entry Noul, and a premise-prefixed speculative question.
module Main (main) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Jev.Operators
import Options.Applicative (Parser, ReadM, command, eitherReader, execParser, fullDesc, help, helper, info, long, metavar, progDesc, showDefault, some, strOption, subparser, (<**>))
import qualified Options.Applicative as Opt
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- ---------------------------------------------------------------------------
-- The packet: a failing check, triaged in one call
-- ---------------------------------------------------------------------------

-- Local payloads. None of these are serialized; the model sees descriptions.
data Diagnostic = Diagnostic { diagnosticKey :: Text, diagnosticText :: Text }
newtype Check = Check Text
newtype Handoff = Handoff Text

type Next = "rerun" ::> Check :? "Rerun the single most relevant check to confirm the failure is stable"
        :|: "read_source" ::> Text :? "Read the source at the location the explaining diagnostic names"
        :|: "ask_model" ::> Handoff :? "Deciding needs judgment beyond the supplied diagnostics and checks"

type Breadth = '[ "localized" :? "Localized to the failing check"
                , "adjacent" :? "May affect adjacent callers of the same code"
                , "contract" :? "Crosses a contract other components rely on" ]

type Triage = Packet
  '[ "checks" ::= PoolDecl "checks" Check
   , "explains" ::= Choice ("no_match" ::> () :? "No listed diagnostic explains the failure" :|: Many Diagnostic)
   , "next" ::= Choice Next
   , "verify" ::= Choice (Many Check :|: "defer" ::> () :? "No listed check is a direct verification; choosing needs a design preference")
   , "relevant" ::= Each (Packet '[ "applies" ::= Noul ])
   , "sufficient" ::= Noul
   , "breadth" ::= Score Breadth
   , "if_flaky" ::= Choice (Many Check)
   ]

data Inputs = Inputs
  { failure :: Text
  , diagnosticLines :: [(Text, Text)]
  , checks :: [(Text, Text)]
  , model :: Text
  }

triage :: Inputs -> (State 'Pooled, Triage Questions)
triage inputs = (pooled world, questions)
  where
    world = stateObject
      [ ("failure", String inputs.failure)
      , ("diagnostics", object [Key.fromText k .= t | (k, t) <- inputs.diagnosticLines])
      ]
    available = pool #checks [(k, String t, Check k) | (k, t) <- inputs.checks]
    questions =
         #checks := available
      :& #explains := choice "Which diagnostic identifies the behavior to investigate, rather than a warning or a downstream consequence?"
                        (#no_match () .| many [(k, String t, Diagnostic k t) | (k, t) <- inputs.diagnosticLines])
      :& #next := choice "What is the most useful next step given only the supplied evidence?"
                    (#rerun (Check "rerun") .| #read_source "read" .| #ask_model (Handoff "needs judgment"))
      :& #verify := choice "Which available check most directly verifies a fix for the explaining diagnostic?"
                      (manyFrom available .| #defer ())
      :& #relevant := eachIn available (\r -> #applies := askAbout r "Does this check exercise the code path the failure names?" :& Nil)
      :& #sufficient := noul "Do `diagnostics` alone establish the mechanism of `failure`?"
      :& #breadth := score "How broadly would fixing the explaining diagnostic alter established behavior?"
      :& #if_flaky := given "the failure is intermittent rather than deterministic"
                        (choice "Which check would best expose the intermittency?" (manyFrom available))
      :& Nil

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
    (fullDesc <> progDesc "jev-dsl worked example: triage a failing check through one typed packet")
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
  TIO.putStrLn ("explains: " <> caseOf a.explains
    (  #no_match (\() -> "<no listed diagnostic>")
    .| onMany (\e -> (elementPayload e).diagnosticKey <> "  \"" <> (elementPayload e).diagnosticText <> "\"") ))
  TIO.putStrLn ("  ranked: " <> T.intercalate ", " [selectedKey s <> "=" <> showT p | (p, s) <- ranked a.explains])
  TIO.putStrLn ("next: " <> caseOf a.next
    (  #rerun (\(Check c) -> "rerun check " <> c)
    .| #read_source (\what -> what <> " the implicated source")
    .| #ask_model (\(Handoff why) -> "hand back to the model (" <> why <> ")") )
    <> "  confidence " <> showT (confidence a.next))
  TIO.putStrLn ("verify: " <> caseOf a.verify (onMany (\e -> let Check k = elementPayload e in k) .| #defer (\() -> "<defer to the model>")))
  TIO.putStrLn ("relevant: " <> T.intercalate ", " [k <> "=" <> showT (probabilityYes sub.applies) | (k, sub) <- a.relevant])
  TIO.putStrLn ("sufficient: " <> showT (probabilityYes a.sufficient)
    <> (if yesAbove 0.7 a.sufficient then "  (yes)" else if noBelow 0.3 a.sufficient then "  (no)" else "  (unsure)"))
  TIO.putStrLn ("breadth: " <> showT (expectation a.breadth) <> "  nearest " <> levelOf a.breadth
    <> ", mass at or above adjacent " <> showT (massAtOrAbove #adjacent a.breadth))
  TIO.putStrLn ("if flaky: " <> caseOf a.if_flaky (onMany (\e -> let Check k = elementPayload e in k)))
  TIO.putStrLn ("pool: " <> T.intercalate ", " [k | (k, _, _) <- poolEntries a.checks])
  mapM_ (TIO.putStrLn . ("diagnostic: " <>)) (diagnostics resp)
  where
    showT :: Show x => x -> Text
    showT = T.pack . show
    render = TL.toStrict . TLE.decodeUtf8 . Aeson.encode
