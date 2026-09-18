{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | One worked example, both ends of the wire and nothing in between.
--
-- @jev-dsl-example request ...flags@ prints the request JSON for a failing-
-- check triage. @jev-dsl-example decode ...same flags@ reads the response
-- JSON on stdin, decodes it against the same packet, and prints what the
-- typed answers say under a policy. A transport goes between them; see
-- scripts/example.sh.
--
-- The packet exercises one of everything: a runtime group with an awkward
-- key, a static disjunction with a handback, a per-check battery, a Noul,
-- and a rubric.
module Main (main) where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Jev.Operators
import Options.Applicative (Parser, ReadM, command, eitherReader, execParser, fullDesc, help, helper, info, long, metavar, progDesc, showDefault, some, strOption, subparser, (<**>))
import qualified Options.Applicative as Opt
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- ---------------------------------------------------------------------------
-- The packet: a failing check, triaged in one call
-- ---------------------------------------------------------------------------

-- The rows the state carries and the questions offer. A row renders as the
-- provider sees it and stays a Haskell value for the program.
data Diagnostic = Diagnostic { diagnosticKey :: Text, diagnosticText :: Text }
data Check = Check { checkKey :: Text, checkText :: Text }
newtype Handoff = Handoff Text

-- A row renders as the provider reads it: the whole list is one object
-- keyed the way the questions key their candidates.
instance Field Value [Diagnostic] where
  toField ds = object [Key.fromText d.diagnosticKey .= d.diagnosticText | d <- ds]
instance Field Value [Check] where
  toField cs = object [Key.fromText c.checkKey .= c.checkText | c <- cs]

type Next = "rerun" ::> () :|: "read_source" ::> Text :|: "ask_model" ::> Handoff

type Triage = Packet
  (    "explains" ::= Choice ("no_match" ::> () :|: "diagnostics" ::* Diagnostic)
   :&  "next" ::= Choice Next
   :&  "verify" ::= Choice ("checks" ::* Check :|: "defer" ::> ())
   :&  "relevant" ::= Each Check Noul
   :&  "sufficient" ::= Noul
   :&  "breadth" ::= Score Text ("localized" :|: "adjacent" :|: "contract") )

-- The state's fields keep their Haskell types, so the rows a question is
-- built from are the rows the provider was shown.
type World = State ("failure" ::= Text :& "diagnostics" ::= [Diagnostic] :& "checks" ::= [Check])

data Inputs = Inputs
  { failure :: Text
  , diagnosticLines :: [(Text, Text)]
  , checks :: [(Text, Text)]
  , model :: Text
  }

triage :: Inputs -> (World, Triage Questions)
triage inputs = (world, questions)
  where
    world = state
      (  #failure := inputs.failure
      :& #diagnostics := [Diagnostic k t | (k, t) <- inputs.diagnosticLines]
      :& #checks := [Check k t | (k, t) <- inputs.checks] )
    questions =
         #explains := choice "Which diagnostic identifies the behavior to investigate, rather than a warning or a downstream consequence?"
                        (alt #no_match "No listed diagnostic explains the failure" () .| many #diagnostics (.diagnosticKey) (.diagnosticText) world.diagnostics)
      :& #next := choice "What is the most useful next step given only the supplied evidence?"
                    (  alt #rerun "Rerun the single most relevant check to confirm the failure is stable" ()
                    .| alt #read_source "Read the source at the location the explaining diagnostic names" "read"
                    .| alt #ask_model "Deciding needs judgment beyond the supplied diagnostics and checks" (Handoff "needs judgment") )
      :& #verify := choice "Which available check most directly verifies a fix for the explaining diagnostic?"
                      (many #checks (.checkKey) (.checkText) world.checks .| alt #defer "No listed check is a direct verification; choosing needs a design preference" ())
      :& #relevant := each (.checkKey) (\c -> noul ("Does the check `" <> c.checkKey <> "` (" <> c.checkText <> ") exercise the code path " <> field #failure world <> " names?")) world.checks
      :& #sufficient := noul ("Do " <> field #diagnostics world <> " alone establish the mechanism of " <> field #failure world <> "?")
      -- Each level carries the line the report prints for it, so what the
      -- provider is shown and what the program does sit on the same line.
      :& #breadth := score "How broadly would fixing the explaining diagnostic alter established behavior?"
                       (  level #localized "Localized to the failing check" "localized to the failing check"
                       .| level #adjacent "May affect adjacent callers of the same code" "may affect adjacent callers"
                       .| level #contract "Crosses a contract other components rely on" "crosses a contract others rely on" )

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
  case mode of
    RequestMode -> do
      req <- either (die . ("request: " ++) . show) pure (request (fromString (T.unpack inputs.model)) world questions)
      BL.putStr (Aeson.encode req) >> putStrLn ""
    DecodeMode -> do
      body <- BL.getContents
      response <- either (die . ("response is not JSON: " ++)) pure (Aeson.eitherDecode body)
      resp <- either (die . ("decode: " ++) . show) pure (decode questions response)
      report resp

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

-- ---------------------------------------------------------------------------
-- What the typed answers say, under a policy
-- ---------------------------------------------------------------------------

report :: Response Triage -> IO ()
report r = do
  let u = usage r
  TIO.putStrLn ("usage: " <> showT u.inputTokens <> " in, " <> showT u.outputTokens <> " out; model " <> resolvedModel r)
  mapM_ (TIO.putStrLn . ("note: " <>)) (diagnostics r)
  -- Each choice is settled under a policy: a result only through a handler
  -- per alternative, or a doubt with the numbers behind it.
  line "explains" (explain lenient r.explains) $ settle lenient r.explains
    (  #no_match (\() -> "<no listed diagnostic>")
    .| #diagnostics (\_ d -> d.diagnosticKey <> "  \"" <> d.diagnosticText <> "\"") )
  line "next" (explain careful r.next) $ settle careful r.next
    (  #rerun (\() -> "rerun the most relevant check")
    .| #read_source (\what -> what <> " the implicated source")
    .| #ask_model (\(Handoff why) -> "hand back to the model (" <> why <> ")") )
  line "verify" (explain careful r.verify) $ settle careful r.verify
    (#checks (\_ c -> "run " <> c.checkKey) .| #defer (\() -> "<defer to the model>"))
  -- Nouls are judged under the same policies.
  TIO.putStrLn ("relevant: " <> T.intercalate ", " [c.checkKey <> "=" <> verdict (judge lenient n) | (c, n) <- r.relevant])
  line "sufficient" (explain strict r.sufficient) $ fmap (fmap (\b -> if b then "yes" else "no")) (judge strict r.sufficient)
  -- A rubric is graded, not read off: the level half the weight reaches,
  -- and the line it carries is the one written beside its wording.
  TIO.putStrLn ("breadth: " <> grade 0.5 r.breadth
    <> "\n  expectation " <> showT r.breadth.expectation
    <> ", mass at or above adjacent " <> showT (massAtOrAbove #adjacent r.breadth))
  where
    line name why outcome = TIO.putStrLn (name <> ": " <> either (.why) (\(Settled t) -> t) outcome <> "\n  " <> why)
    verdict = either (const "?") (\(Settled b) -> if b then "yes" else "no")
    showT :: Show x => x -> Text
    showT = T.pack . show
