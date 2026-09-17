{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A guard at a city gate, as a catamorphism with Jev for its algebra.
--
-- The script is a tree written by hand: what the guard asks, which kinds
-- of answer it distinguishes, when it checks the notices, and how it weighs
-- the whole account at the end. It is a fixed point of 'GuardF', and its
-- three branching constructors line up with Jev's three question kinds:
--
--   * 'Ask'   — a free-form reply is sorted into one branch: a choice
--   * 'Check' — the account is held against each notice in a pool: one Noul per notice
--   * 'Weigh' — the account so far is graded on a rubric: a score
--
-- Two folds run over the same tree. 'render' is a pure algebra that prints
-- the script. 'interpret' is an algebra whose carrier is a program: each
-- node becomes a 'Play' that talks to the traveller, makes one Jev call,
-- and continues into the child Jev chose. The continuation for each branch
-- rides inside the Jev alternative as its payload, so there is no routing
-- code. Nothing is generated at run time; the author wrote every line and
-- every branch. Jev only decides which branch a reply takes.
--
--   scripts/guard.sh --script      print the tree, no network
--   scripts/guard.sh               play it, one Jev call per node visited
module Main (main) where

import Control.Monad (unless)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Jev.Operators
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import System.Process

-- ---------------------------------------------------------------------------
-- The world: what the script's branches are derived from
-- ---------------------------------------------------------------------------

data World = World
  { city :: Text
  , edict :: Text                    -- the premise on every Jev call
  , neighbours :: [(Text, Text)]     -- key, what the guard knows of the place
  , places :: [(Text, Text)]         -- key, what people go there for
  , banned :: [(Text, Text)]         -- key, what is not allowed through
  , notices :: [(Text, Text)]        -- key, the notice as posted at the gate
  }

world :: World
world = World
  { city = "Harrow"
  , edict = T.unwords
      [ "The council has closed the glassworks quarter to anyone not on the guild's roll"
      , "until a missing furnace notebook is recovered. Ordinary trade through the gate continues." ]
  , neighbours =
      [ ("hill_farms", "The farms in the hills to the east; carts of produce most mornings")
      , ("redwater", "The town downriver with its own glassworks; the guilds are not on good terms")
      , ("coast", "The fishing villages on the coast; salt, fish, and sailors between ships") ]
  , places =
      [ ("market", "The market square inside the gate")
      , ("glassworks", "The glassworks quarter, closed to outsiders under the edict")
      , ("cathedral", "The cathedral and the hospice beside it") ]
  , banned =
      [ ("unsealed_glass", "Worked glass without the guild's seal")
      , ("foreign_lenses", "Lenses ground outside the city") ]
  , notices =
      [ ("apprentice", "Wanted: a glassworks apprentice, about seventeen, who left with a furnace notebook. May be travelling under another name and trade.")
      , ("debt_buyer", "Watch for: an agent from Redwater buying up the debts of lens-grinders. Well dressed, asks after names.") ]
  }

-- ---------------------------------------------------------------------------
-- The script's shape, and the fold over it
-- ---------------------------------------------------------------------------

data Action = Admit | TurnAway | SendForCaptain deriving (Show, Eq)

data GuardF r
  = Ask Text [(Text, Text, r)]           -- the guard's line; branch label, what the label means, child
  | Check [(Text, r)] r                  -- child per notice matched, and the child when none does
  | Weigh Text (Text, r) (Text, r) (Text, r)  -- a question, and the sound / thin / false levels
  | Verdict Action

instance Functor GuardF where
  fmap f (Ask line bs) = Ask line [(k, m, f r) | (k, m, r) <- bs]
  fmap f (Check ms none) = Check [(k, f r) | (k, r) <- ms] (f none)
  fmap f (Weigh q (a, x) (b, y) (c, z)) = Weigh q (a, f x) (b, f y) (c, f z)
  fmap _ (Verdict v) = Verdict v

newtype Fix f = Fix (f (Fix f))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (Fix node) = alg (fmap (cata alg) node)

-- ---------------------------------------------------------------------------
-- The script itself. Branches come from the world; the shape is authored.
-- ---------------------------------------------------------------------------

gate :: World -> Fix GuardF
gate w = askOrigin $ \origin -> case origin of
  "evasive" -> askCargo cargoRule
  _ -> askPurpose $ \purpose ->
    -- The notices concern Redwater and the glassworks; anyone else is not held against them.
    if origin == "redwater" || purpose == "glassworks" then checkNotices (askCargo cargoRule) else askCargo cargoRule
  where
    -- Banned cargo is a rule, not a judgment: it never reaches the weighing.
    cargoRule cargo = if cargo `elem` map fst w.banned then verdict TurnAway else weigh

    ask line branches k = Fix (Ask line [(key, meaning, k key) | (key, meaning) <- branches])
    evasive = ("evasive", "Does not say, changes the subject, or answers a different question")

    askOrigin = ask "Evening. Where have you come from today?" (w.neighbours ++ [evasive])
    askPurpose = ask "And your business in the city?" (w.places ++ [evasive])
    askCargo = ask "What are you carrying?"
      (w.banned ++ [("nothing", "Nothing of note: personal effects, ordinary goods, an empty cart"), evasive])

    -- Any notice that fits sends for the captain; the rest of the script continues otherwise.
    checkNotices continue = Fix (Check [(k, verdict SendForCaptain) | (k, _) <- w.notices] continue)

    weigh = Fix (Weigh "Taken together, how sound is this traveller's account?"
      ("The answers fit each other and fit the road they came by", verdict Admit)
      ("Plausible but thin: something is left out, or the answers do not quite fit together", verdict TurnAway)
      ("The account contradicts itself or the guard's knowledge of the roads", verdict SendForCaptain))

    verdict = Fix . Verdict

-- ---------------------------------------------------------------------------
-- Fold one: print the script
-- ---------------------------------------------------------------------------

render :: GuardF Text -> Text
render (Ask line bs) = T.unlines (("ask  " <> quote line) : concat [branch (k <> "  (" <> m <> ")") r | (k, m, r) <- bs])
render (Check ms none) = T.unlines ("check the notices" : concat [branch ("fits " <> k) r | (k, r) <- ms] ++ branch "no notice fits" none)
render (Weigh q (a, x) (b, y) (c, z)) =
  T.unlines (("weigh  " <> quote q) : concat [branch (l <> "  (" <> m <> ")") r | (l, m, r) <- [("sound", a, x), ("thin", b, y), ("false", c, z)]])
render (Verdict v) = T.pack (show v) <> "\n"

-- A leaf is shown on the branch's own line; a subtree is indented under it.
branch :: Text -> Text -> [Text]
branch label child = case T.lines child of
  [leaf] -> ["  " <> label <> "  -> " <> leaf]
  ls -> ("  " <> label) : map ("    " <>) ls

quote :: Text -> Text
quote t = "\"" <> t <> "\""

-- ---------------------------------------------------------------------------
-- Fold two: play the script against a traveller
-- ---------------------------------------------------------------------------

data Turn = Turn { asked :: Text, replied :: Text, taken :: Text, sureness :: Double }
newtype Traveller = Traveller { turns :: [Turn] }
data Outcome = Outcome Action [Turn]

type Play = Traveller -> IO Outcome
type Transport = Value -> IO (Either Text Value)

interpret :: World -> Transport -> GuardF Play -> Play
interpret w call = \case
  Ask line branches -> \t -> do
    guard line
    reply <- hear
    a <- must =<< jev1 call jevLatest (situation w t [("question", String line), ("reply", String reply)])
      (given w.edict (choice "Which branch does the traveller's reply take?"
        (many [(k, String meaning, play) | (k, meaning, play) <- branches])))
    let runnersUp = [k <> " " <> pct m | (m, s) <- contenders 0.2 a, let k = selectedKey s, k /= selectedKey (chosen a)]
    aside ("heard " <> selectedKey (chosen a) <> " " <> pct (confidence a)
      <> if null runnersUp then "" else "  (also " <> T.intercalate ", " runnersUp <> ")")
    handle (chosen a) (onMany (\k play -> play (t `saw` Turn line reply k (confidence a))))
    -- k is the branch label Jev chose; play is that branch's continuation.

  Check matches none -> \t -> do
    let posted = pool #notices [(k, String text, ()) | (k, text) <- w.notices]
    resp <- must =<< roundTrip call jevLatest (situation w t [])
      ( #notices := posted
      :& #fits := eachIn posted (\notice -> #this := askAbout notice "Does the traveller's account so far fit this notice?" :& Nil)
      :& Nil )
    let scored = sortOn (Down . fst) [(yes sub.this, k) | (k, sub) <- (answers resp).fits]
    aside ("notices " <> T.intercalate ", " [k <> " " <> pct p | (p, k) <- scored])
    case scored of
      (p, k) : _ | p >= 0.6, Just play <- lookup k matches -> play t
      _ -> none t

  Weigh q (sound, x) (thin, y) (false, z) -> \t -> do
    a <- must =<< jev1 call jevLatest (situation w t [])
      (given w.edict (score q (level #sound (String sound) .| level #thin (String thin) .| level #false (String false))))
    aside ("weighed " <> levelOf a <> "  " <> T.intercalate ", " [k <> " " <> pct m | (k, m) <- masses a])
    case levelOf a of
      "sound" -> x t
      "thin" -> y t
      _ -> z t

  Verdict v -> \t -> pure (Outcome v (reverse t.turns))
  where
    saw (Traveller ts) turn = Traveller (turn : ts)

-- What every call sees: the world as the guard knows it, the conversation
-- so far, and whatever the node adds.
situation :: World -> Traveller -> [(Text, Value)] -> State
situation w t extra = state $ object $
  [ "gate" .= object
      [ "city" .= w.city
      , "roads_in" .= object [Key.fromText k .= d | (k, d) <- w.neighbours]
      , "places" .= object [Key.fromText k .= d | (k, d) <- w.places]
      , "not_allowed_through" .= object [Key.fromText k .= d | (k, d) <- w.banned] ]
  , "conversation_so_far" .= [object ["guard" .= u.asked, "traveller" .= u.replied, "taken_as" .= u.taken] | u <- reverse t.turns]
  ] ++ [Key.fromText k .= v | (k, v) <- extra]

-- ---------------------------------------------------------------------------
-- The gate, the transport, and the terminal
-- ---------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= \case
  ["--script"] -> TIO.putStr (cata render (gate world))
  [] -> do
    hSetBuffering stdout NoBuffering
    calls <- newIORef (0 :: Int, 0 :: Int, 0 :: Int)
    let play = cata (interpret world (counted calls curl)) (gate world)
    Outcome action turnsTaken <- play (Traveller [])
    TIO.putStrLn ""
    TIO.putStrLn ("verdict: " <> T.pack (show action))
    mapM_ (\u -> TIO.putStrLn ("  " <> quote u.replied <> " -> " <> u.taken <> " " <> pct u.sureness)) turnsTaken
    (n, i, o) <- readIORef calls
    TIO.putStrLn ("  " <> T.pack (show n) <> " calls, " <> T.pack (show i) <> " in / " <> T.pack (show o) <> " out tokens")
  _ -> hPutStrLn stderr "usage: jev-dsl-guard [--script]" >> exitFailure

guard :: Text -> IO ()
guard line = TIO.putStrLn ("guard: " <> line)

aside :: Text -> IO ()
aside t = TIO.putStrLn ("       [" <> t <> "]")

-- Piped replies are echoed so a transcript reads the same either way.
hear :: IO Text
hear = do
  TIO.putStr "you:   "
  eof <- isEOF
  if eof then TIO.putStrLn "" >> TIO.putStrLn "(the traveller walks off)" >> exitFailure else do
    reply <- T.strip <$> TIO.getLine
    tty <- hIsTerminalDevice stdin
    unless tty (TIO.putStrLn reply)
    pure reply

must :: Either JevError a -> IO a
must = either (\e -> hPutStrLn stderr ("jev: " ++ show e) >> exitFailure) pure

pct :: Double -> Text
pct x = T.pack (show (round (x * 100) :: Int)) <> "%"

-- Every call is counted with its token usage, whatever the packet's shape.
counted :: IORef (Int, Int, Int) -> Transport -> Transport
counted ref call body = do
  r <- call body
  case r of
    Right v -> modifyIORef' ref (\(n, i, o) -> (n + 1, i + field "input_tokens" v, o + field "output_tokens" v))
    Left _ -> pure ()
  pure r
  where
    field k (Object m) | Just (Object u) <- KeyMap.lookup "usage" m, Just (Number x) <- KeyMap.lookup (Key.fromText k) u = round x
    field _ _ = 0

-- The transport: scripts/transport.sh holds the key and calls curl.
curl :: Transport
curl body = do
  (Just hin, Just hout, _, ph) <- createProcess (proc "scripts/transport.sh" []) { std_in = CreatePipe, std_out = CreatePipe }
  BL.hPut hin (Aeson.encode body) >> hClose hin
  out <- BL.hGetContents hout
  _ <- waitForProcess ph
  pure (either (Left . T.pack) Right (Aeson.eitherDecode out))
