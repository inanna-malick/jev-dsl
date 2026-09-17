{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A guard at a city gate, as a catamorphism with Jev for its algebra.
--
-- The script is a tree written by hand: what the guard asks, which kinds
-- of answer it distinguishes, when it checks the wanted posters, and how it weighs
-- the whole account at the end. It is a fixed point of 'GuardF', and its
-- three branching constructors line up with Jev's three question kinds:
--
--   * 'Ask'   — a free-form reply is sorted into one branch: a choice
--   * 'Check' — the account is held against each wanted poster in a pool: one Noul per poster
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
  , posters :: [(Text, Text)]        -- key, the wanted poster as nailed up at the gate
  }

world :: World
world = World
  { city = "Greyhaven"
  , edict = T.unwords
      [ "The city is under curfew after the robbery at the counting house. The watch is to question"
      , "everyone at the gates, turn back anyone who cannot account for themselves, and hold anyone who matches a poster." ]
  , neighbours =
      [ ("north_road", "The north road, through the forest; merchants, pilgrims, and the occasional deserter")
      , ("harbour", "The harbour town at the river mouth; sailors, smugglers, and anyone off a ship")
      , ("farmlands", "The farms and villages to the south; carts of produce every morning") ]
  , places =
      [ ("market", "The market square")
      , ("temple", "The temple of the dawn and its infirmary")
      , ("tavern", "The Broken Wheel and the other taverns by the wall")
      , ("barracks", "The watch barracks; recruits, and messages for the captain") ]
  , banned =
      [ ("unbound_weapon", "A blade or bow not peace-bonded at the gate")
      , ("smuggled_goods", "Untaxed spirits, spices, or anything hidden from the customs officer") ]
  , posters =
      [ ("thief", "WANTED: the thief of the counting house. Slight, quick, seen leaving by the north road with a heavy satchel. Reward.")
      , ("deserter", "WANTED: a deserter from the city watch, tall, scar across the left hand. Do not approach alone.") ]
  }

-- ---------------------------------------------------------------------------
-- The script's shape, and the fold over it
-- ---------------------------------------------------------------------------

data Action = Admit | TurnAway | SendForCaptain deriving (Show, Eq)

data GuardF r
  = Ask Text [(Text, Text, r)]           -- the guard's line; branch label, what the label means, child
  | Check [(Text, r)] r                  -- child per poster matched, and the child when none does
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
  "evasive" -> askCargo (cargoRule False)
  _ -> askPurpose $ \purpose ->
    -- The posters concern the north road and the taverns; nobody else is held against them.
    askCargo (cargoRule (origin == "north_road" || purpose == "tavern"))
  where
    -- Banned cargo is a rule, not a judgment: it never reaches the weighing.
    cargoRule suspect cargo
      | cargo `elem` map fst w.banned = verdict TurnAway
      | suspect = checkPosters weigh
      | otherwise = weigh

    ask line branches k = Fix (Ask line [(key, meaning, k key) | (key, meaning) <- branches])
    evasive = ("evasive", "Does not say, changes the subject, or answers a different question")

    askOrigin = ask "Halt. Where do you hail from, traveller?" (w.neighbours ++ [evasive])
    askPurpose = ask "And what brings you to Greyhaven?" (w.places ++ [evasive])
    askCargo = ask "Anything to declare? Weapons, goods, anything the customs officer should see?"
      (w.banned ++ [("nothing", "Nothing to declare: personal effects, ordinary goods, a bonded weapon"), evasive])

    -- Any poster that matches sends for the captain; otherwise the script continues.
    checkPosters continue = Fix (Check [(k, verdict SendForCaptain) | (k, _) <- w.posters] continue)

    weigh = Fix (Weigh "Taken together, does this traveller's story hold up?"
      ("The answers fit each other and fit the road they came by", verdict Admit)
      ("Plausible but thin: something is left out, or the answers do not quite fit together", verdict TurnAway)
      ("The account contradicts itself or the guard's knowledge of the roads", verdict SendForCaptain))

    verdict = Fix . Verdict

-- ---------------------------------------------------------------------------
-- Fold one: print the script
-- ---------------------------------------------------------------------------

render :: GuardF Text -> Text
render (Ask line bs) = T.unlines (("ask  " <> quote line) : concat [branch (k <> "  (" <> m <> ")") r | (k, m, r) <- bs])
render (Check ms none) = T.unlines ("check the posters" : concat [branch ("matches " <> k) r | (k, r) <- ms] ++ branch "no poster matches" none)
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
    let posted = pool #posters [(k, String text, ()) | (k, text) <- w.posters]
    resp <- must =<< roundTrip call jevLatest (situation w t [])
      ( #posters := posted
      :& #fits := eachIn posted (\poster -> #this := askAbout poster "Does the traveller's account so far match this wanted poster?" :& Nil)
      :& Nil )
    let scored = sortOn (Down . fst) [(yes sub.this, k) | (k, sub) <- (answers resp).fits]
    aside ("posters " <> T.intercalate ", " [k <> " " <> pct p | (p, k) <- scored])
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
    TIO.putStrLn ("guard: " <> spoken action)
    TIO.putStrLn ("verdict: " <> T.pack (show action))
    mapM_ (\u -> TIO.putStrLn ("  " <> quote u.replied <> " -> " <> u.taken <> " " <> pct u.sureness)) turnsTaken
    (n, i, o) <- readIORef calls
    TIO.putStrLn ("  " <> T.pack (show n) <> " calls, " <> T.pack (show i) <> " in / " <> T.pack (show o) <> " out tokens")
  _ -> hPutStrLn stderr "usage: jev-dsl-guard [--script]" >> exitFailure

spoken :: Action -> Text
spoken Admit = "Go on through. Mind the curfew."
spoken TurnAway = "Not tonight. Move along, and don't let me see you at this gate again."
spoken SendForCaptain = "Guards! Hold this one. Someone fetch the captain."

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
