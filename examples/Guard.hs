{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A city guard at the gate, as a catamorphism with Jev for its algebra.
--
-- The script is a tree written by hand: what the guard asks, which kinds
-- of reply it tells apart, when it holds the story against the wanted
-- posters, how it weighs the story, and what it will talk about after. It
-- is a fixed point of 'GuardF'. Four of its constructors are Jev's
-- questions:
--
--   * 'Ask'    — a free-form reply is sorted into one branch: a choice; with an
--                optional tripwire Noul in the same packet for admissions and slips
--   * 'Check'  — the story is held against each wanted poster in a pool: one Noul per poster
--   * 'Weigh'  — the story so far is graded on a rubric: a score
--   * 'Happen' — something may happen at the gate: a choice among authored events
--
-- The rest is plumbing: 'Say', 'Knot', 'Verdict', 'End'. The tree is
-- rational: finite to write, infinite to unfold, because the hubs after
-- each verdict are tied back into themselves. 'cata' over it is productive
-- because the algebra builds programs and never forces a child until the
-- player takes that branch.
--
-- Two folds run over the same tree. 'render' prints the script, naming
-- each knot once. 'interpret' turns each node into a 'Play' that talks to
-- the traveller, makes one Jev call, and continues into the child Jev
-- chose; the continuation rides inside the Jev alternative as its payload.
-- Nothing is generated at run time. The author wrote every line, every
-- branch, and every event. Jev decides which branch a reply takes, which
-- poster matches, whether the story holds, and what happens next.
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
import Data.Maybe (fromMaybe)
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
-- The world: what the script's branches are derived from, and what changes
-- ---------------------------------------------------------------------------

data World = World
  { edict :: Text                    -- the premise on every Jev call
  , bellGone :: Bool                 -- the curfew bell has rung
  , neighbours :: [(Text, Text)]     -- key, what the guard knows of the road
  , places :: [(Text, Text)]         -- key, what people go there for
  , banned :: [(Text, Text)]         -- key, what is not allowed through
  , posters :: [(Text, Text)]        -- key, the wanted poster as nailed up at the gate
  , happenings :: [Happening]        -- what may happen while you stand here
  }

data Happening = Happening
  { tag :: Text
  , blurb :: Text                    -- what Jev is shown
  , seen :: Text                     -- what the player sees
  , said :: Text                     -- what the guard says about it
  , apply :: World -> World          -- the world afterwards
  }

world :: World
world = World
  { edict = T.unwords
      [ "The city is under curfew after the robbery at the counting house. The watch is to question"
      , "everyone at the gates, turn back anyone who cannot account for themselves, and hold anyone who matches a poster." ]
  , bellGone = False
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
  , happenings =
      [ Happening "bell" "The curfew bell rings out over the city"
          "The curfew bell starts up over the rooftops, slow and heavy."
          "There's the bell. Nobody's got long now."
          (\w -> w { bellGone = True, edict = w.edict <> " The curfew bell has gone; anyone still outside is to be moved along." })
      , Happening "runner" "A runner from the barracks arrives with news about the thief"
          "A boy in watch colours comes pelting down the wall road and mutters something to the guard."
          "Seen at the harbour tonight, they say. The thief. So much for the north road."
          (\w -> w { posters = [(k, if k == "thief" then p <> " Fresh word: seen at the harbour tonight." else p) | (k, p) <- w.posters] })
      , Happening "rain" "It starts to rain"
          "The first drops hit the flagstones. Then the rest of them."
          "Perfect. Of course it is."
          id
      , Happening "cart" "A cart pulls up behind the traveller and waits"
          "Behind you a cart creaks to a halt, and a carter sits looking at the back of your head."
          "You're holding up the line. Make it quick."
          id
      , Happening "drunk" "A drunk is thrown out of the Broken Wheel, within earshot of the gate"
          "Somewhere behind the wall a door bangs and somebody lands in the street, singing."
          "Every night. Every single night."
          id
      , Happening "captain" "The captain passes the gate on his rounds"
          "Boots on the wall walk. The captain, with two of the watch behind him, stops at the gate."
          "Captain. This one's for you."
          id
      ]
  }

-- ---------------------------------------------------------------------------
-- The script's shape, and the fold over it
-- ---------------------------------------------------------------------------

data Action = Admit | TurnAway | SendForCaptain deriving (Show, Eq)

data GuardF r
  = Ask Text (Maybe (Text, r)) [(Text, Text, r)]   -- the guard's line; a tripwire and where it leads; branch label, meaning, child
  | Say Text r                                     -- the guard speaks; no reply expected
  | Check [(Text, r)] r                            -- child per poster matched, and the child when none does
  | Weigh Text (Text, r) (Text, r) (Text, r)       -- a question, and the sound / thin / false levels
  | Happen r                                       -- something may happen here
  | Knot Text r                                    -- a named point the script comes back to
  | Verdict Action r                               -- the guard decides, and the story goes on
  | End

instance Functor GuardF where
  fmap f (Ask line trip bs) = Ask line (fmap (fmap f) trip) [(k, m, f r) | (k, m, r) <- bs]
  fmap f (Say line r) = Say line (f r)
  fmap f (Check ms none) = Check [(k, f r) | (k, r) <- ms] (f none)
  fmap f (Weigh q (a, x) (b, y) (c, z)) = Weigh q (a, f x) (b, f y) (c, f z)
  fmap f (Happen r) = Happen (f r)
  fmap f (Knot n r) = Knot n (f r)
  fmap f (Verdict v r) = Verdict v (f r)
  fmap _ End = End

newtype Fix f = Fix (f (Fix f))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (Fix node) = alg (fmap (cata alg) node)

-- ---------------------------------------------------------------------------
-- The script itself. Branches come from the world; the shape is authored.
-- ---------------------------------------------------------------------------

gate :: World -> Fix GuardF
gate w = askOrigin False
  where
    -- The approach: three questions, the posters if the road or the errand warrants, then the weighing.
    askOrigin pressed = askLine "Halt. Where do you hail from, traveller?" Nothing (w.neighbours ++ [evasive]) $ \origin ->
      if origin == "evasive" && not pressed
        then say "I'll ask once more, and I'd think about the answer this time." (askOrigin True)
        else afterOrigin origin
    afterOrigin "evasive" = askCargo False
    afterOrigin origin = askPurpose $ \purpose ->
      -- The posters concern the north road and the taverns; nobody else is held against them.
      askCargo (origin == "north_road" || purpose == "tavern")
    askPurpose = askLine "And what brings you to Greyhaven?" Nothing (w.places ++ [evasive])
    askCargo suspect = askLine "Anything to declare? Weapons, goods, anything the customs officer should see?" Nothing
      (w.banned ++ [("nothing", "Nothing to declare: personal effects, ordinary goods, a bonded weapon"), evasive]) $ \cargo ->
        -- Banned cargo is a rule, not a judgment: it never reaches the weighing.
        if cargo `elem` map fst w.banned then verdict TurnAway
        else if suspect then checkPosters weigh else weigh
    checkPosters continue = Fix (Check [(k, verdict SendForCaptain) | (k, _) <- w.posters] continue)

    weigh = weighInto Admit TurnAway SendForCaptain
    weighInto sound thin false = Fix (Weigh "Taken together, does this traveller's story hold up?"
      ("The answers fit each other and fit the road they came by", verdict sound)
      ("Plausible but thin: something is left out, or the answers do not quite fit together", verdict thin)
      ("The story contradicts itself, the posters, or the guard's knowledge of the roads", verdict false))

    -- Every verdict opens onto a hub, and every hub is tied back into itself.
    verdict act = Fix (Verdict act (hub act))
    hub Admit = admitted
    hub TurnAway = turnedAway
    hub SendForCaptain = held

    admitted = knot "gate" $ happen $
      askLine "Anything else before you go through?" (Just (slip, say "Wait. Say that again." weigh))
        ( [(k, "Asks about the " <> k <> " on the posters, or the reward") | (k, _) <- w.posters]
       ++ [(k, "Asks the way to the " <> k <> ", or what goes on there") | (k, _) <- w.places]
       ++ [ ("curfew", "Asks what the curfew means for them tonight")
          , ("captain", "Asks about the captain or the watch")
          , ("rumour", "Asks about the robbery, or for news and gossip")
          , ("chat", "Small talk, a remark about the night, or anything else")
          , ("leave", "Says goodbye, moves on, or has nothing more to ask") ] )
        $ \case
          "leave" -> say "Then go on. And mind the curfew." end
          topic -> say (fromMaybe "Mm. Long night. Move along when you're ready." (lookup topic smallTalk)) admitted

    turnedAway = knot "turned_away" $ happen $
      askLine "The gate's closed to you tonight. Unless you've something to add." (Just (slip, say "That's enough." (verdict SendForCaptain)))
        [ ("explain", "Adds to their story, gives a reason, or names someone who can vouch for them")
        , ("bribe", "Offers money, a favour, or anything of value to the guard")
        , ("insult", "Insults, mocks, or threatens the guard")
        , ("beg", "Pleads, appeals to pity, or asks for an exception")
        , ("chat", "Small talk, a question, or anything else")
        , ("leave", "Gives up, says goodbye, or turns to go") ]
        $ \case
          "explain" -> say "Go on, then. All of it, from the start." weigh
          "bribe" -> say "Did you just offer the watch coin? At its own gate?" (verdict SendForCaptain)
          "insult" -> say "Say that again and it's the captain you'll be explaining yourself to." turnedAway
          "beg" -> say "Save it. I've heard better from the drunks at the Broken Wheel." turnedAway
          "leave" -> say "Then go. The road's that way." end
          _ -> say "The gate's still closed." turnedAway

    held = knot "held" $ happen $
      askLine "Stand there. The captain's on his way. Anything to say for yourself?" (Just (slip, say "Noted. The captain will want to hear that." held))
        [ ("explain", "Tries to explain, gives an account, or names someone who can vouch for them")
        , ("protest", "Protests innocence, objects, or demands to be released")
        , ("threaten", "Threatens the guard or the watch")
        , ("run", "Tries to run, push past, or escape")
        , ("chat", "Anything else") ]
        $ \case
          -- A held traveller can talk their way down to the road, never straight through the gate.
          "explain" -> say "Go on. Slowly." (weighInto TurnAway TurnAway SendForCaptain)
          "protest" -> say "Tell it to the captain." held
          "threaten" -> say "Threatening the watch at its own gate. Bold." held
          "run" -> say "Runner! Nobody runs from this gate. Not far." end
          _ -> say "Stand there." held

    smallTalk =
      [ ("thief", "Slight, quick, heavy satchel. Seen anyone like that on the road? Tell the watch. Don't be a hero.")
      , ("deserter", "Tall, scar across the left hand. If you see him, walk the other way and find one of us.")
      , ("market", "The market? Straight on past the well. You'll smell it before you see it.")
      , ("temple", "Left at the well, follow the bells. The infirmary's round the back.")
      , ("tavern", "The Broken Wheel's the first door on your right, and you'll wish it wasn't.")
      , ("barracks", "By the east wall. Ask for the sergeant, not the captain, if you want an answer tonight.")
      , ("curfew", "Indoors by the second bell. The watch won't ask twice tonight, not after the counting house.")
      , ("captain", "The captain's not slept since the robbery. If you've nothing to tell him, don't take up his time.")
      , ("rumour", "They say the counting house was opened with a key, not a crowbar. Draw your own conclusions. I'm not paid to.") ]

    slip = "Does this reply admit to something the edict forbids, contradict what the traveller said earlier, or give the guard fresh reason for suspicion?"
    evasive = ("evasive", "Does not say, changes the subject, or answers a different question")

    askLine line trip branches k = Fix (Ask line trip [(l, meaning, k l) | (l, meaning) <- branches])
    say line next = Fix (Say line next)
    knot name body = Fix (Knot name body)
    happen next = Fix (Happen next)
    end = Fix End

-- ---------------------------------------------------------------------------
-- Fold one: print the script, each knot once
-- ---------------------------------------------------------------------------

render :: GuardF ([Text] -> Text) -> [Text] -> Text
render node tied = case node of
  Ask line trip bs -> T.unlines (("ask  " <> quote line) : maybe [] (\(q, r) -> branch ("if slips  (" <> q <> ")") (r tied)) trip
                                 ++ concat [branch (k <> "  (" <> m <> ")") (r tied) | (k, m, r) <- bs])
  Say line next -> "say  " <> quote line <> "\n" <> next tied
  Check ms none -> T.unlines ("check the posters" : concat [branch ("matches " <> k) (r tied) | (k, r) <- ms] ++ branch "no poster matches" (none tied))
  Weigh q (a, x) (b, y) (c, z) ->
    T.unlines (("weigh  " <> quote q) : concat [branch (l <> "  (" <> m <> ")") (r tied) | (l, m, r) <- [("sound", a, x), ("thin", b, y), ("false", c, z)]])
  Happen next -> "something may happen\n" <> next tied
  Knot name body | name `elem` tied -> "back to " <> name <> "\n"
                 | otherwise -> name <> ":\n" <> body (name : tied)
  Verdict v next -> "verdict " <> T.pack (show v) <> "\n" <> next tied
  End -> "end\n"

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
data Traveller = Traveller { turns :: [Turn], standing :: [Action], happened :: [Text], here :: World }
data Outcome = Outcome (Maybe Action) [Turn]

type Play = Traveller -> IO Outcome
type Transport = Value -> IO (Either Text Value)

interpret :: Transport -> GuardF Play -> Play
interpret call = \case
  Ask line trip branches -> \t -> do
    guard line
    reply <- hear
    let st = situation t [("question", String line), ("reply", String reply)]
        sorting = given t.here.edict (choice "Which branch does the traveller's reply take?"
                    (many [(k, String meaning, play) | (k, meaning, play) <- branches]))
        follow a = do
          let runnersUp = [k <> " " <> pct m | (m, s) <- contenders 0.2 a, let k = selectedKey s, k /= a.key]
          aside ("heard " <> a.key <> " " <> pct a.confidence
            <> if null runnersUp then "" else "  (also " <> T.intercalate ", " runnersUp <> ")")
          handle (chosen a) (onMany (\k play -> play (t `saw` Turn line reply k a.confidence)))
    case trip of
      Nothing -> ask1 call jevLatest st sorting >>= must >>= follow
      Just (wording, tripped) -> do
        resp <- must =<< ask call jevLatest st (#branch := sorting :& #slip := noul wording :& Nil)
        let a = answers resp
        if a.slip.yes >= 0.6
          then aside ("slip " <> pct (a.slip.yes) <> ", was heading for " <> a.branch.key)
                 >> tripped (t `saw` Turn line reply "slip" (a.slip.yes))
          else follow a.branch

  Say line next -> \t -> guard line >> next t

  Check matches none -> \t -> do
    let posted = pool #posters [(k, String text, ()) | (k, text) <- t.here.posters]
    resp <- must =<< ask call jevLatest (situation t [])
      ( #posters := posted
      :& #fits := eachIn posted (\poster -> #this := askAbout poster "Does the traveller's story so far match this wanted poster?" :& Nil)
      :& Nil )
    let scored = sortOn (Down . fst) [(sub.this.yes, k) | (k, sub) <- (answers resp).fits]
    aside ("posters " <> T.intercalate ", " [k <> " " <> pct p | (p, k) <- scored])
    case scored of
      (p, k) : _ | p >= 0.6, Just play <- lookup k matches -> play t
      _ -> none t

  Weigh q (sound, x) (thin, y) (false, z) -> \t -> do
    a <- must =<< ask1 call jevLatest (situation t [])
      (given t.here.edict (score q (level #sound (String sound) .| level #thin (String thin) .| level #false (String false))))
    aside ("weighed " <> a.nearest <> "  " <> T.intercalate ", " [k <> " " <> pct m | (k, m) <- a.masses])
    case a.nearest of
      "sound" -> x t
      "thin" -> y t
      _ -> z t

  Happen next -> \t -> do
    let unused = [h | h <- t.here.happenings, h.tag `notElem` t.happened]
    if null unused then next t else do
      a <- must =<< ask1 call jevLatest (situation t [])
        (given t.here.edict (choice "Which of these, if any, happens now? Most moments, nothing does."
          (alt #nothing "The night goes on; nothing in particular happens" () .| many [(h.tag, String h.blurb, h) | h <- unused])))
      handle (chosen a)
        (  #nothing (\() -> next t)
        .| onMany (\_ h -> do
             aside ("happening " <> h.tag <> " " <> pct a.confidence)
             narrate h.seen
             guard h.said
             let t' = t { happened = h.tag : t.happened, here = h.apply t.here }
             -- The captain's rounds end a held traveller's night; everyone else watches him pass.
             if h.tag == "captain" && take 1 t.standing == [SendForCaptain]
               then pure (Outcome (Just SendForCaptain) (reverse t'.turns))
               else next t') )

  Knot _ next -> next

  Verdict v next -> \t -> do
    guard (spoken t.here v)
    next t { standing = v : t.standing }

  End -> \t -> pure (Outcome (headMay t.standing) (reverse t.turns))
  where
    saw t turn = t { turns = turn : t.turns }
    headMay xs = case xs of { x : _ -> Just x; [] -> Nothing }

spoken :: World -> Action -> Text
spoken w Admit | w.bellGone = "Go on through, and quick about it. The bell's gone."
               | otherwise = "Go on through. Mind the curfew."
spoken _ TurnAway = "Not tonight. Move along, and don't let me see you at this gate again."
spoken _ SendForCaptain = "Guards! Hold this one. Someone fetch the captain."

-- What every call sees: the gate as the guard knows it tonight, the
-- conversation so far, the verdicts already spoken, and whatever the node adds.
situation :: Traveller -> [(Text, Value)] -> State
situation t extra = state $ object $
  [ "gate" .= object
      [ "city" .= ("Greyhaven" :: Text)
      , "roads_in" .= object [Key.fromText k .= d | (k, d) <- t.here.neighbours]
      , "places" .= object [Key.fromText k .= d | (k, d) <- t.here.places]
      , "not_allowed_through" .= object [Key.fromText k .= d | (k, d) <- t.here.banned]
      , "posters" .= object [Key.fromText k .= d | (k, d) <- t.here.posters] ]
  , "conversation_so_far" .= [object ["guard" .= u.asked, "traveller" .= u.replied, "taken_as" .= u.taken] | u <- reverse t.turns]
  , "verdicts_so_far" .= map show (reverse t.standing)
  , "happened_so_far" .= reverse t.happened
  ] ++ [Key.fromText k .= v | (k, v) <- extra]

-- ---------------------------------------------------------------------------
-- The gate, the transport, and the terminal
-- ---------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= \case
  ["--script"] -> TIO.putStr (cata render (gate world) [])
  [] -> do
    hSetBuffering stdout NoBuffering
    calls <- newIORef (0 :: Int, 0 :: Int, 0 :: Int)
    let play = cata (interpret (counted calls curl)) (gate world)
    Outcome final taken <- play (Traveller [] [] [] world)
    TIO.putStrLn ""
    TIO.putStrLn ("verdict: " <> maybe "none" (T.pack . show) final)
    mapM_ (\u -> TIO.putStrLn ("  " <> quote u.replied <> " -> " <> u.taken <> " " <> pct u.sureness)) taken
    (n, i, o) <- readIORef calls
    TIO.putStrLn ("  " <> T.pack (show n) <> " calls, " <> T.pack (show i) <> " in / " <> T.pack (show o) <> " out tokens")
  _ -> hPutStrLn stderr "usage: jev-dsl-guard [--script]" >> exitFailure

guard :: Text -> IO ()
guard line = TIO.putStrLn ("guard: " <> line)

narrate :: Text -> IO ()
narrate t = TIO.putStrLn ("       " <> t)

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
