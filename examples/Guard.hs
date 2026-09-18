{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | A city guard at the gate, as a catamorphism with Jev for its algebra.
--
-- The script is a tree written by hand: what the guard asks, which kinds
-- of reply it tells apart, when it holds the story against the wanted
-- posters, how it weighs the story, and what it will talk about after. It
-- is a fixed point of 'GuardF'. Four of its constructors are Jev's
-- questions:
--
--   * 'Ask'    — a free-form reply is sorted into one branch: a choice over the
--                branches, and in the same packet a Noul per topic the reply may
--                also raise and a choice for whatever would stop the traveller
--                where they stand. The stop is optional; absent questions
--                send nothing, so every 'Ask' is one packet and one call
--   * 'Check'  — the story is held against each wanted poster: one Noul per poster, in one call
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

import Control.Monad (unless, when)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (mapAccumL, nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.TypeLits (KnownSymbol)
import Jev.Operators
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import System.Process

-- ---------------------------------------------------------------------------
-- The world: what the script's branches are derived from, and what changes
-- ---------------------------------------------------------------------------

data World = World
  { edict :: Text                    -- the standing orders, in the state of every Jev call
  , bellGone :: Bool                 -- the curfew bell has rung
  , neighbours :: [(Text, Text)]     -- key, what the guard knows of the road
  , places :: [(Text, Text)]         -- key, what people go there for
  , banned :: [(Text, Text)]         -- key, what is not allowed through
  , posters :: [(Text, Text)]        -- key, the wanted poster as nailed up at the gate
  , happenings :: [Happening]        -- what may happen while you stand here
  , again :: [(Text, [Text])]        -- how a line is put the second and later times it is asked
  }

data Happening = Happening
  { tag :: Text
  , blurb :: Text                    -- what Jev is shown
  , seen :: Text                     -- what the player sees
  , said :: Maybe Action -> Text     -- what the guard says about it, given where the traveller stands
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
      , ("tavern", "The Broken Wheel and the other taverns by the wall; a bed, a drink, a meal")
      , ("barracks", "The watch barracks; recruits, and messages for the captain") ]
  , banned =
      [ ("unbound_weapon", "A blade or bow not peace-bonded at the gate")
      , ("smuggled_goods", "Untaxed spirits, spices, or anything hidden from the customs officer") ]
  , posters =
      [ ("thief", "WANTED: the thief of the counting house. Slight, quick, seen leaving by the north road with a heavy satchel. Reward.")
      , ("deserter", "WANTED: a deserter from the city watch, tall, scar across the left hand. Do not approach alone.") ]
  , again =
      [ ("Anything else before you go through?", ["Anything else?", "Still here? Go on, then, what is it?"])
      , ("The gate's closed to you tonight. Unless you've something to add.", ["Anything to add?", "I'm still here, and the gate's still closed."])
      , ("Stand there. The captain's on his way. Anything to say for yourself?", ["Anything else to say?", "Still talking. Go on."]) ]
  , happenings =
      [ Happening "bell" "The curfew bell rings out over the city"
          "The curfew bell starts up over the rooftops, slow and heavy."
          (const "There's the bell. Nobody's got long now.")
          (\w -> w { bellGone = True, edict = w.edict <> " The curfew bell has gone; anyone still outside is to be moved along." })
      , Happening "runner" "A runner from the barracks arrives with news about the thief"
          "A boy in watch colours comes pelting down the wall road and mutters something to the guard."
          (const "Seen at the harbour tonight, they say. The thief. So much for the north road.")
          (\w -> w { posters = [(k, if k == "thief" then p <> " Fresh word: seen at the harbour tonight." else p) | (k, p) <- w.posters] })
      , Happening "rain" "It starts to rain"
          "The first drops hit the flagstones. Then the rest of them."
          (const "Perfect. Of course it is.")
          id
      , Happening "cart" "A cart pulls up behind the traveller and waits"
          "Behind you a cart creaks to a halt, and a carter sits looking at the back of your head."
          (const "You're holding up the line. Make it quick.")
          id
      , Happening "drunk" "A drunk is thrown out of the Broken Wheel, within earshot of the gate"
          "Somewhere behind the wall a door bangs and somebody lands in the street, singing."
          (const "Every night. Every single night.")
          id
      , Happening "captain" "The captain passes the gate on his rounds"
          "Boots on the wall walk. The captain, with two of the watch behind him, stops at the gate."
          (\case Just SendForCaptain -> "Captain. This one's for you."; _ -> "Evening, Captain. Nothing to report.")
          id
      ]
  }

-- ---------------------------------------------------------------------------
-- The script's shape, and the fold over it
-- ---------------------------------------------------------------------------

data Action = Admit | TurnAway | SendForCaptain deriving (Show, Eq)

-- 'Ask' holds the branches as Jev alternatives, each carrying the child it
-- leads to. There is no table from a key to a node and no case on a string:
-- the alternative the provider picks hands the program the node the author
-- wrote beside it.
data GuardF r
  = Ask Text (Maybe (Trip r)) (Uniform r)          -- the guard's line; where a stop leads; the branches
  | Say Text r                                     -- the guard speaks; no reply expected
  | Check [(Text, r)] r                            -- child per poster matched, and the child when none does
  | Weigh Text (Text, r) (Text, r) (Text, r)       -- a question, and the sound / thin / false levels
  | Happen r                                       -- something may happen here
  | Knot Text r                                    -- a named point the script comes back to
  | Verdict Action r                               -- the guard decides, and the story goes on
  | End

-- An admission and a contradiction are different things and deserve
-- different lines, so the tripwire is a disjunction with a continuation per
-- outcome rather than one Noul standing for both.
data Trip r = Trip r r   -- where an admission leads, and where a contradiction leads

instance Functor Trip where
  fmap f (Trip a b) = Trip (f a) (f b)

instance Functor GuardF where
  fmap f (Ask line trip bs) = Ask line (fmap (fmap f) trip) (mapUniform f bs)
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
gate w = askOrigin
  where
    -- The approach: three questions, the posters if the road or the errand warrants, then the weighing.
    askOrigin = askPatient "origin" "Halt. Where do you hail from, traveller?" True w.neighbours $ \origin ->
      if origin == "evasive" then askCargo False else askPurpose origin $ \purpose ->
        -- The posters concern the north road and the taverns; nobody else is held against them.
        askCargo (origin == "north_road" || purpose == "tavern")
    askPurpose origin = askPatient ("purpose_after_" <> origin) "And what brings you to Greyhaven?" False w.places
    askCargo suspect = askPatient (if suspect then "cargo_suspect" else "cargo") "Anything to declare? Weapons, goods, anything the customs officer should see?" False
      (w.banned ++ [("nothing", "Nothing to declare: personal effects, ordinary goods, a bonded weapon")]) $ \cargo ->
        -- Contraband is a rule, not a judgment. A weapon gets bonded at the post and the talk goes on;
        -- smuggled goods end it.
        case cargo of
          "smuggled_goods" -> verdict TurnAway
          "unbound_weapon" -> say "Then bond it. There's cord by the post; loop it through the guard and knot it. Good." (onward suspect)
          _ -> onward suspect
    onward suspect = if suspect then checkPosters weigh else weigh

    -- Every question on the approach takes a sidetrack in its stride. Someone playing a part, asking
    -- the questions back, flattering, name-dropping, drunk, pleading, or lost for words gets a retort
    -- and the question again, twice at most, then counts as evasive. A threat closes the gate; a bribe
    -- fetches the captain. Where pressEvasive is set, a first evasive answer is asked again too.
    --
    -- Each retry level is a knot, because every sidetrack at one level leads to the same next level.
    -- Without that the printed script would re-expand one subtree per sidetrack, which is exponential;
    -- the name must identify the node, so it carries whatever the continuation was built from.
    --
    -- Every alternative carries the node it leads to, so the branch the provider picks is the branch
    -- the program runs: no table from a key to a node, and no case on a string.
    askPatient name line pressEvasive offered' k = go (0 :: Int)
      where
        go n = knot (name <> "_" <> T.pack (show n)) $ askLine line Nothing $ uniform
          (  rows #branch [(l, m, k l) | (l, m) <- offered']
          .| rows #aside [(l, m, aside' retort) | (l, m, retort) <- sidetracks line]
          .| alt #evasive evasiveMeaning evasiveGoes
          .| alt #threat threatMeaning (say "Threaten the watch at its own gate and you can spend the night on the wrong side of it." (verdict TurnAway))
          .| alt #bribe bribeMeaning (say "Did you just offer the watch coin? At its own gate?" (verdict SendForCaptain)) )
          where
            aside' retort = if n < 2 then say retort (go (n + 1)) else k "evasive"
            evasiveGoes
              | pressEvasive && n == 0 = say "I'll ask once more, and I'd think about the answer this time." (go 1)
              | otherwise = k "evasive"
    sidetracks :: Text -> [(Text, Text, Text)]
    sidetracks line =
      [ ("nonsense", nonsenseMeaning, callOuts !! (T.length line `mod` length callOuts))
      , ("question_back", "Asks the guard a question instead of answering: who is asking, why the questions, what the curfew is about"
        , "I'm the one asking tonight. The curfew's the council's doing, not mine. Now:")
      , ("flattery", "Flatters, sweet-talks, or compliments the guard"
        , "Save it. It's late and I've heard better. Now:")
      , ("name_drop", "Claims to know the captain, the council, or someone important, or demands special treatment"
        , "Everyone knows the captain tonight. It doesn't change the question.")
      , ("drunk", "Slurs, rambles, or is plainly drunk"
        , "Take a breath. Say it slowly, and say it once.")
      , ("sob_story", "Pleads, tells a hard-luck story, or begs before being asked anything"
        , "Nobody asked for your life story. Just the question.")
      , ("lost", "Does not seem to understand the question, or answers in another tongue"
        , "Slowly, then. Simple words.") ]
    threatMeaning = "Threatens the guard or the watch" :: Text
    bribeMeaning = "Offers coin, a favour, or anything of value to be let through" :: Text
    callOuts =
      [ "Are you having me on right now? Once more."
      , "Is this a game to you? Try that again, plainly."
      , "I've had drunks make more sense at this gate. Again." ]

    checkPosters continue = Fix (Check [(k, verdict SendForCaptain) | (k, _) <- w.posters] continue)

    -- A story that does not hold up gets one plain re-ask before any verdict. Dodging that closes the
    -- gate; otherwise thin is let through with a warning and false is turned away. The captain is for
    -- posters, bribes, and runners.
    weigh = knot "weigh" (weighInto (verdict Admit) pressOnce pressOnce)
    pressOnce = knot "press_again" $
      askLine "Hm. That doesn't quite hang together. Once more, plainly: what brings you in, and what have you got with you?" Nothing $ uniform
        (  alt #straight "Answers plainly, with detail a guard could check" plainly
        .| alt #changes_story "Gives an account that differs from what they said before" plainly
        .| alt #evasive evasiveMeaning (verdict TurnAway)
        .| alt #threat threatMeaning (say "That is the wrong thing to say to me tonight." (verdict TurnAway))
        .| alt #bribe bribeMeaning (say "Did you just offer the watch coin? At its own gate?" (verdict SendForCaptain))
        .| alt #nonsense nonsenseMeaning (say "I have no patience left for the act. Off with you." (verdict TurnAway)) )
      where plainly = weighInto (verdict Admit) (say "Fine. But I've got my eye on you." (verdict Admit)) (verdict TurnAway)
    weighInto sound thin false = Fix (Weigh "Taken together, does this traveller's story hold up?"
      ("The answers fit each other and the road they came by; an ordinary traveller on an ordinary errand sounds like this, even when brief or odd in manner", sound)
      ("A real gap: a claim that cannot be squared with the rest, a question dodged, or an errand that does not fit the cargo", thin)
      ("The story contradicts itself, the posters, or the guard's knowledge of the roads", false))

    -- Every verdict opens onto a hub, and every hub is tied back into itself.
    verdict act = Fix (Verdict act (hub act))
    hub Admit = admitted
    hub TurnAway = turnedAway
    hub SendForCaptain = held

    admitted = knot "gate" $ happen $
      askLine "Anything else before you go through?"
        (Just (Trip (say "Wait. Say that again." weigh) (say "That is not what you told me a moment ago." weigh))) $ uniform
        (  rows #poster [(k, "Asks about the " <> k <> " on the posters, or the reward", answered k) | (k, _) <- w.posters]
        .| rows #place [(k, "Asks the way to the " <> k <> ", whether it is open, what goes on there, or for what it offers: " <> d, answered k) | (k, d) <- w.places]
        .| rows #topic
             [ ("curfew", "Asks about the curfew: when the bell goes, what it means for them tonight", answered "curfew")
             , ("captain", "Asks about the captain or the watch", answered "captain")
             , ("rumour", "Asks about the robbery, or for news and gossip", answered "rumour")
             , ("chat", "Small talk, a remark about the night, or anything else", answered "chat") ]
        .| alt #flattery "Flatters, sweet-talks, or compliments the guard" (say "Save it. Through you go, before I change my mind." admitted)
        .| alt #threat threatMeaning (say "Threaten the watch and you'll not be going through after all." (verdict TurnAway))
        .| alt #nonsense nonsenseMeaning (say "Very funny. Anything else, or are we done?" admitted)
        .| alt #leave "Signals they are done, however indirectly: says goodbye, thanks the guard, says they will get out of the way, or asks whether they may go"
             (say "Then go on. And mind the curfew." end) )
      where answered topic = say (fromMaybe "Mm. Long night. Move along when you're ready." (lookup topic smallTalk)) admitted

    turnedAway = knot "turned_away" $ happen $
      askLine "The gate's closed to you tonight. Unless you've something to add." Nothing $ uniform
        (  alt #explain "Adds to their story, gives a reason, or names someone who can vouch for them" (say "Go on, then. All of it, from the start." weigh)
        .| alt #bribe "Offers money, a favour, or anything of value to the guard" (say "Did you just offer the watch coin? At its own gate?" (verdict SendForCaptain))
        .| alt #insult "Insults, mocks, or threatens the guard" (say "Say that again and it's the captain you'll be explaining yourself to." turnedAway)
        .| alt #beg "Pleads, appeals to pity, or asks for an exception" (say "Save it. I've heard better from the drunks at the Broken Wheel." turnedAway)
        .| alt #chat "Small talk, a question, or anything else" (say "The gate's still closed." turnedAway)
        .| alt #nonsense nonsenseMeaning (say "Play the fool somewhere else. The gate's still closed." turnedAway)
        .| alt #leave "Gives up, says goodbye, or turns to go" (say "Then go. The road's that way." end) )

    held = knot "held" $ happen $
      -- No tripwire here: there is nothing left to escalate to, and it would only steal the branches below.
      askLine "Stand there. The captain's on his way. Anything to say for yourself?" Nothing $ uniform
        -- A held traveller can talk their way down to the road, never straight through the gate.
        (  alt #explain "Tries to explain, gives an account, or names someone who can vouch for them"
             (say "Go on. Slowly." (weighInto (verdict TurnAway) (verdict TurnAway) (verdict SendForCaptain)))
        .| alt #name_drop "Claims rank, connections, or acquaintance with someone important as a reason to be released"
             (say "Then your important friend can come and say so to the captain." held)
        .| alt #protest "Protests innocence, objects, or demands to be released" (say "Tell it to the captain." held)
        .| alt #threaten "Threatens the guard or the watch" (say "Threatening the watch at its own gate. Bold." held)
        .| alt #run "Tries to run, push past, or escape" (say "Runner! Nobody runs from this gate. Not far." end)
        .| alt #chat "Anything else" (say "Stand there." held)
        .| alt #nonsense nonsenseMeaning (say "Save the act for the captain. Stand there." held) )

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

    evasiveMeaning = "Does not say, changes the subject, or answers a different question" :: Text
    nonsenseMeaning = "Nonsense, gibberish, or play-acting: mocks the guard, claims to be the guard, gives the guard orders, or talks as if to a machine"

    askLine line trip offers = Fix (Ask line trip offers)
    say line next = Fix (Say line next)
    knot name body = Fix (Knot name body)
    happen next = Fix (Happen next)
    end = Fix End

-- | A runtime group whose rows are written as key, wording and the node
-- they lead to. The row's node is the payload, so the group carries what
-- every other alternative carries and the whole chain is uniform.
rows :: forall k r. KnownSymbol k => Label k -> [(Text, Text, r)] -> Offers (k ::* r)
rows l xs = mapCarried child (many l (\(k, _, _) -> k) (\(_, m, _) -> m) xs)
  where
    child :: (Text, Text, r) -> r
    child (_, _, c) = c

-- ---------------------------------------------------------------------------
-- Fold one: print the script, each knot once
-- ---------------------------------------------------------------------------

-- The carrier threads the knots already printed through the children in
-- order, so a hub reached from many places is printed once and named after.
render :: GuardF ([Text] -> ([Text], Text)) -> [Text] -> ([Text], Text)
render node tied = case node of
  Ask line trip offers ->
    let bs = branches offers
        (tied1, tripLines) = case trip of
          Nothing -> (tied, [])
          Just (Trip admits contradicts) ->
            let (tA, outA) = admits tied
                (tB, outB) = contradicts tA
            in (tB, branch "if it admits something" outA ++ branch "if it contradicts the story" outB)
        (tied2, rest) = mapAccumL (\acc (k, m, r) -> let (acc', out) = r acc in (acc', branch (k <> "  (" <> m <> ")") out)) tied1 bs
    in (tied2, T.unlines (("ask  " <> quote line) : tripLines ++ concat rest))
  Say line next -> let (tied', out) = next tied in (tied', "say  " <> quote line <> "\n" <> out)
  Check ms none ->
    let (tied1, matched) = mapAccumL (\acc (k, r) -> let (acc', out) = r acc in (acc', branch ("matches " <> k) out)) tied ms
        (tied2, rest) = none tied1
    in (tied2, T.unlines ("check the posters" : concat matched ++ branch "no poster matches" rest))
  Weigh q (a, x) (b, y) (c, z) ->
    let (tied', ls) = mapAccumL (\acc (l, m, r) -> let (acc', out) = r acc in (acc', branch (l <> "  (" <> m <> ")") out)) tied
                        [("sound", a, x), ("thin", b, y), ("false", c, z)]
    in (tied', T.unlines (("weigh  " <> quote q) : concat ls))
  Happen next -> let (tied', out) = next tied in (tied', "something may happen\n" <> out)
  Knot name body
    | name `elem` tied -> (tied, "back to " <> name <> "\n")
    | otherwise -> let (tied', out) = body (name : tied) in (tied', name <> ":\n" <> out)
  Verdict v next -> let (tied', out) = next tied in (tied', "verdict " <> T.pack (show v) <> "\n" <> out)
  End -> (tied, "end\n")

-- A leaf is shown on the branch's own line; a subtree is indented under it.
branch :: Text -> Text -> [Text]
branch label child = case T.lines child of
  [leaf] -> ["  " <> label <> "  -> " <> leaf]
  ls -> ("  " <> label) : map ("    " <>) ls

quote :: Text -> Text
quote t = "\"" <> t <> "\""

-- The hub topics the guard will answer more than one of in a breath.
topics :: World -> [Text]
topics w = map fst w.posters ++ map fst w.places ++ ["curfew", "captain", "rumour"]

-- ---------------------------------------------------------------------------
-- Fold two: play the script against a traveller
-- ---------------------------------------------------------------------------

data Turn = Turn { asked :: Text, replied :: Text, heardAs :: Text, sureness :: Double }
data Traveller = Traveller { turns :: [Turn], standing :: [Action], happened :: [Text], told :: [Text], here :: World }
data Outcome = Outcome (Maybe Action) [Turn]

type Play = Traveller -> IO Outcome
type Transport = Value -> IO (Either Text Value)

-- The carrier is a program plus one fact about the subtree it came from: the line it opens with, if
-- any. That fact lets an Ask answer a runner-up topic in the same breath as the winner.
data Node = Node { play :: Play, quip :: Maybe Text }

interpret :: Session IO -> GuardF Node -> Node
interpret sess = \case
  Ask line trip offers -> program $ \t -> do
    let askedBefore = length [() | u <- t.turns, u.asked == line]
        variants = fromMaybe [] (lookup line t.here.again)
    guard (if askedBefore == 0 || null variants then line else variants !! min (askedBefore - 1) (length variants - 1))
    reply <- hear
    let st = exchange t line reply
        bs = branches offers

        -- A reply can raise more than one topic at once, and a choice cannot
        -- say so: its distribution is uncertainty about which single branch
        -- fits, not evidence that several do. Things that can be true at the
        -- same time are a Noul each, asked per topic in the same call, so
        -- "which way to the temple, and when's the bell?" gets both answers
        -- without a second round trip.
        topical = [(k, meaning, node) | (k, meaning, node) <- bs, k `elem` topics t.here]
        alsoQ = each (\(k, _, _) -> k) (\(_, meaning, _) -> noul (T.unwords
                  [ "Is the traveller asking the guard about this, or asking for it?"
                  , "Mentioning it in passing, denying it, or answering the guard's own question about it is not asking."
                  , "The topic:", meaning ])) topical

        follow winner heard sureness spread alsos = do
          let others = [(k, m) | (k, m) <- spread, k /= heard, m >= 0.2]
              -- Judged, not thresholded by hand; a topic the guard has
              -- already spoken to is not raised again. The battery hands back
              -- the row the question was built from, so the node that answers
              -- the topic is already here and there is nothing to look up.
              raised = [ (k, node, n) | ((k, _, node), n) <- alsos, k /= heard, holds lenient n ]
              alsoSaid = nub [ q | (_, node, _) <- raised, Just q <- [node.quip]
                             , Just q /= winner.quip, q `notElem` t.told ]
          aside ("heard " <> heard <> " " <> pct sureness
            <> if null others then "" else "  (also " <> T.intercalate ", " [k <> " " <> pct m | (k, m) <- others] <> ")")
          unless (null raised) (aside ("also asked " <> T.intercalate ", " [k <> " " <> pct n.yes | (k, _, n) <- raised]))
          mapM_ guard alsoSaid
          winner.play (t `saw` Turn line reply heard sureness)
            { told = alsoSaid ++ maybe [] pure winner.quip ++ t.told }

        -- What would stop a traveller mid-sentence, as a disjunction whose
        -- alternatives carry their own optional continuations. The author
        -- explicitly gives "nothing new" the payload Nothing: no diversion.
        -- Every outcome has a payload; its meaning is the author's choice.
        -- Stopping to ask again is cheap and reversible, so it is
        -- settled under the policy for starting something, not the one for
        -- receipts: the guard is meant to err towards asking.
        -- The wording names a state field, and the name is the state's own:
        -- a field this state does not have is a compile error, not a
        -- question the provider silently reads as being about nothing.
        stopping (Trip admits contradicts) =
          choice "Does this reply give the guard fresh reason to stop the traveller where they stand?"
            (  alt #admits (T.unwords
                 [ "The reply owns up to something the standing orders forbid: goods hidden from the customs officer,"
                 , "a weapon not bonded, a crime, or being someone the posters want. Read an admission made in passing"
                 , "or as a joke as an admission." ])
                 (Just admits)
            .| alt #contradicts (T.unwords
                 [ "The reply cannot both be true and leave", field #conversation_so_far st, "standing: it names a different road in,"
                 , "a different errand, or different goods than this same traveller already gave, or denies having said"
                 , "what the record shows they said." ])
                 (Just contradicts)
            .| alt #nothing_new (T.unwords
                 [ "The reply adds nothing the guard has not already heard: small talk, a question, a denial, or a"
                 , "repeat or elaboration of what", field #conversation_so_far st, "already contains." ])
                 Nothing )
        -- Stopping a traveller mid-sentence acts on a single reading, so
        -- the step that does it demands a verdict from the policy meant to
        -- decide it. A lenient verdict does not typecheck here.
        stop :: Settled Careful (Maybe Node) -> Maybe Play
        stop (Settled node) = fmap (.play) node
        divert a = case takenUnder careful a of
          Right v | Just go <- stop v -> do
            aside (explain careful a)
            Just <$> go (t `saw` Turn line reply a.key a.mass)
          -- A doubt is not a stop. It is a reading the guard could not make,
          -- and the traveller gets the benefit of it, with a line whenever
          -- the guard nearly stopped them. The doubt carries its own line.
          Left d -> do
            when (a.key /= "nothing_new") (aside ("let it pass: " <> d.why))
            pure Nothing
          _ -> pure Nothing

    -- One packet and one call. The optional tripwire sends nothing when
    -- absent and reads as Maybe, without a synthetic key or singleton list.
    withUniform offers $ \o -> do
      r <- must =<< ask sess st
        (  #branch := choice "Which branch does the traveller's reply take?" o
        :& #also   := alsoQ
        :& #stop   := optional (stopping <$> trip) )
      stopped <- maybe (pure Nothing) divert r.stop
      -- The alternative the provider picked carries the node the author
      -- wrote beside it, so following the answer is taking its payload.
      maybe (follow (taken r.branch) r.branch.key r.branch.mass r.branch.masses r.also) pure stopped

  Say line next -> Node (\t -> guard line >> next.play t) (Just line)

  Check matches none -> program $ \t -> do
    -- One Noul per poster, each carrying its own wording: the per-item
    -- battery. A battery is a question, so it needs no packet around it, and
    -- each answer comes back beside its row, which here is the poster as it
    -- reads tonight and the child the tree wrote for a match.
    -- The rows come from the state that was sent, so the poster a question
    -- is built from is the poster the provider was shown.
    let st = situation t
        wanted = [(k, poster, node) | (k, node) <- matches, Just poster <- [lookup k st.gate.posters]]
    fits <- must =<< ask1 sess st
      (each (\(k, _, _) -> k) (\(_, poster, _) -> noul
        ("Does the traveller's story so far match this wanted poster from " <> field (#gate :/ #posters) st <> "? " <> poster)) wanted)
    let scored = sortOn (Down . (.yes) . snd) fits
    aside ("posters " <> T.intercalate ", " [k <> " " <> pct n.yes | ((k, _, _), n) <- scored])
    -- Holding someone starts something, so the closest poster is judged under
    -- that policy, and the policy's own line says why it went the way it did.
    case scored of
      ((k, _, node), n) : _ -> do
        aside (k <> ": " <> explain careful n)
        if holds careful n then node.play t else none.play t
      [] -> none.play t

  Weigh q (sound, x) (thin, y) (false, z) -> program $ \t -> do
    -- Each level carries what the provider is shown and what the guard does
    -- next, on one line. The highest level half the weight reaches is the
    -- one taken, which is the median; there is no string to dispatch on and
    -- no second list that could fall out of step with this one.
    a <- must =<< ask1 sess (situation t)
      (score q (level #sound sound x .| level #thin thin y .| level #false false z))
    let (landed, next) = graded 0.5 a
    aside ("weighed " <> landed <> "  (" <> T.intercalate ", " [k <> " " <> pct m | (k, m) <- a.masses]
      <> "; thin or worse " <> pct (massAtOrAbove #thin a) <> ")")
    next.play t

  Happen next -> program $ \t -> do
    -- The night moves at its own pace: something can happen at most every other exchange, and
    -- which three events are on offer turns with what has been said, so no event always comes first.
    let unused = [h | h <- t.here.happenings, h.tag `notElem` t.happened]
        turned = let n = sum [T.length u.replied | u <- t.turns] `mod` max 1 (length unused)
                 in take 3 (drop n unused ++ take n unused)
    if null unused || even (length t.turns) then next.play t else do
      a <- must =<< ask1 sess (situation t)
        (choice "Which of these fits this moment at the gate, given what has happened so far?"
          (alt #nothing "The night goes on; nothing in particular happens" Nothing
           .| mapCarried Just (many #happening (.tag) (.blurb) turned)))
      -- Firing an event changes the world, so it goes through a policy too.
      -- Every alternative already carries what to do, the quiet one
      -- included, so the policy adds no handlers. If the night reads as
      -- ambiguous, nothing in particular happens.
      case takenUnder lenient a of
        Left _ -> next.play t
        Right (Settled Nothing) -> next.play t
        Right (Settled (Just h)) -> do
             aside ("happening " <> h.tag <> "; " <> explain lenient a)
             narrate h.seen
             guard (h.said (headMay t.standing))
             let t' = t { happened = h.tag : t.happened, here = h.apply t.here }
             -- The captain's rounds end a held traveller's night; everyone else watches him pass.
             if h.tag == "captain" && take 1 t.standing == [SendForCaptain]
               then pure (Outcome (Just SendForCaptain) (reverse t'.turns))
               else next.play t'

  Knot _ next -> next

  Verdict v next -> program $ \t -> do
    guard (spoken t.here v)
    next.play t { standing = v : t.standing }

  End -> program $ \t -> pure (Outcome (headMay t.standing) (reverse t.turns))
  where
    program p = Node p Nothing
    saw t turn = t { turns = turn : t.turns }
    headMay xs = case xs of { x : _ -> Just x; [] -> Nothing }

spoken :: World -> Action -> Text
spoken w Admit | w.bellGone = "Right. You're in, and quick about it: the bell's gone."
               | otherwise = "Right. You're in."
spoken _ TurnAway = "Not tonight. Move along, and don't let me see you at this gate again."
spoken _ SendForCaptain = "Guards! Hold this one. Someone fetch the captain."

-- What every call sees: the standing orders, the gate as the guard knows
-- it tonight, the conversation so far, and the verdicts already spoken.
-- The state keeps its Haskell values, so a question built from
-- @st.gate.posters@ is built from what the provider was shown, and wording
-- naming a field is checked against the field.
type Gate = Packet
  (  "city" ::= Text :& "roads_in" ::= [(Text, Text)] :& "places" ::= [(Text, Text)]
  :& "not_allowed_through" ::= [(Text, Text)] :& "posters" ::= [(Text, Text)] ) Fields

type Exchanged = Packet ("guard" ::= Text :& "traveller" ::= Text :& "taken_as" ::= Text) Fields

type Standing =
     "standing_orders" ::= Text
  :& "gate" ::= Gate
  :& "conversation_so_far" ::= [Exchanged]
  :& "verdicts_so_far" ::= [Text]
  :& "happened_so_far" ::= [Text]

type Situation = State Standing
type Exchange = State (Standing :& "question" ::= Text :& "reply" ::= Text)

-- The node that hears a reply sends the reply too. A packet is a value and
-- packets join, so the shared part is written once.
theGate :: Traveller -> Packet Standing Fields
theGate t =
     #standing_orders := t.here.edict
  :& #gate :=
      (  #city := ("Greyhaven" :: Text)
      :& #roads_in := t.here.neighbours
      :& #places := t.here.places
      :& #not_allowed_through := t.here.banned
      :& #posters := t.here.posters )
  :& #conversation_so_far := [#guard := u.asked :& #traveller := u.replied :& #taken_as := u.heardAs | u <- reverse t.turns]
  :& #verdicts_so_far := map (T.pack . show) (reverse t.standing)
  :& #happened_so_far := reverse t.happened

situation :: Traveller -> Situation
situation t = state (theGate t)

exchange :: Traveller -> Text -> Text -> Exchange
exchange t line reply = state (theGate t :& #question := line :& #reply := reply)

-- ---------------------------------------------------------------------------
-- The gate, the transport, and the terminal
-- ---------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= \case
  ["--script"] -> TIO.putStr (snd (cata render (gate world) []))
  [] -> do
    hSetBuffering stdout NoBuffering
    calls <- newIORef (0 :: Int, 0 :: Int, 0 :: Int)
    let node = cata (interpret (session (counted calls curl) jevLatest)) (gate world)
    Outcome final heard <- node.play (Traveller [] [] [] [] world)
    TIO.putStrLn ""
    TIO.putStrLn ("verdict: " <> maybe "none" (T.pack . show) final)
    mapM_ (\u -> TIO.putStrLn ("  " <> quote u.replied <> " -> " <> u.heardAs <> " " <> pct u.sureness)) heard
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
    Right v -> modifyIORef' ref (\(n, i, o) -> (n + 1, i + count "input_tokens" v, o + count "output_tokens" v))
    Left _ -> pure ()
  pure r
  where
    count k (Object m) | Just (Object u) <- KeyMap.lookup "usage" m, Just (Number x) <- KeyMap.lookup (Key.fromText k) u = round x
    count _ _ = 0

-- The transport: scripts/transport.sh holds the key and calls curl.
curl :: Transport
curl body = do
  (Just hin, Just hout, _, ph) <- createProcess (proc "scripts/transport.sh" []) { std_in = CreatePipe, std_out = CreatePipe }
  BL.hPut hin (Aeson.encode body) >> hClose hin
  out <- BL.hGetContents hout
  _ <- waitForProcess ph
  pure (either (Left . T.pack) Right (Aeson.eitherDecode out))
