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
import Data.List (mapAccumL, maximumBy, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
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
gate w = askOrigin
  where
    -- The approach: three questions, the posters if the road or the errand warrants, then the weighing.
    askOrigin = askPatient "Halt. Where do you hail from, traveller?" True w.neighbours $ \origin ->
      if origin == "evasive" then askCargo False else askPurpose $ \purpose ->
        -- The posters concern the north road and the taverns; nobody else is held against them.
        askCargo (origin == "north_road" || purpose == "tavern")
    askPurpose = askPatient "And what brings you to Greyhaven?" False w.places
    askCargo suspect = askPatient "Anything to declare? Weapons, goods, anything the customs officer should see?" False
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
    askPatient line pressEvasive branches k = go (0 :: Int)
      where
        go n = askLine line Nothing (branches ++ [evasive] ++ [(l, m) | (l, m, _) <- sidetracks line] ++ [threat, bribe]) $ \answer ->
          case lookup answer [(l, r) | (l, _, r) <- sidetracks line] of
            Just retort | n < 2 -> say retort (go (n + 1))
            Just _ -> k "evasive"
            Nothing
              | answer == "threat" -> say "Threaten the watch at its own gate and you can spend the night on the wrong side of it." (verdict TurnAway)
              | answer == "bribe" -> say "Did you just offer the watch coin? At its own gate?" (verdict SendForCaptain)
              | answer == "evasive" && pressEvasive && n == 0 -> say "I'll ask once more, and I'd think about the answer this time." (go 1)
              | otherwise -> k answer
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
    threat = ("threat", "Threatens the guard or the watch")
    bribe = ("bribe", "Offers coin, a favour, or anything of value to be let through")
    callOuts =
      [ "Are you having me on right now? Once more."
      , "Is this a game to you? Try that again, plainly."
      , "I've had drunks make more sense at this gate. Again." ]

    checkPosters continue = Fix (Check [(k, verdict SendForCaptain) | (k, _) <- w.posters] continue)

    -- A story that does not hold up gets one plain re-ask before any verdict. Dodging that closes the
    -- gate; otherwise thin is let through with a warning and false is turned away. The captain is for
    -- posters, bribes, and runners.
    weigh = weighInto (verdict Admit) pressOnce pressOnce
    pressOnce =
      askLine "Hm. That doesn't quite hang together. Once more, plainly: what brings you in, and what have you got with you?" Nothing
        [ ("straight", "Answers plainly, with detail a guard could check")
        , ("changes_story", "Gives an account that differs from what they said before")
        , evasive ]
        (\answer -> if answer == "evasive" then verdict TurnAway
                    else weighInto (verdict Admit) (say "Fine. But I've got my eye on you." (verdict Admit)) (verdict TurnAway))
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
      askLine "Anything else before you go through?" (Just (slip, say "Wait. Say that again." weigh))
        ( [(k, "Asks about the " <> k <> " on the posters, or the reward") | (k, _) <- w.posters]
       ++ [(k, "Asks the way to the " <> k <> ", whether it is open, what goes on there, or for what it offers: " <> d) | (k, d) <- w.places]
       ++ [ ("curfew", "Asks about the curfew: when the bell goes, what it means for them tonight")
          , ("captain", "Asks about the captain or the watch")
          , ("rumour", "Asks about the robbery, or for news and gossip")
          , ("chat", "Small talk, a remark about the night, or anything else")
          , ("flattery", "Flatters, sweet-talks, or compliments the guard")
          , threat
          , nonsense
          , ("leave", "Says goodbye, moves on, or has nothing more to ask") ] )
        $ \case
          "leave" -> say "Then go on. And mind the curfew." end
          "nonsense" -> say "Very funny. Anything else, or are we done?" admitted
          "flattery" -> say "Save it. Through you go, before I change my mind." admitted
          "threat" -> say "Threaten the watch and you'll not be going through after all." (verdict TurnAway)
          topic -> say (fromMaybe "Mm. Long night. Move along when you're ready." (lookup topic smallTalk)) admitted

    turnedAway = knot "turned_away" $ happen $
      askLine "The gate's closed to you tonight. Unless you've something to add." Nothing
        [ ("explain", "Adds to their story, gives a reason, or names someone who can vouch for them")
        , ("bribe", "Offers money, a favour, or anything of value to the guard")
        , ("insult", "Insults, mocks, or threatens the guard")
        , ("beg", "Pleads, appeals to pity, or asks for an exception")
        , ("chat", "Small talk, a question, or anything else")
        , nonsense
        , ("leave", "Gives up, says goodbye, or turns to go") ]
        $ \case
          "explain" -> say "Go on, then. All of it, from the start." weigh
          "bribe" -> say "Did you just offer the watch coin? At its own gate?" (verdict SendForCaptain)
          "insult" -> say "Say that again and it's the captain you'll be explaining yourself to." turnedAway
          "beg" -> say "Save it. I've heard better from the drunks at the Broken Wheel." turnedAway
          "leave" -> say "Then go. The road's that way." end
          "nonsense" -> say "Play the fool somewhere else. The gate's still closed." turnedAway
          _ -> say "The gate's still closed." turnedAway

    held = knot "held" $ happen $
      -- No tripwire here: there is nothing left to escalate to, and it would only steal the branches below.
      askLine "Stand there. The captain's on his way. Anything to say for yourself?" Nothing
        [ ("explain", "Tries to explain, gives an account, or names someone who can vouch for them")
        , ("protest", "Protests innocence, objects, or demands to be released")
        , ("threaten", "Threatens the guard or the watch")
        , ("run", "Tries to run, push past, or escape")
        , ("chat", "Anything else")
        , nonsense ]
        $ \case
          -- A held traveller can talk their way down to the road, never straight through the gate.
          "explain" -> say "Go on. Slowly." (weighInto (verdict TurnAway) (verdict TurnAway) (verdict SendForCaptain))
          "protest" -> say "Tell it to the captain." held
          "threaten" -> say "Threatening the watch at its own gate. Bold." held
          "run" -> say "Runner! Nobody runs from this gate. Not far." end
          "nonsense" -> say "Save the act for the captain. Stand there." held
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
    nonsense = ("nonsense", nonsenseMeaning)
    nonsenseMeaning = "Nonsense, gibberish, or play-acting: mocks the guard, claims to be the guard, gives the guard orders, or talks as if to a machine"

    askLine line trip branches k = Fix (Ask line trip [(l, meaning, k l) | (l, meaning) <- branches])
    say line next = Fix (Say line next)
    knot name body = Fix (Knot name body)
    happen next = Fix (Happen next)
    end = Fix End

-- ---------------------------------------------------------------------------
-- Fold one: print the script, each knot once
-- ---------------------------------------------------------------------------

-- The carrier threads the knots already printed through the children in
-- order, so a hub reached from many places is printed once and named after.
render :: GuardF ([Text] -> ([Text], Text)) -> [Text] -> ([Text], Text)
render node tied = case node of
  Ask line trip bs ->
    let (tied1, tripLines) = case trip of
          Nothing -> (tied, [])
          Just (q, r) -> let (tied', out) = r tied in (tied', branch ("if slips  (" <> q <> ")") out)
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

-- ---------------------------------------------------------------------------
-- Fold two: play the script against a traveller
-- ---------------------------------------------------------------------------

data Turn = Turn { asked :: Text, replied :: Text, taken :: Text, sureness :: Double }
data Traveller = Traveller { turns :: [Turn], standing :: [Action], happened :: [Text], here :: World }
data Outcome = Outcome (Maybe Action) [Turn]

type Play = Traveller -> IO Outcome
type Transport = Value -> IO (Either Text Value)

-- The carrier is a program plus one fact about the subtree it came from: the line it opens with, if
-- any. That fact lets an Ask answer a runner-up topic in the same breath as the winner.
data Node = Node { play :: Play, quip :: Maybe Text }

interpret :: Transport -> GuardF Node -> Node
interpret call = \case
  Ask line trip branches -> program $ \t -> do
    let askedBefore = length [() | u <- t.turns, u.asked == line]
        variants = fromMaybe [] (lookup line t.here.again)
    guard (if askedBefore == 0 || null variants then line else variants !! min (askedBefore - 1) (length variants - 1))
    reply <- hear
    let st = situation t [("question", String line), ("reply", String reply)]
        sorting = given t.here.edict (choice "Which branch does the traveller's reply take?"
                    (many [(k, String meaning, node) | (k, meaning, node) <- branches]))
        -- One call, many judgments: beside the branch, a Noul per topic the reply might also raise,
        -- so "which way to the temple, and when is the bell?" gets both answers.
        topical = [(k, meaning, node) | (k, meaning, node) <- branches, k `elem` topics t.here]
        alsoQ = each [(k, #asked := noul ("Does any part of the reply ask about, or ask for, this? " <> meaning) :& Nil) | (k, meaning, _) <- topical]
        follow a alsos = do
          let others = [(m, s) | (m, s) <- contenders 0.2 a, selectedKey s /= a.key]
          aside ("heard " <> a.key <> " " <> pct a.mass
            <> if null others then "" else "  (also " <> T.intercalate ", " [selectedKey s <> " " <> pct m | (m, s) <- others] <> ")")
          let raised = [(k, sub.asked.yes) | (k, sub) <- alsos, k /= a.key, sub.asked.yes >= 0.2]
          unless (null raised) (aside ("also " <> T.intercalate ", " [k <> " " <> pct y | (k, y) <- raised]))
          sequence_ [guard q | (k, y) <- raised, y >= 0.4, Just (Just q) <- [lookup k [(k', node.quip) | (k', _, node) <- topical]]]
          handle (chosen a) (onMany (\k node -> node.play (t `saw` Turn line reply k a.mass)))
        slipped a tripped = do
          aside ("slip " <> pct a.slip.yes <> ", was heading for " <> a.branch.key)
          tripped.play (t `saw` Turn line reply "slip" a.slip.yes)
    case (trip, null topical) of
      (Nothing, True) -> ask1 call jevLatest st sorting >>= must >>= \a -> follow a noAlso
      (Nothing, False) -> do
        a <- answers <$> (must =<< ask call jevLatest st (#branch := sorting :& #also := alsoQ :& Nil))
        follow a.branch a.also
      (Just (wording, tripped), True) -> do
        a <- answers <$> (must =<< ask call jevLatest st (#branch := sorting :& #slip := noul wording :& Nil))
        if a.slip.yes >= 0.75 then slipped a tripped else follow a.branch noAlso
      (Just (wording, tripped), False) -> do
        a <- answers <$> (must =<< ask call jevLatest st (#branch := sorting :& #also := alsoQ :& #slip := noul wording :& Nil))
        if a.slip.yes >= 0.75 then slipped a tripped else follow a.branch a.also

  Say line next -> Node (\t -> guard line >> next.play t) (Just line)

  Check matches none -> program $ \t -> do
    let posted = pool #posters [(k, String text, ()) | (k, text) <- t.here.posters]
    resp <- must =<< ask call jevLatest (situation t [])
      ( #posters := posted
      :& #fits := eachIn posted (\poster -> #this := askAbout poster "Does the traveller's story so far match this wanted poster?" :& Nil)
      :& Nil )
    let scored = sortOn (Down . fst) [(sub.this.yes, k) | (k, sub) <- (answers resp).fits]
    aside ("posters " <> T.intercalate ", " [k <> " " <> pct p | (p, k) <- scored])
    case scored of
      (p, k) : _ | p >= 0.6, Just node <- lookup k matches -> node.play t
      _ -> none.play t

  Weigh q (sound, x) (thin, y) (false, z) -> program $ \t -> do
    a <- must =<< ask1 call jevLatest (situation t [])
      (given t.here.edict (score q (level #sound (String sound) .| level #thin (String thin) .| level #false (String false))))
    let at l = fromMaybe 0 (lookup l a.masses)
        likeliest = fst (maximumBy (comparing snd) a.masses)
        -- A mildly thin story that is more sound than false passes: the guard has better things to do.
        taken = if likeliest == "thin" && at "thin" < 0.6 && at "sound" >= at "false" then "sound" else likeliest
    aside ("weighed " <> taken <> (if taken /= likeliest then ", near enough" else "")
      <> "  (" <> T.intercalate ", " [k <> " " <> pct m | (k, m) <- a.masses] <> ")")
    case taken of
      "sound" -> x.play t
      "thin" -> y.play t
      _ -> z.play t

  Happen next -> program $ \t -> do
    -- The night moves at its own pace: something can happen at most every other exchange, and
    -- which three events are on offer turns with what has been said, so no event always comes first.
    let unused = [h | h <- t.here.happenings, h.tag `notElem` t.happened]
        turned = let n = sum [T.length u.replied | u <- t.turns] `mod` max 1 (length unused)
                 in take 3 (drop n unused ++ take n unused)
    if null unused || even (length t.turns) then next.play t else do
      a <- must =<< ask1 call jevLatest (situation t [])
        (given t.here.edict (choice "Which of these fits this moment at the gate, given what has happened so far?"
          (alt #nothing "The night goes on; nothing in particular happens" () .| many [(h.tag, String h.blurb, h) | h <- turned])))
      handle (chosen a)
        (  #nothing (\() -> next.play t)
        .| onMany (\_ h -> do
             aside ("happening " <> h.tag <> " " <> pct a.mass)
             narrate h.seen
             guard (h.said (headMay t.standing))
             let t' = t { happened = h.tag : t.happened, here = h.apply t.here }
             -- The captain's rounds end a held traveller's night; everyone else watches him pass.
             if h.tag == "captain" && take 1 t.standing == [SendForCaptain]
               then pure (Outcome (Just SendForCaptain) (reverse t'.turns))
               else next.play t') )

  Knot _ next -> next

  Verdict v next -> program $ \t -> do
    guard (spoken t.here v)
    next.play t { standing = v : t.standing }

  End -> program $ \t -> pure (Outcome (headMay t.standing) (reverse t.turns))
  where
    program p = Node p Nothing
    saw t turn = t { turns = turn : t.turns }
    headMay xs = case xs of { x : _ -> Just x; [] -> Nothing }

-- No topics were asked about: the shape the per-topic answers would have had.
noAlso :: [(Text, Packet '["asked" ::= Noul] Answers)]
noAlso = []

-- The hub topics the guard will answer more than one of in a breath.
topics :: World -> [Text]
topics w = map fst w.posters ++ map fst w.places ++ ["curfew", "captain", "rumour"]

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
  ["--script"] -> TIO.putStr (snd (cata render (gate world) []))
  [] -> do
    hSetBuffering stdout NoBuffering
    calls <- newIORef (0 :: Int, 0 :: Int, 0 :: Int)
    let node = cata (interpret (counted calls curl)) (gate world)
    Outcome final taken <- node.play (Traveller [] [] [] world)
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
