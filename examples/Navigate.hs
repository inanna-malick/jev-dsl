{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | A semantic code navigator with no frontier model in the loop.
--
-- Ask a question about this library's own source. Haskell parses the
-- modules into declarations; Jev picks where to start; the program reads
-- that declaration, asks one packet about it, follows the reference Jev
-- chooses, and stops when a declaration answers directly or when the
-- question needs judgment the source cannot supply. Two starting points
-- stay alive when the first packet is torn between them, and a closing
-- packet judges between their witnesses. The model context never holds
-- the file; the answer is a line, with the evidence that led there.
--
--   scripts/navigate.sh "where is a premise rendered onto the wire?"
module Main (main) where

import Control.Monad (forM, forM_)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isSpace)
import Data.IORef
import Data.List (nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Jev.Operators
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.IO (hClose, hPutStrLn, stderr)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)

-- ---------------------------------------------------------------------------
-- Deterministic side: the source, parsed into declarations
-- ---------------------------------------------------------------------------

data Decl = Decl
  { declModule :: FilePath
  , declName :: Text
  , declStart :: Int          -- 1-based line of the first line
  , declLines :: [Text]
  , declSection :: Text       -- the nearest section banner above it
  , declDoc :: [Text]         -- the comment block right above it
  } deriving (Show, Eq)

declKey :: Decl -> Text
declKey d = T.pack (takeFileName d.declModule) <> "/" <> d.declName

data Module = Module { modulePath :: FilePath, moduleDoc :: [Text], moduleSections :: [(Text, [Decl])] }

moduleName :: Module -> Text
moduleName = T.pack . takeFileName . modulePath

-- Top-level declarations: runs of lines starting at a column-0 line, merged
-- while the leading token stays the same (signature plus equations). A
-- comment block belongs to the declaration below it; a banner names the
-- section every declaration below it lives in.
outline :: FilePath -> IO Module
outline path = do
  ls <- T.lines <$> TIO.readFile path
  let indexed = zip [1 :: Int ..] ls
      starts = [ (n, nameOf l) | (n, l) <- indexed, isStart l ]
      merged = foldr merge [] starts
      merge (n, name) acc = case acc of
        (_, name') : rest | name == name' -> (n, name) : rest
        _ -> (n, name) : acc
      bounds = zip merged (map fst (drop 1 merged) ++ [length ls + 1])
      bannerAt n = case [ t | (m, t) <- banners, m < n ] of
        [] -> "top"
        ts -> last ts
      banners = [ (n, T.strip (T.drop 2 l)) | (n, l) <- indexed, "-- " `T.isPrefixOf` l, n > 1
                , Just prev <- [lookup (n - 1) indexed], "-- ---" `T.isPrefixOf` prev ]
      docAbove n = reverse (takeWhile (\l -> "--" `T.isPrefixOf` l) (reverse [ l | (m, l) <- indexed, m < n, m >= n - 12 ]))
      body n end = takeWhile (not . isTrailingComment) (map snd (takeWhile ((< end) . fst) (dropWhile ((< n) . fst) indexed)))
      isTrailingComment l = "-- " `T.isPrefixOf` l || "-- -" `T.isPrefixOf` l
      decls = [ Decl path name n (trimBlank (body n end)) (bannerAt n) (docAbove n)
              | ((n, name), end) <- bounds, name `notElem` ["import", "module", "infix", "infixl", "infixr"] ]
      header = takeWhile ("--" `T.isPrefixOf`) (dropWhile (not . ("-- |" `T.isPrefixOf`)) ls)
      sections = [ (sec, [ d | d <- decls, d.declSection == sec ]) | sec <- nub (map declSection decls) ]
  pure (Module path header sections)
  where
    trimBlank = reverse . dropWhile T.null . reverse
    isStart l = not (T.null l) && not (isSpace (T.head l)) && not ("--" `T.isPrefixOf` l) && not ("{-" `T.isPrefixOf` l) && not ("#" `T.isPrefixOf` l)
    nameOf l = case T.words (stripKinds l) of
      "instance" : rest -> T.unwords ("instance" : take 4 (takeWhile (/= "where") (afterContext rest)))
      w : rest | w `elem` ["data", "newtype", "type", "class"] -> T.unwords (w : takeWhile (`notElem` ["where", "=", "::"]) (take 4 rest))
      w : _ -> w
      [] -> ""
    afterContext ws = if "=>" `elem` ws then drop 1 (dropWhile (/= "=>") ws) else ws
    -- (k :: Symbol) becomes k, so two declarations never share a name by accident
    stripKinds t = case T.breakOn "(" t of
      (before, rest) | T.null rest -> before
                     | otherwise ->
          let (inside, after) = T.breakOn ")" (T.drop 1 rest)
              bare = fst (T.breakOn " :: " inside)
          in before <> (if " :: " `T.isInfixOf` inside then bare else "(" <> inside <> ")") <> stripKinds (T.drop 1 after)

allDecls :: [Module] -> [Decl]
allDecls = concatMap (concatMap snd . moduleSections)

-- What Jev sees of the whole codebase on every call: modules, their
-- purpose, their sections, and the names under each.
codebaseMap :: [Module] -> Value
codebaseMap mods = object
  [ Key.fromText (moduleName m) .= object
      [ "purpose" .= T.unwords (map (T.strip . T.dropWhile (== '|') . T.drop 2) (moduleDoc m))
      , "sections" .= object [ Key.fromText sec .= [ d.declName | d <- ds ] | (sec, ds) <- moduleSections m ]
      ]
  | m <- mods ]

-- The symbols a declaration defines: the function or type it names, its
-- constructors, its record fields, its class methods. Type variables are
-- never symbols.
symbols :: Decl -> [Text]
symbols d = nub (filter (not . T.null) (own ++ inner))
  where
    firstLine = T.strip (T.unwords (take 1 d.declLines))
    ws = T.words firstLine
    own = case ws of
      "instance" : _ -> []
      w : rest | w `elem` ["data", "newtype", "type", "class"] -> take 1 (dropWhile (`elem` ["family", "instance"]) rest)
      w : _ -> [bare w]
      [] -> []
    -- constructors, fields, methods: identifiers that begin a line's payload
    inner = concat
      [ case T.words (T.dropWhile (`elem` ("=|{}," :: String)) (T.strip l)) of
          w : "::" : _ -> [bare w]                         -- field, method, or GADT constructor
          w : _ | isCon w && startsPayload l -> [bare w]   -- constructor after = or |
          _ -> []
      | l <- d.declLines ]
    startsPayload l = any (`T.isPrefixOf` T.strip l) ["=", "|"] || "= " `T.isInfixOf` l && isDataLike
    isDataLike = take 1 ws `elem` [["data"], ["newtype"]]
    isCon w = not (T.null w) && T.head w `elem` ['A' .. 'Z']
    bare w = T.filter (`notElem` ("()" :: String)) (fst (T.breakOn " " w))

data Relation = Uses | UsedBy | Both deriving (Eq, Show)

-- Declarations this one uses (their symbols appear in its body) and the
-- declarations that use it; both directions, because an inquiry about a
-- constructor is often answered where it is matched.
neighbours :: [Decl] -> Decl -> [(Relation, Decl)]
neighbours everything d = [ (rel, e) | e <- everything, e /= d, Just rel <- [relation e] ]
  where
    mine = symbols d
    tokensOf e = nub (concatMap (T.split (not . isIdent)) e.declLines)
    isIdent c = isAlphaNum c || c `elem` ("_'" :: String)
    myTokens = tokensOf d
    relation e =
      let uses = any (`elem` myTokens) (symbols e)
          usedBy = any (`elem` tokensOf e) mine
      in case (uses, usedBy) of
           (True, True) -> Just Both
           (True, False) -> Just Uses
           (False, True) -> Just UsedBy
           (False, False) -> Nothing

numbered :: Decl -> [(Int, Text)]
numbered d = zip [d.declStart ..] d.declLines

-- ---------------------------------------------------------------------------
-- Judgment side: the packets
-- ---------------------------------------------------------------------------

data Outcome
  = Witness Decl Int Text [Step]        -- module, line, its text, the trail
  | NeedsJudgment [Step]                -- Jev said the source cannot settle it
  | Exhausted [Step]

-- Every declaration read, with how directly it answered: the fallback when
-- no single declaration clears the bar.
data Seen = Seen Decl Int Text Double [Step]

seenDirect :: Seen -> Double
seenDirect (Seen _ _ _ x _) = x

data Step = Step { stepDecl :: Decl, stepWhy :: Text }

-- Packet 0: which module, from the codebase map. Cheap.
pickModule transport inquiry mods = do
  roundTrip transport jevLatest
    (state (object [ "inquiry" .= inquiry, "directory" .= [ modulePath m | m <- mods ], "codebase" .= codebaseMap mods ]))
    (  #which := choice "Which module holds the code that does what the inquiry asks about?"
                    (alt #none "None of these modules" () .| many [ (moduleName m, object ["purpose" .= take 1 (moduleDoc m)], m) | m <- mods ])
    :& #each := each [ (moduleName m, #holds := noul ("Does " <> moduleName m <> " contain the code the inquiry asks about?") :& Nil) | m <- mods ]
    :& Nil )

-- Packet 1: which declaration, with the whole module's source in the
-- state so the judgment is over bodies, not names.
pickDecl transport inquiry mods m = do
  src <- TIO.readFile (modulePath m)
  let here = concatMap snd (moduleSections m)
      entry d = (declKey d, object ["lines" .= (T.pack (show d.declStart) <> "-" <> T.pack (show (d.declStart + length d.declLines - 1)))
                                  , "section" .= d.declSection, "head" .= T.strip (T.unwords (take 1 d.declLines))], d)
  jev1 transport jevLatest
    (state (object [ "inquiry" .= inquiry, "module" .= moduleName m, "codebase" .= codebaseMap mods
                   , "source" .= [ T.pack (show n) <> "| " <> l | (n, l) <- zip [1 :: Int ..] (T.lines src) ] ]))
    (choice "Which declaration contains the code that does what the inquiry asks about? Judge by the source, not the name."
       (alt #none "The inquiry is not answered in this module" () .| many (map entry here)))

-- One hop: read a declaration, judge it, and choose what to read next.
-- Jev sees the declaration with its comment and section, every neighbour
-- with its relation, its own comment, and the lines here that mention it,
-- and what the earlier hops concluded.
hop transport inquiry mods trail d = do
  let everything = allDecls mods
      lines' = numbered d
      mentions e = [ n | (n, l) <- lines', any (\sym -> sym `elem` T.split (not . isIdent) l) (symbols e) ]
      isIdent c = isAlphaNum c || c `elem` ("_'" :: String)
      neighbourWording rel e = object
        [ "relation" .= relText rel
        , "section" .= (T.pack (takeFileName e.declModule) <> " / " <> e.declSection)
        , "head" .= T.strip (T.unwords (take 1 e.declLines))
        , "comment" .= T.unwords (map (T.strip . T.dropWhile (== '|') . T.drop 2) e.declDoc)
        , "mentioned_at_lines" .= mentions e
        ]
      refs = pool #refs [ (declKey e, neighbourWording rel e, e) | (rel, e) <- take 24 (neighbours everything d) ]
      relText r = case r of { Uses -> "this declaration uses it" :: Text; UsedBy -> "it uses this declaration"; Both -> "each uses the other" }
  roundTrip transport jevLatest
    (state (object
      [ "inquiry" .= inquiry
      , "reading" .= object ["module" .= takeFileName d.declModule, "section" .= d.declSection, "declaration" .= d.declName, "comment" .= d.declDoc]
      , "source" .= [ T.pack (show n) <> "| " <> l | (n, l) <- lines' ]
      , "so_far" .= [ object ["declaration" .= declKey s.stepDecl, "found" .= s.stepWhy] | s <- trail ]
      , "codebase" .= codebaseMap mods
      ]))
    (  #refs := refs
    :& #answers := score "Do the lines shown contain the code that does what the inquiry asks about?"
                     (  level #not_here "No; this declaration does not concern the inquiry"
                     .| level #related "No; it is involved, but the code that does it is in a neighbouring declaration"
                     .| level #partly "Partly; some of what the inquiry asks about is done here"
                     .| level #directly "Yes; the code that does it is in these lines" )
    :& #line := choice "Which line most precisely answers the inquiry?"
                  (alt #no_single_line "No single line; the answer is the declaration as a whole" () .| many [ (T.pack (show n), String l, n) | (n, l) <- lines', not (T.null (T.strip l)) ])
    :& #next := choice "What should be read next?"
                  (  alt #stop_here "This declaration settles the inquiry; nothing more to read" ()
                  .| alt #ask_model "Settling the inquiry needs judgment or context the source does not supply" ()
                  .| manyFrom refs )
    :& #bears := eachIn refs (\r -> #on_inquiry := askAbout r "Does this neighbouring declaration bear on the inquiry?" :& Nil)
    :& #if_elsewhere := given "the behavior the inquiry asks about is implemented in a neighbouring declaration, one this uses or one that uses it"
                          (choice "Which neighbouring declaration implements it?" (manyFrom refs .| alt #unclear "Cannot tell from this declaration" ()))
    :& Nil )

-- Closing packet: two witnesses, one answer.
close transport inquiry witnesses =
  jev1 transport jevLatest (state (object ["inquiry" .= inquiry]))
    (choice "Which of these spans answers the inquiry?"
       (alt #both "Both are needed to answer it" () .| many [ (declKey d <> ":" <> T.pack (show n), String (T.strip l), w) | w@(d, n, l) <- witnesses ]))

-- ---------------------------------------------------------------------------
-- The investigation
-- ---------------------------------------------------------------------------

policy :: Policy
policy = Policy { minMass = 0.35, minMargin = 0.1, minConfidence = 0.25 }

investigate transport tokens inquiry mods maxHops = do
  seen <- newIORef []
  r0 <- pickModule transport inquiry mods
  case r0 of
    Left e -> pure (Left e)
    Right resp -> do
      count tokens resp
      let a = answers resp
          modules = take 2 [ m | (_, s) <- contenders 0.15 a.which, Just m <- [handle s (#none (\() -> Nothing) .| onMany (\_ m -> Just m))] ]
      say ("module: " <> T.intercalate ", " [ k <> " " <> pct (yes sub.holds) | (k, sub) <- a.each ] <> "; reading " <> T.intercalate " and " (map moduleName modules))
      if null modules then pure (Right (NeedsJudgment [])) else do
        picks <- forM modules (pickDecl transport inquiry mods)
        case sequence picks of
          Left e -> pure (Left e)
          Right pickeds -> do
            let starts = take 2 (sortOn (negate . fst) [ (m, d) | picked <- pickeds, (m, s) <- contenders 0.15 picked, Just d <- [handle s (#none (\() -> Nothing) .| onMany (\_ d -> Just d))] ])
            say ("start: " <> T.intercalate ", " [ declKey d <> " " <> pct m | (m, d) <- starts ])
            if null starts then pure (Right (NeedsJudgment [])) else do
              outcomes <- forM starts $ \(m, d) -> walk seen [] [Step d ("starting point, " <> pct m)] d maxHops
              let witnesses = [ w | Right w@(Witness {}) <- outcomes ]
                  judgments = [ o | Right o@(NeedsJudgment _) <- outcomes ]
                  exhausted = sortOn (\o -> case o of Exhausted t -> negate (length t); _ -> 0) [ o | Right o@(Exhausted _) <- outcomes ]
              case (witnesses, [ e | Left e <- outcomes ]) of
                (_, e : _) -> pure (Left e)
                ([w], _) -> pure (Right w)
                ([Witness d1 n1 l1 t1, Witness d2 n2 l2 t2], _) | (d1, n1) /= (d2, n2) -> do
                  say "two witnesses; asking which answers"
                  judge [(d1, n1, l1, t1), (d2, n2, l2, t2)]
                (w : _, _) -> pure (Right w)
                ([], _) | j : _ <- judgments, null exhausted -> pure (Right j)
                ([], _) -> do
                  -- nothing cleared the bar: let Jev judge between the two best partial answers
                  best <- take 2 . sortOn (negate . seenDirect) <$> readIORef seen
                  case best of
                    [] -> pure (Right (Exhausted []))
                    [Seen d n l _ t] -> pure (Right (Witness d n l t))
                    Seen d1 n1 l1 x1 t1 : Seen d2 n2 l2 x2 t2 : _ -> do
                      say ("no declaration answered directly; asking which of the two best partial answers (" <> pct x1 <> ", " <> pct x2 <> ") answers")
                      judge [(d1, n1, l1, t1), (d2, n2, l2, t2)]
  where
    judge [(d1, n1, l1, t1), (d2, n2, l2, t2)] = do
      c <- close transport inquiry [(d1, n1, l1), (d2, n2, l2)]
      pure $ fmap (\ans -> handle (chosen ans)
        (  #both (\() -> Witness d1 n1 l1 (t1 ++ t2))
        .| onMany (\_ (d, n, l) -> Witness d n l (if d == d1 then t1 else t2)) )) c
    judge _ = pure (Right (Exhausted []))
    walk seen visited trail d hops
      | hops <= (0 :: Int) = pure (Right (Exhausted trail))
      | otherwise = do
          r <- hop transport inquiry mods trail d
          case r of
            Left e -> pure (Left e)
            Right resp -> do
              count tokens resp
              let a = answers resp
                  direct = massAtOrAbove #directly a.answers
                  lineOf = handle (chosen a.line) (#no_single_line (\() -> Nothing) .| onMany (\_ n -> Just n))
                  bearing = [ k | (k, sub) <- a.bears, yes sub.on_inquiry > 0.5 ]
              say ("read " <> declKey d <> ": " <> levelOf a.answers <> " (directly " <> pct direct <> "), line "
                <> maybe "-" (T.pack . show) lineOf <> ", next " <> selectedKey (chosen a.next) <> " " <> pct (confidence a.next)
                <> (if null bearing then "" else ", bearing: " <> T.intercalate ", " (take 3 bearing) <> (if length bearing > 3 then ", …" else "")))
              let lineNo = maybe d.declStart id lineOf
                  lineText = maybe (T.strip (T.unwords (take 1 d.declLines))) (\n -> T.strip (d.declLines !! (n - d.declStart))) lineOf
                  witness = Witness d lineNo lineText trail
                  premised = handle (chosen a.if_elsewhere) (onMany (\_ e -> Just e) .| #unclear (\() -> Nothing))
                  -- the best neighbour by the evidence already in hand: the per-neighbour
                  -- Nouls first, the next-choice masses to break ties
                  ranked = [ (yes sub.on_inquiry + m, e) | (m, s) <- contenders 0 a.next, Just e <- [handle s (#stop_here (\() -> Nothing) .| #ask_model (\() -> Nothing) .| onMany (\_ e -> Just e))], Just sub <- [lookup (declKey e) a.bears] ]
                  bestRef = case sortOn (negate . fst) ranked of
                    (_, e) : _ -> Just e
                    [] -> Nothing
                  followed = case premised of
                    Just e -> Just e
                    Nothing -> bestRef
                  neighbourByKey k = case [ e | (_, s) <- contenders 0 a.next, Just e <- [handle s (#stop_here (\() -> Nothing) .| #ask_model (\() -> Nothing) .| onMany (\key e -> if key == k then Just e else Nothing))] ] of
                    e : _ -> Just e
                    [] -> Nothing
                  better x y = case (x, y) of
                    (Right w@(Witness {}), _) -> Right w
                    (_, Right w@(Witness {})) -> Right w
                    (Right j@(NeedsJudgment _), _) -> Right j
                    _ -> x
              let finding = levelOf a.answers <> " (" <> pct direct <> ")" <> maybe "" (\n -> ", line " <> T.pack (show n)) lineOf
              modifyIORef' seen (Seen d lineNo lineText direct trail :)
              if direct >= 0.6 then pure (Right witness) else
                case accept policy a.next of
                  Left doubt@(NearTie (k1, _) (k2, _)) | Just e1 <- neighbourByKey k1, Just e2 <- neighbourByKey k2 -> do
                    -- two neighbours nearly tied: read both, keep the better outcome
                    say ("  doubt: " <> T.pack (show doubt) <> "; reading both")
                    o1 <- follow finding (Just e1)
                    o2 <- follow finding (Just e2)
                    pure (better o1 o2)
                  Left doubt -> do
                    say ("  doubt: " <> T.pack (show doubt) <> "; the premised choice says " <> maybe "unclear" declKey premised
                      <> (case (premised, bestRef) of (Nothing, Just e) -> ", so following the best reference " <> declKey e; _ -> ""))
                    follow finding followed
                  Right s -> handle s
                    (  #stop_here (\() -> pure (Right witness))
                    .| #ask_model (\() -> pure (Right (NeedsJudgment trail)))
                    .| onMany (\_ e -> follow finding (Just e)) )
      where
        follow _ Nothing = pure (Right (Exhausted trail))
        follow finding (Just e)
          | e `elem` visited = pure (Right (Exhausted trail))
          | otherwise = walk seen (d : visited) (trail ++ [Step e ("neighbour of " <> declKey d <> ", which " <> finding)]) e (hops - 1)

-- ---------------------------------------------------------------------------
-- Plumbing
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  files <- filter ((== ".hs") . takeExtension) <$> listDirectory "src/Jev/Core"
  mods <- forM (sortOn id files) (\f -> outline ("src/Jev/Core" </> f))
  let decls = allDecls mods
  inquiry <- case args of
    [q] -> pure (T.pack q)
    ["--graph", name] -> do
      -- the deterministic side alone: what a declaration defines and touches
      forM_ [ d | d <- decls, T.pack name `T.isInfixOf` declKey d ] $ \d -> do
        TIO.putStrLn (declKey d <> "  defines " <> T.intercalate ", " (symbols d))
        forM_ (neighbours decls d) $ \(rel, e) -> TIO.putStrLn ("  " <> T.pack (show rel) <> " " <> declKey e)
      exitFailure
    _ -> hPutStrLn stderr "usage: jev-dsl-navigate \"question about this library's source\"" >> exitFailure
  say ("outline: " <> T.pack (show (length decls)) <> " declarations in " <> T.pack (show (length files)) <> " modules, " <> T.pack (show (sum [ length (moduleSections m) | m <- mods ])) <> " sections")
  tokens <- newIORef (0 :: Int, 0 :: Int)
  result <- investigate curl tokens inquiry mods 5
  (i, o) <- readIORef tokens
  case result of
    Left e -> hPutStrLn stderr ("jev: " ++ show e) >> exitFailure
    Right (Witness d n l trail) -> do
      TIO.putStrLn ("\n" <> T.pack d.declModule <> ":" <> T.pack (show n) <> "  " <> l)
      TIO.putStrLn ("in " <> d.declName <> ", via " <> T.intercalate " -> " [ declKey s.stepDecl | s <- trail ])
    Right (NeedsJudgment trail) -> TIO.putStrLn ("\nneeds judgment beyond the source; read so far: " <> T.intercalate " -> " [ declKey s.stepDecl | s <- trail ])
    Right (Exhausted trail) -> TIO.putStrLn ("\nran out of hops; read: " <> T.intercalate " -> " [ declKey s.stepDecl | s <- trail ])
  TIO.putStrLn ("tokens: " <> T.pack (show i) <> " in, " <> T.pack (show o) <> " out; no frontier model consulted")

say :: Text -> IO ()
say = TIO.hPutStrLn stderr

pct :: Double -> Text
pct x = T.pack (show (fromIntegral (round (x * 100) :: Int) / 100 :: Double))

count :: IORef (Int, Int) -> Response s -> IO ()
count ref resp = modifyIORef' ref (\(i, o) -> (i + n "input_tokens", o + n "output_tokens"))
  where n k = case usage resp of
          Object m | Just (Number x) <- KeyMap.lookup (Key.fromText k) m -> round x
          _ -> 0

-- The transport: scripts/transport.sh, which holds the key and calls curl.
curl :: Value -> IO (Either Text Value)
curl body = do
  (Just hin, Just hout, _, ph) <- createProcess (proc "scripts/transport.sh" []) { std_in = CreatePipe, std_out = CreatePipe }
  BL.hPut hin (Aeson.encode body) >> hClose hin
  out <- BL.hGetContents hout
  _ <- waitForProcess ph
  pure (either (Left . T.pack) Right (Aeson.eitherDecode out))
