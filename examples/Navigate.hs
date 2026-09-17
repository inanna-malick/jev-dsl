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

import Control.Monad (forM)
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
  } deriving (Show, Eq)

declKey :: Decl -> Text
declKey d = T.pack (takeFileName d.declModule) <> "/" <> d.declName

-- Top-level declarations: runs of lines starting at a column-0 line, merged
-- while the leading token stays the same (signature plus equations).
outline :: FilePath -> IO [Decl]
outline path = do
  ls <- T.lines <$> TIO.readFile path
  let indexed = zip [1 :: Int ..] ls
      starts = [ (n, nameOf l) | (n, l) <- indexed, isStart l ]
      merged = foldr merge [] starts
      merge (n, name) acc = case acc of
        (_, name') : rest | name == name' -> (n, name) : rest
        _ -> (n, name) : acc
      bounds = zip merged (map fst (drop 1 merged) ++ [length ls + 1])
  pure [ Decl path name n (map snd (takeWhile ((< end) . fst) (dropWhile ((< n) . fst) indexed)))
       | ((n, name), end) <- bounds, name `notElem` ["import", "module", "infix", "infixl", "infixr"] ]
  where
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

-- Names this declaration mentions that are declarations elsewhere.
references :: [Decl] -> Decl -> [Decl]
references everything d = nub [ e | e <- everything, e /= d, simpleName e `elem` tokens, simpleName e `notElem` ["", "where", "case", "of"] ]
  where
    tokens = nub (concatMap (T.split (not . isIdent)) d.declLines)
    isIdent c = isAlphaNum c || c `elem` ("_'" :: String)
    simpleName e = last (T.words e.declName)

numbered :: Decl -> [(Int, Text)]
numbered d = zip [d.declStart ..] d.declLines

-- ---------------------------------------------------------------------------
-- Judgment side: the packets
-- ---------------------------------------------------------------------------

data Outcome
  = Witness Decl Int Text [Step]        -- module, line, its text, the trail
  | NeedsJudgment [Step]                -- Jev said the source cannot settle it
  | Exhausted [Step]

data Step = Step { stepDecl :: Decl, stepWhy :: Text }

-- Packet 0: which module, from the names it declares. Cheap.
pickModule transport inquiry decls = do
  let byModule = nub (map declModule decls)
      names m = [ d.declName | d <- decls, d.declModule == m ]
  roundTrip transport jevLatest
    (state (object [ "inquiry" .= inquiry, "modules" .= object [ Key.fromString (takeFileName m) .= names m | m <- byModule ] ]))
    (  #which := choice "Which module holds what the inquiry asks about?"
                    (alt #none "None of these modules" () .| many [ (T.pack (takeFileName m), String (T.pack (show (length (names m))) <> " declarations"), m) | m <- byModule ])
    :& #each := each [ (T.pack (takeFileName m), #holds := noul ("Does " <> T.pack (takeFileName m) <> " contain the code the inquiry asks about?") :& Nil) | m <- byModule ]
    :& Nil )

-- Packet 1: which declaration, with the whole module's source in the
-- state so the judgment is over bodies, not names.
pickDecl transport inquiry decls m = do
  src <- TIO.readFile m
  let here = [ d | d <- decls, d.declModule == m ]
      entry d = (declKey d, object ["lines" .= (T.pack (show d.declStart) <> "-" <> T.pack (show (d.declStart + length d.declLines - 1)))
                                  , "head" .= T.strip (T.unwords (take 1 d.declLines))], d)
  jev1 transport jevLatest
    (state (object [ "inquiry" .= inquiry, "module" .= takeFileName m, "source" .= [ T.pack (show n) <> "| " <> l | (n, l) <- zip [1 :: Int ..] (T.lines src) ] ]))
    (choice "Which declaration contains the code that does what the inquiry asks about? Judge by the source, not the name."
       (alt #none "The inquiry is not answered in this module" () .| many (map entry here)))

-- One hop: read a declaration, judge it, and choose what to read next.
hop transport inquiry everything trail d = do
  let refs = pool #refs [ (declKey e, String (T.strip (T.unwords (take 1 e.declLines))), e) | e <- references everything d ]
      lines' = numbered d
  roundTrip transport jevLatest
    (state (object
      [ "inquiry" .= inquiry
      , "reading" .= object ["module" .= takeFileName d.declModule, "declaration" .= d.declName]
      , "source" .= [ T.pack (show n) <> "| " <> l | (n, l) <- lines' ]
      , "trail" .= [ object ["declaration" .= declKey s.stepDecl, "why" .= s.stepWhy] | s <- trail ]
      ]))
    (  #refs := refs
    :& #answers := score "Do the lines shown contain the code that does what the inquiry asks about?"
                     (  level #not_here "No; this declaration does not concern the inquiry"
                     .| level #related "No; it is involved, but the code that does it is in something it references"
                     .| level #partly "Partly; some of what the inquiry asks about is done here"
                     .| level #directly "Yes; the code that does it is in these lines" )
    :& #line := choice "Which line most precisely answers the inquiry?"
                  (alt #no_single_line "No single line; the answer is the declaration as a whole" () .| many [ (T.pack (show n), String l, n) | (n, l) <- lines', not (T.null (T.strip l)) ])
    :& #next := choice "What should be read next?"
                  (  alt #stop_here "This declaration settles the inquiry; nothing more to read" ()
                  .| alt #ask_model "Settling the inquiry needs judgment or context the source does not supply" ()
                  .| manyFrom refs )
    :& #bears := eachIn refs (\r -> #on_inquiry := askAbout r "Does this referenced declaration bear on the inquiry?" :& Nil)
    :& #if_elsewhere := given "the behavior the inquiry asks about is implemented in a referenced declaration"
                          (choice "Which referenced declaration implements it?" (manyFrom refs .| alt #unclear "Cannot tell from this declaration" ()))
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

investigate transport tokens inquiry decls maxHops = do
  r0 <- pickModule transport inquiry decls
  case r0 of
    Left e -> pure (Left e)
    Right resp -> do
      count tokens resp
      let a = answers resp
          modules = take 2 [ m | (_, s) <- contenders 0.3 a.which, Just m <- [handle s (#none (\() -> Nothing) .| onMany (\_ m -> Just m))] ]
      say ("module: " <> T.intercalate ", " [ k <> " " <> pct (yes sub.holds) | (k, sub) <- a.each ] <> "; reading " <> T.intercalate " and " (map (T.pack . takeFileName) modules))
      if null modules then pure (Right (NeedsJudgment [])) else do
        picks <- forM modules (pickDecl transport inquiry decls)
        case sequence picks of
          Left e -> pure (Left e)
          Right pickeds -> do
            let starts = take 2 (sortOn (negate . fst) [ (m, d) | picked <- pickeds, (m, s) <- contenders 0.15 picked, Just d <- [handle s (#none (\() -> Nothing) .| onMany (\_ d -> Just d))] ])
            say ("start: " <> T.intercalate ", " [ declKey d <> " " <> pct m | (m, d) <- starts ])
            if null starts then pure (Right (NeedsJudgment [])) else do
              outcomes <- forM starts $ \(m, d) -> walk [] [Step d ("starting point, " <> pct m)] d maxHops
              case outcomes of
                [Right o] -> pure (Right o)
                [Right (Witness d1 n1 l1 t1), Right (Witness d2 n2 l2 t2)] | (d1, n1) /= (d2, n2) -> do
                  say "two witnesses; asking which answers"
                  c <- close transport inquiry [(d1, n1, l1), (d2, n2, l2)]
                  pure $ fmap (\ans -> handle (chosen ans)
                    (  #both (\() -> Witness d1 n1 l1 (t1 ++ t2))
                    .| onMany (\_ (d, n, l) -> Witness d n l (if d == d1 then t1 else t2)) )) c
                o : _ -> pure o
                [] -> pure (Right (Exhausted []))
  where
    walk visited trail d hops
      | hops <= (0 :: Int) = pure (Right (Exhausted trail))
      | otherwise = do
          r <- hop transport inquiry decls trail d
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
              let witness = Witness d (maybe d.declStart id lineOf) (maybe (T.strip (T.unwords (take 1 d.declLines))) (\n -> T.strip (d.declLines !! (n - d.declStart))) lineOf) trail
                  followed = handle (chosen a.if_elsewhere) (onMany (\_ e -> Just e) .| #unclear (\() -> Nothing))
              if direct >= 0.6 then pure (Right witness) else
                case accept policy a.next of
                  Left doubt -> do
                    say ("  doubt: " <> T.pack (show doubt) <> "; the premised choice says " <> maybe "unclear" declKey followed)
                    follow followed
                  Right s -> handle s
                    (  #stop_here (\() -> pure (Right witness))
                    .| #ask_model (\() -> pure (Right (NeedsJudgment trail)))
                    .| onMany (\_ e -> follow (Just e)) )
      where
        follow Nothing = pure (Right (Exhausted trail))
        follow (Just e)
          | e `elem` visited = pure (Right (Exhausted trail))
          | otherwise = walk (d : visited) (trail ++ [Step e ("referenced from " <> declKey d)]) e (hops - 1)

-- ---------------------------------------------------------------------------
-- Plumbing
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  inquiry <- case args of
    [q] -> pure (T.pack q)
    _ -> hPutStrLn stderr "usage: jev-dsl-navigate \"question about this library's source\"" >> exitFailure
  files <- filter ((== ".hs") . takeExtension) <$> listDirectory "src/Jev/Core"
  decls <- concat <$> forM (sortOn id files) (\f -> outline ("src/Jev/Core" </> f))
  say ("outline: " <> T.pack (show (length decls)) <> " declarations in " <> T.pack (show (length files)) <> " modules")
  tokens <- newIORef (0 :: Int, 0 :: Int)
  result <- investigate curl tokens inquiry decls 5
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
