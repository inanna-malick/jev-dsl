{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- The small authoring forms, exercised through the public facade.
module Ergonomics (ergonomicChecks) where

import Check
import Control.Monad (forM_)
import Data.Aeson (Value, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import Data.List (sort)
import Data.Text (Text)
import GHC.OverloadedLabels (fromLabel)
import Jev.Operators
import Proto (questionsOf, stub)

ergonomicChecks :: Checks -> IO ()
ergonomicChecks c = do
  let world = state (#source := ("evidence" :: Text)
                 :& #gate := (#posters := (["wanted"] :: [Text])
                           :& #watch := (#captain := ("Ada" :: Text))))
  checkEq c "field: top-level reference" "`source`" (field #source world)
  checkEq c "field: nested reference" "`gate.posters`" (field (#gate :/ #posters) world)
  checkEq c "field: three segments associate to the right" "`gate.watch.captain`"
    (field (#gate :/ #watch :/ #captain) world)
  let awkward = state (fromLabel @"gate.part" := (fromLabel @"post\\ers" := True))
  checkEq c "field: escaped labels stay distinct from path separators" "`gate\\.part.post\\\\ers`"
    (field (fromLabel @"gate.part" :/ fromLabel @"post\\ers") awkward)

  -- One static exit and runtime rows, with the same payload type. The
  -- exit's Nothing is explicit author intent, not an approval signal.
  let options = alt #none "No matching row" Nothing
             .| mapCarried Just (many #rows fst snd [("one", "first" :: Text), ("two", "second")])
      packet = #next := choice "Which row?" options
      response selected ps conf = object
        [ "model" .= ("test" :: Text)
        , "answers" .= object ["next" .= object
            [ "type" .= ("choice" :: Text), "choice" .= selected
            , "confidence" .= conf
            , "probabilities" .= object [Key.fromText k .= p | (k, p) <- ps] ] ] ]
      cases :: [(Text, [(Text, Double)], Double)]
      cases =
        [ ("one", [("one", 0.9), ("two", 0.05), ("none", 0.05)], 0.95)
        , ("none", [("one", 0.05), ("two", 0.05), ("none", 0.9)], 0.95)
        , ("one", [("one", 0.9), ("two", 0.05), ("none", 0.05)], 0.1)
        , ("one", [("one", 0.35), ("two", 0.34), ("none", 0.31)], 0.95)
        , ("one", [("one", 0.5), ("two", 0.48), ("none", 0.02)], 0.95) ]
  forM_ cases $ \(selected, ps, conf) -> case decode packet (response selected ps conf) of
    Left e -> check c ("takenUnder: decode: " ++ show e) False
    Right r -> do
      let comparePolicy policy = do
            let expected = settle policy r.next (#none id .| #rows (\_ row -> row))
                actual = takenUnder policy r.next
            checkEq c "takenUnder: same payload or doubt as settle" expected actual
            case actual of
              Left d -> checkEq c "takenUnder: doubt line equals explain" (explain policy r.next) d.why
              Right _ -> checkEq c "takenUnder: retains selected payload" (Right (Settled (taken r.next))) actual
      comparePolicy lenient
      comparePolicy careful
      comparePolicy strict
      checkEq c "takenUnder: selected row or authored exit"
        (if selected == "none" then Nothing else Just ("one", "first" :: Text)) (taken r.next)

  let leaf questionText = #base := noul "Base?" :& #extra := optional (noul <$> questionText)
      body answers' = object
        [ "model" .= ("test" :: Text)
        , "answers" .= object [Key.fromText k .= a | (k, a) <- answers'] ]
      yesAt p = object ["type" .= ("noul" :: Text), "noul" .= (p :: Double)]
      readLeaf q = fmap (\r -> fmap (.yes) r.extra) . decode (leaf q)
      present = Just "Extra?"
      baseAnswer = ("base", yesAt 0.8)
  forM_ [Nothing, present] $ \q -> do
    checkEq c "optional: direct wire path or no question"
      (Right (if q == Nothing then ["base"] else ["base", "extra"]))
      (fmap (sort . map fst . questionsOf) (request jevLatest world (leaf q)))
    result <- ask (session (stub "") jevLatest) world (leaf q)
    checkEq c "optional: inferred Maybe answer" (Right (0.8 <$ q))
      (fmap (\r -> fmap (.yes) r.extra) result)
    checkEq c "optional: answer preview retains absence as null"
      (Right (object ["base" .= object ["yes" .= (0.8 :: Double)]
                    , "extra" .= fmap (const (object ["yes" .= (0.8 :: Double)])) q]))
      (fmap (toJSON . answers) result)
  checkEq c "optional: present answer cannot be missing" (Left (Decode (MissingAnswer "extra")))
    (readLeaf present (body [baseAnswer]))
  checkEq c "optional: absent question rejects unexpected answer" (Left (Decode (UnexpectedAnswer "extra")))
    (readLeaf Nothing (body [baseAnswer, ("extra", yesAt 0.8)]))
  checkEq c "optional: present answer is validated" (Left (Decode (ValueOutOfRange "extra" "noul")))
    (readLeaf present (body [baseAnswer, ("extra", yesAt 1.1)]))
  checkEq c "optional: present answer must have the correct kind" (Left (Decode (WrongKind "extra")))
    (readLeaf present (body [baseAnswer, ("extra", object ["type" .= ("choice" :: Text)])]))
  checkEq c "optional: an entirely absent request remains an error" (Left (Prepare EmptyQuestionMap) :: Either JevError Value)
    (request jevLatest world (#extra := optional (noul <$> (Nothing :: Maybe Text))))

  -- Optional packets, choices and scores, and optional leaves inside each.
  let nested enabled =
           #base := noul "Base?"
        :& #extra := optional (if enabled then Just
             ( #route := choice "Where?" (alt #here "Here" (1 :: Int))
            :& #risk := score "Risk?" (level #low "Low" False .| level #high "High" True)
            :& #items := each fst (\(_, q) -> optional (noul <$> q))
                 [("present", Just "Ready?"), ("absent", Nothing)] ) else Nothing)
  forM_ [False, True] $ \enabled -> do
    checkEq c "optional packet: leaves flatten without synthetic segments"
      (Right (if enabled then ["base", "extra.items.present", "extra.risk", "extra.route"] else ["base"]))
      (fmap (sort . map fst . questionsOf) (request jevLatest world (nested enabled)))
    result <- ask (session (stub "here") jevLatest) world (nested enabled)
    checkEq c "optional packet: nested answers keep their operations and rows"
      (Right (if enabled then Just (1, True, [("present", Just 0.8), ("absent", Nothing)]) else Nothing))
      (fmap (\r -> fmap (\a -> (taken a.route, grade 0.5 a.risk,
        [(k, fmap (.yes) n) | ((k, _), n) <- a.items])) r.extra) result)
  checkEq c "optional packet: missing nested answer names its full path"
    (Left (Decode (MissingAnswer "extra.route")))
    (fmap (const ()) (decode (nested True) (body [baseAnswer])))
  checkEq c "optional packet: absent nested answer is unexpected"
    (Left (Decode (UnexpectedAnswer "extra.route")))
    (fmap (const ()) (decode (nested False) (body [baseAnswer, ("extra.route", yesAt 0.8)])))

  -- Presence belongs to the retained question, not to the wire key count.
  let emptyInside = #base := noul "Base?"
                 :& #extra := optional (Just (optional (noul <$> (Nothing :: Maybe Text))))
  result <- ask (session (stub "") jevLatest) world emptyInside
  checkEq c "optional: present wrapper around absent question stays Just Nothing"
    (Right (Just Nothing)) (fmap (\r -> fmap (fmap (.yes)) r.extra) result)
