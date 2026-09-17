-- | A minimal assertion helper: prints every check, counts failures.
module Check (Checks, newChecks, check, checkEq, finish) where

import Control.Monad (unless)
import Data.IORef
import System.Exit (exitFailure)

newtype Checks = Checks (IORef Int)

newChecks :: IO Checks
newChecks = Checks <$> newIORef 0

check :: Checks -> String -> Bool -> IO ()
check (Checks ref) name ok = do
  putStrLn ((if ok then "ok   " else "FAIL ") ++ name)
  unless ok (modifyIORef' ref (+ 1))

checkEq :: (Eq a, Show a) => Checks -> String -> a -> a -> IO ()
checkEq c name expected actual = do
  check c name (expected == actual)
  unless (expected == actual) $ do
    putStrLn ("     expected: " ++ show expected)
    putStrLn ("     actual:   " ++ show actual)

finish :: Checks -> IO ()
finish (Checks ref) = do
  n <- readIORef ref
  if n == 0 then putStrLn "all checks passed" else do
    putStrLn (show n ++ " checks failed")
    exitFailure
