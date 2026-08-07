module Main where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (filterM, unless)
import Data.Function (on)
import Data.List (groupBy, intercalate, isInfixOf, sort, stripPrefix)
import Data.Maybe (mapMaybe)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
  ((</>), dropExtension, makeRelative, takeDirectory, takeExtension)

import Environment (initialEnv)
import Runtime (compile)
import Semantics (eval)

casesDir, examplesDir :: FilePath
casesDir    = "test/cases"
examplesDir = "examples"

-- A test file declares one or more directives in its comments:
--   -- @expect-type:  <type>      inferred type must match this string exactly
--   -- @expect-value: <value>     evaluation must produce this result
--   -- @expect-error: <substring> type checking must fail with this substring
-- Files under examplesDir need not declare any, but are forced to run regardless.
data Expectation
  = Type  String
  | Value String
  | Error String
  | Runs

data Outcome = Pass | Fail String

type Run = Either String (String, String)

run :: String -> Run
run src = do
  (ast, t) <- compile src
  return (show t, show (eval initialEnv ast))

findCases :: FilePath -> IO [FilePath]
findCases root = do
  entries <- sort . map (root </>) <$> listDirectory root
  dirs    <- filterM doesDirectoryExist entries
  nested  <- concat <$> mapM findCases dirs
  return $ filter ((== ".cba") . takeExtension) entries ++ nested

directives :: [(String, String -> Expectation)]
directives =
  [ ("-- @expect-type: ",  Type)
  , ("-- @expect-value: ", Value)
  , ("-- @expect-error: ", Error)
  ]

parseDirectives :: String -> [Expectation]
parseDirectives src = do
  line           <- lines src
  (prefix, ctor) <- directives
  Just val       <- [stripPrefix prefix line]
  return $ ctor val

check :: Run -> Expectation -> Maybe String
check (Right (t', _)) (Type t)
  | t == t'                     = Nothing
  | otherwise                   = Just $ mismatch "Expected type" t "Actual" t'
check (Left err)      (Type t)  = Just $ mismatch "Expected type" t "Got error" err
check (Left err)      (Error s)
  | s `isInfixOf` err           = Nothing
  | otherwise                   = Just $ mismatch "Expected error containing" s "Actual error" err
check (Right (t, _))  (Error s) = Just $ mismatch "Expected error containing" s "Got type" t
check (Right (_, v')) (Value v)
  | v == v'                     = Nothing
  | otherwise                   = Just $ mismatch "Expected value" v "Actual" v'
check (Left err)      (Value v) = Just $ mismatch "Expected value" v "Got error" err
check (Right (_, v))  Runs      = length v `seq` Nothing
check (Left err)      Runs      = Just $ "Expected the program to run\n  Got error: " ++ err

mismatch :: String -> String -> String -> String -> String
mismatch lLabel l rLabel r = lLabel ++ ": " ++ l ++ "\n  " ++ rLabel ++ ": " ++ r

testName, testGroup :: FilePath -> String
testName  = dropExtension . makeRelative casesDir
testGroup = takeDirectory . makeRelative casesDir

printOutcome :: FilePath -> Outcome -> String
printOutcome p Pass       = "PASS: " ++ testName p
printOutcome p (Fail msg) = "FAIL: " ++ testName p ++ "\n  " ++ msg

printGroup :: [(FilePath, Outcome)] -> IO ()
printGroup []                = return ()
printGroup grp@((p, _) : _)  = do
  putStrLn $ "── " ++ testGroup p ++ " ──"
  mapM_ (putStrLn . uncurry printOutcome) grp
  putStrLn ""

judge :: [Expectation] -> String -> Outcome
judge forced src = case parseDirectives src <> forced of
  []  -> Fail "no expectation directives found"
  exs -> case mapMaybe (check (run src)) exs of
    []    -> Pass
    fails -> Fail (intercalate "\n  " fails)

judgeIO :: [Expectation] -> FilePath -> IO (FilePath, Outcome)
judgeIO forced path = do
  src <- readFile path
  r   <- try (evaluate (judge forced src))
  return (path, either (\e -> Fail (show (e :: SomeException))) id r)

main :: IO ()
main = do
  mapM_ (\d -> do
    hasDir <- doesDirectoryExist d
    unless hasDir $ do
      putStrLn $ "ERROR: " ++ d ++ " not found."
      exitFailure) [casesDir, examplesDir]
  cases    <- findCases casesDir
  examples <- findCases examplesDir
  results  <- (++) <$> mapM (judgeIO []) cases
                   <*> mapM (judgeIO [Runs]) examples
  mapM_ printGroup (groupBy ((==) `on` testGroup . fst) results)
  let passed = length [() | (_, Pass) <- results]
      total  = length results
  putStrLn $ show passed ++ "/" ++ show total ++ " tests passed."
  if passed == total then exitSuccess else exitFailure
