module Main where

import Control.Monad (filterM, unless)
import Data.Function (on)
import Data.List (groupBy, intercalate, isInfixOf, sort, stripPrefix)
import Data.Maybe (mapMaybe)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
  ((</>), dropExtension, makeRelative, takeDirectory, takeExtension)
import Environment (initialEnv)
import Eval (eval)
import Parsing.Parser (parse)
import Parsing.Desugar (desugar)
import Inference.Infer (infer)

casesDir :: FilePath
casesDir = "test/cases"

-- A test file declares one or more directives in its comments:
--   -- @expect-type:  <type>      inferred type must match this string exactly
--   -- @expect-value: <value>     evaluation must produce this result
--   -- @expect-error: <substring> type checking must fail with this substring
data Expectation
  = Type  String
  | Value String
  | Error String

data Outcome = Pass | Fail String

type Run = Either String (String, String)

runProgram :: String -> Run
runProgram src = do
  sast <- parse src
  let ast = desugar sast
  t    <- infer ast
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

mismatch :: String -> String -> String -> String -> String
mismatch lLabel l rLabel r = lLabel ++ ": " ++ l ++ "\n  " ++ rLabel ++ ": " ++ r

testName, testGroup :: FilePath -> String
testName  = dropExtension . makeRelative casesDir
testGroup = takeDirectory . makeRelative casesDir

renderOutcome :: FilePath -> Outcome -> String
renderOutcome p Pass       = "PASS: " ++ testName p
renderOutcome p (Fail msg) = "FAIL: " ++ testName p ++ "\n  " ++ msg

printGroup :: [(FilePath, Outcome)] -> IO ()
printGroup []                = return ()
printGroup grp@((p, _) : _)  = do
  putStrLn $ "── " ++ testGroup p ++ " ──"
  mapM_ (putStrLn . uncurry renderOutcome) grp
  putStrLn ""

judge :: String -> Outcome
judge src = case parseDirectives src of
  []  -> Fail "no expectation directives found"
  exs -> case mapMaybe (check (runProgram src)) exs of
    []    -> Pass
    fails -> Fail (intercalate "\n  " fails)

main :: IO ()
main = do
  hasDir <- doesDirectoryExist casesDir
  unless hasDir $ do
    putStrLn $ "ERROR: " ++ casesDir ++ " not found."
    exitFailure
  files <- findCases casesDir
  results <- mapM (\f -> (,) f . judge <$> readFile f) files
  mapM_ printGroup (groupBy ((==) `on` testGroup . fst) results)
  let passed = length [() | (_, Pass) <- results]
      total  = length results
  putStrLn $ show passed ++ "/" ++ show total ++ " tests passed."
  if passed == total then exitSuccess else exitFailure
