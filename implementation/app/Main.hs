module Main where

import Ast
import Parser
import Eval
import System.Environment (getArgs)
import Text.Megaparsec.Error (errorBundlePretty)

run :: Env -> Computation -> EvalResult
run env c = eval env c

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parseProgram filename content of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right ast -> do
          putStrLn "Parsed code!"
          putStrLn (show ast)
          let result = run initialEnv ast
          print result
    _ -> putStrLn "Usage: run-handler <filename>"
