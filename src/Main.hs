module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Runtime (runIO, inbuiltOps)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      result  <- runIO inbuiltOps content
      case result of
        Left err  -> putStrLn err >> exitFailure
        Right out -> putStrLn out
    _ -> putStrLn "Usage: cambria <filename>"
