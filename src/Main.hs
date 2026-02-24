module Main where

import System.Environment (getArgs)

import Environment (initialEnv, primitiveOps)
import Eval (Result(..), eval)
import Syntax

import Parsing.Parser (parse)
import Parsing.Desugar (desugar)

import Inference.Infer (infer)

-- Catches any inbuilt effects at the top level and handles them
evalIO :: Env -> Computation -> IO Result
evalIO env c =
  case eval env c of
    Pure v        -> return $ Pure v
    Impure op v f ->
      case lookup op primitiveOps of
        Just k  -> k v f >>= evalIO env
        Nothing -> return $ Impure op v f

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parse content of
        Left err -> putStrLn err
        Right sugaredAst -> do
          let ast = desugar sugaredAst
          case infer ast of
            Left err -> putStrLn err
            Right t -> do
              result <- evalIO initialEnv ast
              putStrLn $ show result ++ " : " ++ show t
    _ -> putStrLn "Usage: run-handler <filename>"
