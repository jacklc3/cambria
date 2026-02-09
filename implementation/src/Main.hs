module Main where

import Data.Unique (newUnique)
import System.Environment (getArgs)
import System.Random (randomIO)

import Environment (initialEnv)
import Eval (Result(..), eval)
import Syntax

import Parsing.Parser (parse)
import Parsing.Desugar (desugar)

import Inference.Infer (infer)

handlerIO :: [(Op, Value -> Value -> IO Computation)]
handlerIO =
  [ ("unique",    \_           k -> newUnique  >>= return . CApp k . VUnique)
  , ("print",     \(VString s) k -> putStrLn s >>= return . CApp k . const VUnit)
  , ("read",      \_           k -> getLine    >>= return . CApp k . VString)
  , ("flip",      \_           k -> randomIO   >>= return . CApp k . VBool)
  , ("bernoulli", \(VDouble n) k -> randomIO   >>= return . CApp k . VBool . (< n))
  , ("uniform",   \_           k -> randomIO   >>= return . CApp k . VDouble)
  ]

-- Catches any inbuilt effects at the top level and handles them
evalIO :: Env -> Computation -> IO Result
evalIO env c =
  case eval env c of
    Pure v        -> return $ Pure v
    Impure op v f ->
      case lookup op handlerIO of
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
              putStrLn $ "Type inferred: " ++ show t
              result <- evalIO initialEnv ast
              print result
    _ -> putStrLn "Usage: run-handler <filename>"
