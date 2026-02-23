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
  [ ("unique",    \_           k -> CApp k . VUnique       <$> newUnique)
  , ("print",     \(VString s) k -> CApp k . const VUnit   <$> putStrLn s)
  , ("read",      \_           k -> CApp k . VString       <$> getLine)
  , ("flip",      \_           k -> CApp k . VBool         <$> randomIO)
  , ("bernoulli", \(VDouble n) k -> CApp k . VBool . (< n) <$> randomIO)
  , ("uniform",   \_           k -> CApp k . VDouble       <$> randomIO)
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
              result <- evalIO initialEnv ast
              putStrLn $ show result ++ " : " ++ show t
    _ -> putStrLn "Usage: run-handler <filename>"
