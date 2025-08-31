module Main where

import Syntax
import Environment (initialEnv)
import Parsing.Parser (parseExpr)
import Parsing.Desugar (desugar)
import Eval (Result(..), eval)

import Data.Unique (newUnique)
import System.Environment (getArgs)
import System.Random (randomIO)

handlerIO :: [(Op, Value -> Value -> IO Computation)]
handlerIO =
  [ ("new",       \_           k -> newUnique  >>= return . CApp k . VName)
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
      case parseExpr content of
        Left err  -> putStr err
        Right sugaredAst -> do
          let ast = desugar sugaredAst
          result <- evalIO initialEnv ast
          print result
    _ -> putStrLn "Usage: run-handler <filename>"
