module Main where

import Ast
import Parser (parseProgram)
import Eval (Result(..), eval, initialEnv)

import Data.Unique (newUnique)
import System.Environment (getArgs)
import System.Random (randomIO)
import Text.Megaparsec.Error (errorBundlePretty)

ioHandler :: [(Op, Value -> Value -> IO Computation)]
ioHandler =
  [ ("new", \_ k -> newUnique >>= (\p -> return $ CApp k (VParameter p)))
  , ("print", \v k -> print v >>= (\_ -> return $ CApp k VUnit))
  , ("flip", \_ k -> randomIO >>= (\b -> return $ CApp k (VBool b)))
  ]

-- Catches any inbuilt effects at the top level and handles them
evalInbuilt :: Env -> Computation -> IO Result
evalInbuilt env c =
  case eval env c of
    Pure v -> return $ Pure v
    Impure op v f ->
      case lookup op ioHandler of
        Just k  -> k v f >>= (\r -> evalInbuilt env r)
        Nothing -> return $ Impure op v f -- Immediatly propogate to top level

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parseProgram filename content of
        Left err  -> putStr (errorBundlePretty err)
        Right ast -> do
          result <- evalInbuilt initialEnv ast
          print result
    _ -> putStrLn "Usage: run-handler <filename>"
