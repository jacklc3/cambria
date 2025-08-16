module Main where

import Ast
import Environment (initialEnv)
import Parser (parseProgram)
import Eval (Result(..), eval)

import Data.Unique (newUnique)
import System.Environment (getArgs)
import System.Random (randomIO)
import Text.Megaparsec.Error (errorBundlePretty)

handlerIO :: [(Op, Value -> Value -> IO Computation)]
handlerIO =
  [ ("new",   \_ k           -> newUnique  >>= return . CApp k . VParameter)
  , ("print", \(VString s) k -> putStrLn s >>= return . CApp k . const VUnit)
  , ("read",  \_ k           -> getLine    >>= return . CApp k . VString)
  , ("flip",  \_ k           -> randomIO   >>= return . CApp k . VBool)
  , ("flipratio",  \(VPair (VInt i) (VInt j)) k
        -> (randomIO :: IO Double) >>= \r -> return $ CApp k $ VBool $ r <  ((fromIntegral i) / (fromIntegral $ i+j)))
  ]

-- Catches any inbuilt effects at the top level and handles them
evalIO :: Env -> Computation -> IO Result
evalIO env c =
  case eval env c of
    Pure v        -> return $ Pure v
    Impure op v f ->
      case lookup op handlerIO of
        Just k  -> k v f >>= (\r -> evalIO env r)
        Nothing -> return $ Impure op v f

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parseProgram filename content of
        Left err  -> putStr (errorBundlePretty err)
        Right ast -> do
          result <- evalIO initialEnv ast
          print result
    _ -> putStrLn "Usage: run-handler <filename>"
