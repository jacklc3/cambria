module Main where

import Ast
import Parser (parseProgram)
import Eval (Result(..), eval, initialEnv)
import Gensym (SymbolGenT, runSymbolGenT, gensym, liftIO)
import System.Environment (getArgs)
import Text.Megaparsec.Error (errorBundlePretty)
import Data.Map (Map)
import qualified Data.Map as Map

type M a = SymbolGenT Parameter IO a
type InbuiltHandler = Map OpName (Value -> Value -> M Computation)

-- TODO: Add other IO effects like randomness
inbuiltHandler :: InbuiltHandler
inbuiltHandler = Map.fromList
  [ ("new", \_ k -> gensym >>= (\p -> return $ CApp k (VParameter p)))
  , ("print", \(VString s) k -> liftIO (putStrLn s) >>= (\_ -> return $ CApp k VUnit))
  ]

-- Catches any inbuilt effects at the top level and handles them
evalInbuilt :: Env -> Computation -> M Result
evalInbuilt env c =
  case eval env c of
    Pure v -> return $ Pure v
    Impure op v cl ->
      case Map.lookup op inbuiltHandler of
        Just k  -> k v cl >>= (\r -> evalInbuilt env r)
        Nothing -> return $ Impure op v cl -- Immediatly propogate to top level

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parseProgram filename content of
        Left err  -> putStr (errorBundlePretty err)
        Right ast -> do
          result <- runSymbolGenT (evalInbuilt initialEnv ast)
          print result
    _ -> putStrLn "Usage: run-handler <filename>"
