module Main where

import Ast
import Parser
import Eval
import Gensym
import System.Environment (getArgs)
import Text.Megaparsec.Error (errorBundlePretty)

topHandler :: Handler
topHandler = Handler ("__", return $ CReturn (VVar "__"))
  [ ("new", "_x", "_k", gensym >>= (\p -> return $ CApp (VVar "_k") (VParameter p)))
  ]


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      case parseProgram filename content of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right ast -> do
          let result = runSymbolGen (eval initialEnv (CHandle topHandler ast))
          print result
    _ -> putStrLn "Usage: run-handler <filename>"
