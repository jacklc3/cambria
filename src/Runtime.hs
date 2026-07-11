module Runtime (OpTable, runIO, inbuiltOps, compile) where

import Control.Exception (evaluate)
import Data.Unique (newUnique)
import System.Random (randomIO)

import Semantics (Result(..), eval)
import Syntax

import Environment (initialEnv)
import Inference.Infer (infer)
import Parsing.Parser (parse)
import Parsing.Desugar (desugar)

type OpTable = [(Op, Value -> Value -> IO Computation)]

inbuiltOps :: OpTable
inbuiltOps =
  [ ("fresh",     \_           k -> CApp k . VName         <$> newUnique)
  , ("print",     \(VString s) k -> CApp k . const VUnit   <$> putStrLn s)
  , ("read",      \_           k -> CApp k . VString       <$> getLine)
  , ("flip",      \_           k -> CApp k . VBool         <$> randomIO)
  , ("bernoulli", \(VDouble n) k -> CApp k . VBool . (< n) <$> randomIO)
  , ("uniform",   \_           k -> CApp k . VDouble       <$> randomIO)
  ]

-- Parses, desugars and infers
compile :: String -> Either String (Computation, CompType)
compile src = do
  ast <- parse src >>= desugar
  t <- infer ast
  return (ast, t)

-- Handles effects at the top level with the given table
evalIO :: OpTable -> Computation -> IO Result
evalIO ops c =
  case eval initialEnv c of
    Pure v        -> return $ Pure v
    Impure op v f ->
      case lookup op ops of
        Just k  -> k v f >>= evalIO ops
        Nothing -> return $ Impure op v f

-- Compiles, evaluates under the given operations and strictly renders "value : type".
runIO :: OpTable -> String -> IO (Either String String)
runIO ops src =
  case compile src of
    Left err       -> return (Left err)
    Right (ast, t) -> do
      result <- evalIO ops ast
      let rendered = show result ++ " : " ++ show t
      Right rendered <$ evaluate (foldr seq () rendered)
