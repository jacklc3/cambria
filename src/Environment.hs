module Environment where

import qualified Data.Map as Map
import Data.Unique (hashUnique, newUnique)
import System.Random (randomIO)

import Syntax

find :: Ident -> Env -> Value
find x (Env env) =
  case Map.lookup x env of
    Just v  -> v
    Nothing -> error $ "Unbound variable: " ++ x

def :: Ident -> Value -> Env -> Env
def x v (Env env) = Env (Map.insert x v env)

primitives :: [(Ident, Value -> Value)]
primitives =
  [ ("+",      \(VPair (VInt x) (VInt y)) -> VInt (x + y))
  , ("-",      \(VPair (VInt x) (VInt y)) -> VInt (x - y))
  , ("*",      \(VPair (VInt x) (VInt y)) -> VInt (x * y))
  , ("/",      \(VPair (VInt x) (VInt y)) -> VDouble (fromIntegral x / fromIntegral y))
  , ("++",     \(VPair (VString x) (VString y)) -> VString (x ++ y))
  , ("max",    \(VPair (VInt x) (VInt y)) -> VInt (max x y))
  , ("fst",    \(VPair x _) -> x)
  , ("snd",    \(VPair _ x) -> x)
  , ("==",     \(VPair x y) -> VBool (x == y))
  , ("hash",   \(VUnique a) -> VString $ show $ hashUnique a)
  , ("insert", \(VPair (VPair k v) (VMap m)) -> VMap ((k, v) : filter (\(k', _) -> k' /= k) m))
  , ("remove", \(VPair k (VMap m)) -> VMap (filter (\(k', _) -> k' /= k) m))
  , ("lookup", \(VPair k (VMap m)) -> case Prelude.lookup k m of
      Just v  -> v
      Nothing -> error $ "Key not found in map: " ++ show k)
  , ("member", \(VPair k (VMap m)) -> VBool (any (\(k', _) -> k' == k) m))
  , ("::",     \(VPair x (VList xs)) -> VList (x : xs))
  , ("null",   \(VList xs) -> VBool (null xs))
  , ("uncons", \(VList xs') -> case xs' of
      []     -> VEither L VUnit
      (x:xs) -> VEither R (VPair x (VList xs)))
  ]

constants :: [(Ident, Value)]
constants =
  [ ("[]",    VList [])
  , ("empty", VMap [])
  ]

primitiveOps :: [(Op, Value -> Value -> IO Computation)]
primitiveOps =
  [ ("unique",    \_           k -> CApp k . VUnique       <$> newUnique)
  , ("print",     \(VString s) k -> CApp k . const VUnit   <$> putStrLn s)
  , ("read",      \_           k -> CApp k . VString       <$> getLine)
  , ("flip",      \_           k -> CApp k . VBool         <$> randomIO)
  , ("bernoulli", \(VDouble n) k -> CApp k . VBool . (< n) <$> randomIO)
  , ("uniform",   \_           k -> CApp k . VDouble       <$> randomIO)
  ]

initialEnv :: Env
initialEnv = Env (Map.fromList (map liftPrim primitives ++ constants))
  where liftPrim (name, f) = (name, VPrimitive (CReturn . f))
