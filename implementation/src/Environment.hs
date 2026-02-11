module Environment where

import Data.Unique (hashUnique)
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (catch, PatternMatchFail)

import Syntax

find :: Ident -> Env -> Value
find x (Env env) =
  case Map.lookup x env of
    Just v  -> v
    Nothing -> error $ "Unbound variable: " ++ x

def :: Ident -> Value -> Env -> Env
def x v (Env env) = Env (Map.insert x v env)

primitives :: [(String, Value -> Value)]
primitives =
  [ ("+",    \(VPair (VInt x) (VInt y)) -> VInt (x + y))
  , ("-",    \(VPair (VInt x) (VInt y)) -> VInt (x - y))
  , ("*",    \(VPair (VInt x) (VInt y)) -> VInt (x * y))
  , ("/",    \(VPair (VInt x) (VInt y)) -> VDouble ((fromIntegral x) / (fromIntegral y)))
  , ("++",   \(VPair (VString x) (VString y)) -> VString (x ++ y))
  , ("max",  \(VPair (VInt x) (VInt y)) -> VInt (max x y))
  , ("fst",  \(VPair x _) -> x)
  , ("snd",  \(VPair _ x) -> x)
  , ("==",   \(VPair x y) -> VBool (x == y))
  , ("hash", \(VUnique a) -> VString $ show $ hashUnique a)
  , ("empty",  \VUnit -> VMap [])
  , ("insert", \(VPair (VPair k v) (VMap m)) -> VMap ((k, v) : filter (\(k', _) -> k' /= k) m))
  , ("remove", \(VPair k (VMap m)) -> VMap (filter (\(k', _) -> k' /= k) m))
  , ("lookup", \(VPair k (VMap m)) -> case Prelude.lookup k m of
      Just v  -> v
      Nothing -> error $ "Key not found in map: " ++ show k)
  , ("member", \(VPair k (VMap m)) -> VBool (any (\(k', _) -> k' == k) m))
  , ("nil",    \VUnit -> VList [])
  , ("cons",   \(VPair x (VList xs)) -> VList (x : xs))
  , ("head",   \(VList (x:_)) -> x)
  , ("tail",   \(VList (_:xs)) -> VList xs)
  , ("isnil",  \(VList xs) -> VBool (null xs))
  ]

initialEnv :: Env
initialEnv = Env $ Map.fromList $ map guardPrimitive $ primitives
  where guardPrimitive (prim, f) = (prim, VPrimitive (CReturn . primwrap prim f))

primwrap :: String -> (Value -> Value) -> Value -> Value
primwrap prim f v = unsafePerformIO (catch (return $! f v) match_fail)
  where
    match_fail :: PatternMatchFail -> a
    match_fail _ = error $ "Runtime Error: Invalid arguments to '" ++ prim ++ "': " ++ show v
