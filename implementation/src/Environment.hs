module Environment where

import Ast
import Data.Unique (hashUnique)
import qualified Data.Map as Map

find :: Ident -> Env -> Value
find x (Env env) =
  case Map.lookup x env of
    Just v  -> v
    Nothing -> error $ "Unbound variable: " ++ x

def :: Ident -> Value -> Env -> Env
def x v (Env env) = Env (Map.insert x v env)

initialEnv :: Env
initialEnv = Env $ Map.fromList
  [ ("+", primBinOpInt (\x y -> VInt (x + y)))
  , ("-", primBinOpInt (\x y -> VInt (x - y)))
  , ("*", primBinOpInt (\x y -> VInt (x * y)))
  , ("/", primBinOpInt (\x y -> VDouble ((fromIntegral x) / (fromIntegral y))))
  , ("++", primBinOpStr (\x y -> VString (x ++ y)))
  , ("max", primBinOpInt (\x y -> VInt (max x y)))
  , ("fst", VPrimitive (\(VPair x _) -> CReturn x))
  , ("snd", VPrimitive (\(VPair _ x) -> CReturn x))
  , ("==", primBinOpVal (\x y -> VBool (x == y)))
  , ("hash", VPrimitive (\(VParameter a) -> CReturn $ VString $ show $ hashUnique a))
  ]

primBinOpInt :: (Integer -> Integer -> Value) -> Value
primBinOpInt op = VPrimitive f1
  where
    f1 (VInt x)   = CReturn (VPrimitive (f2 x))
    f1 _          = error "Type error: first argument to primitive was not an integer."
    f2 x (VInt y) = CReturn (op x y)
    f2 _ _        = error "Type error: second argument to primitive was not an integer."

primBinOpStr :: (String -> String -> Value) -> Value
primBinOpStr op = VPrimitive f1
  where
    f1 (VString x)   = CReturn (VPrimitive (f2 x))
    f1 _             = error "Type error: first argument to primative was not a string."
    f2 x (VString y) = CReturn (op x y)
    f2 _ _           = error "Type error: second argument to primitive was not a string."

primBinOpVal :: (Value -> Value -> Value) -> Value
primBinOpVal op = VPrimitive f1
  where
    f1 v    = CReturn (VPrimitive (f2 v))
    f2 v v' = CReturn (op v v')
