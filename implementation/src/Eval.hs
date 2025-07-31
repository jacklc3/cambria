module Eval where

import Ast
import qualified Data.Map as Map

data Result
  = Pure Value
  | Impure OpName Value Value
  | RuntimeError String

instance Show Result where
  show (Pure v)         = "Pure " ++ show v
  show (Impure op v f)  = "Impure " ++ op ++ " " ++ show v
  show (RuntimeError s) = "Error: " ++ s

eval :: Env -> Computation -> Result
eval env (CReturn v) = Pure (evalValue env v)

eval env (CApp f v) =
  case evalValue env f of
    VClosure x c cEnv ->
      let newEnv = Map.insert x (evalValue env v) cEnv
      in  eval newEnv c
    VPrimative f ->  eval env (f (evalValue env v))
    _ -> RuntimeError $ "Cannot apply non-function: " ++ show f

eval env (CIf b c1 c2) =
  case evalValue env b of
    VBool True  -> eval env c1
    VBool False -> eval env c2
    v           -> RuntimeError $ "If condition must be a boolean, but got: " ++ show v

eval env (CCase e x1 c1 x2 c2) =
  case evalValue env e of
    VEither L v -> eval (Map.insert x1 v env) c1
    VEither R v -> eval (Map.insert x2 v env) c2
    v           -> RuntimeError $ "Case analysis must be an either, but got: " ++ show v

eval env (CDo x c1 c2) =
  case eval env c1 of
    Pure v               -> eval (Map.insert x v env) c2
    Impure op v f        -> Impure op v (newBody f)
    err@(RuntimeError _) -> err
  where
    newBody f@(VClosure y c env') = VClosure y (CDo x (CApp f (VVar y)) c2) env
    newBody _                     = error "Non-closure in continuation of impure"

eval env (COp op v) =
  Impure op (evalValue env v) (VClosure "_y" (CReturn (VVar "_y")) env)

eval env (CHandle h c) =
  case eval env c of
    Pure v ->
      let RetClause x cr = retClause h
      in  eval (Map.insert x v env) cr
    Impure op v f ->
      case Map.lookup op (opClauses h) of
        Just (OpClause x k cop) ->
          let newEnv = Map.insert x v $ Map.insert k (deepHandle f) env
          in  eval newEnv cop
        Nothing -> Impure op v (deepHandle f)
      where
        deepHandle (VClosure y c env) = VClosure y (CHandle h c) env
        deepHandle _                  = error "continuation of handler was not a closure"
    err@(RuntimeError _) -> err

evalValue :: Env -> Value -> Value
evalValue env (VVar name) =
  case Map.lookup name env of
    Just v -> evalValue env v
    Nothing -> error $ "Unbound variable: " ++ name
evalValue env (VPair v1 v2) = VPair (evalValue env v1) (evalValue env v2)
evalValue env (VEither s v) = VEither s (evalValue env v)
evalValue env (VFun x c)    = VClosure x c env
evalValue _   v             = v

initialEnv :: Env
initialEnv = Map.fromList
  [ ("+", primBinOpInt (\x y -> VInt (x + y)))
  , ("-", primBinOpInt (\x y -> VInt (x - y)))
  , ("*", primBinOpInt (\x y -> VInt (x * y)))
  , ("++", primBinOpStr (\x y -> VString (x ++ y)))
  , ("max", primBinOpInt (\x y -> VInt (max x y)))
  , ("fst", VPrimative (\(VPair x _) -> CReturn x))
  , ("snd", VPrimative (\(VPair _ x) -> CReturn x))
  , ("==", primBinOpVal (\x y -> VBool (x == y)))
  ]

primBinOpInt :: (Integer -> Integer -> Value) -> Value
primBinOpInt op = VPrimative f1
  where
    f1 (VInt x)   = CReturn (VPrimative (f2 x))
    f1 _          = error "Type error: first argument to primitive was not an integer."
    f2 x (VInt y) = CReturn (op x y)
    f2 _ _        = error "Type error: second argument to primitive was not an integer."

primBinOpStr :: (String -> String -> Value) -> Value
primBinOpStr op = VPrimative f1
  where
    f1 (VString x)   = CReturn (VPrimative (f2 x))
    f1 _             = error "Type error: first argument was not a string."
    f2 x (VString y) = CReturn (op x y)
    f2 _ _           = error "Type error: second argument to primitive was not a string."

primBinOpVal :: (Value -> Value -> Value) -> Value
primBinOpVal op = VPrimative f1
  where
    f1 v    = CReturn (VPrimative (f2 v))
    f2 v v' = CReturn (op v v')
