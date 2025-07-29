module Eval where

import Ast
import qualified Data.Map as Map

data Result
    = Pure Value
    | Impure OpName Value (Value -> Computation) Env
    | RuntimeError String

instance Show Result where
    show (Pure v)        = "Pure " ++ show v
    show (Impure op v _ _) = "Impure " ++ op ++ " " ++ show v
    show (RuntimeError s)  = "Error: " ++ s

eval :: Env -> Computation -> Result
eval env (CReturn v) = Pure (evalValue env v)

eval env (CApp f v) =
  case evalValue env f of
    VClosure x c cEnv ->
      let newEnv = Map.insert x (evalValue env v) cEnv
      in  eval newEnv c
    {-
    VRecFun recEnv fName xName body ->
      let evaluatedArg = evalValue env v
          recClosure = VRecFun recEnv fName xName body
          newEnv = Map.insert xName evaluatedArg (Map.insert fName recClosure recEnv)
      in eval newEnv body
    -}
    VContinuation k kEnv ->  eval kEnv (k (evalValue env v))
    _ -> RuntimeError $ "Cannot apply closure or continutation: " ++ show f

eval env (CIf cond c1 c2) =
  case evalValue env cond of
    VBool True  -> eval env c1
    VBool False -> eval env c2
    _           -> RuntimeError $ "If condition must be a boolean, but got: " ++ show cond

eval env (CSeq x c1 c2) =
  case eval env c1 of
    Pure v               -> eval (Map.insert x v env) c2
    Impure op v k opEnv  -> Impure op v (\res -> CSeq x (k res) c2) opEnv
    err@(RuntimeError _) -> err

eval env (COp op v) =
  Impure op (evalValue env v) CReturn env

eval env (CHandle h c) =
  case eval env c of
    Pure v ->
      let RetClause x c' = hRetClause h
      in  eval (Map.insert x v env) c'
    Impure op v opCont opEnv ->
      case Map.lookup op (hOpClauses h) of
        Just (OpClause x k c') ->
          let hCont = VContinuation deepHandle opEnv
              hEnv = Map.insert x v $ Map.insert k hCont opEnv
          in  eval hEnv c'
        Nothing -> Impure op v deepHandle opEnv
      where deepHandle v = CHandle h (opCont v)
    err@(RuntimeError _) -> err

evalValue :: Env -> Value -> Value
evalValue env (VVar name) =
  case Map.lookup name env of
    Just v -> evalValue env v
    Nothing -> error $ "Unbound variable: " ++ name
evalValue env (VPair v1 v2) = VPair (evalValue env v1) (evalValue env v2)
evalValue env (VFun x c) = VClosure x c env
evalValue _ v = v

initialEnv :: Env
initialEnv = Map.fromList [
    ("+", primBinOpInt (\x y -> VInt (x + y))),
    ("-", primBinOpInt (\x y -> VInt (x - y))),
    ("*", primBinOpInt (\x y -> VInt (x * y))),
    ("++", primBinOpStr (\x y -> VString (x ++ y))),
    ("max", primBinOpInt (\x y -> VInt (max x y))),
    ("fst", VContinuation (\(VPair x _) -> CReturn x) Map.empty),
    ("snd", VContinuation (\(VPair _ x) -> CReturn x) Map.empty) ]

primBinOpInt :: (Integer -> Integer -> Value) -> Value
primBinOpInt op = VContinuation handle_x Map.empty
  where
    handle_x (VInt x) = CReturn (VContinuation (handle_y x) Map.empty)
    handle_x _        = error "Type error: first argument to primitive was not an integer."
    handle_y x (VInt y) = CReturn (op x y)
    handle_y _ _        = error "Type error: second argument to primitive was not an integer."

primBinOpStr :: (String -> String -> Value) -> Value
primBinOpStr op = VContinuation handle_x Map.empty
  where
    handle_x (VString x)   = CReturn (VContinuation (handle_y x) Map.empty)
    handle_x _             = error "Type error: first argument was not a string."
    handle_y x (VString y) = CReturn (op x y)
    handle_y v v'          = error ("Type error: second argument was not a string: " ++ show v ++ show v')
