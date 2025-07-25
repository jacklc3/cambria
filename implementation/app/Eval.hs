module Eval where

import Ast
import Data.Map (Map)
import Data.List (foldl')
import qualified Data.Map as Map

type Env = Map VarName Value

data EvalResult
    = Pure Value
    | Impure OpName Value (Value -> Computation)
    | RuntimeError String

instance Show EvalResult where
    show (Pure v) = "Pure " ++ show v
    show (Impure op v _) = "Impure " ++ op ++ " " ++ show v
    show (RuntimeError s) = "Error: " ++ s

eval :: Env -> Computation -> EvalResult
eval env (CReturn val) = Pure (evalValue env val)

eval env (CApp funVal argVal) =
  case evalValue env funVal of
    VFun paramName body ->
      let newEnv = Map.insert paramName (evalValue env argVal) env
      in eval newEnv body
    {-
    VRecFun recEnv fName xName body ->
      let evaluatedArg = evalValue env argVal
          recClosure = VRecFun recEnv fName xName body
          newEnv = Map.insert xName evaluatedArg (Map.insert fName recClosure recEnv)
      in eval newEnv body
    -}
    VContinuation k -> eval env (k (evalValue env argVal))
    _ -> RuntimeError $ "Cannot apply non-function: " ++ show funVal

eval env (CIf cond c1 c2) =
  case evalValue env cond of
    VBool True -> eval env c1
    VBool False -> eval env c2
    _ -> RuntimeError $ "If condition must be a boolean, but got: " ++ show cond

eval env (CSeq var c1 c2) =
  case eval env c1 of
    Pure val -> eval (Map.insert var val env) c2
    Impure op v k -> Impure op v (\res -> CSeq var (k res) c2)
    err@(RuntimeError _) -> err

eval env (COp op v) =
    Impure op (evalValue env v) CReturn

eval env (CHandle handler comp) =
    evalHandler env handler comp

evalValue :: Env -> Value -> Value
evalValue env (VVar name) =
  case Map.lookup name env of
    Just val -> evalValue env val
    Nothing -> error $ "Unbound variable: " ++ name
evalValue env (VPair v1 v2) = VPair (evalValue env v1) (evalValue env v2)
evalValue _ val = val

evalHandler :: Env -> Handler -> Computation -> EvalResult
evalHandler env handler comp =
  case eval env comp of
    Pure val -> let (var, retBody) = hReturnClause handler
                in eval (Map.insert var val env) retBody

    Impure opName opVal opCont ->
      case findOpClause opName (hOpClauses handler) of
        Just (paramName, contName, clauseBody) ->
          let
            handledContinuation = VContinuation (\resultVal ->
              CHandle handler (opCont resultVal))
            handlerEnv = Map.insert paramName opVal
                       $ Map.insert contName handledContinuation env
          in eval handlerEnv clauseBody
        Nothing ->
          Impure opName opVal (\resultVal ->
            CHandle handler (opCont resultVal))

    err@(RuntimeError _) -> err

findOpClause :: OpName -> [(OpName, VarName, VarName, Computation)] -> Maybe (VarName, VarName, Computation)
findOpClause name = foldl' go Nothing
  where
    go Nothing (op, x, k, c) | op == name = Just (x, k, c)
    go acc _ = acc

initialEnv :: Env
initialEnv = Map.fromList [
    ("+", primBinOpInt (\x y -> VInt (x + y))),
    ("-", primBinOpInt (\x y -> VInt (x - y))),
    ("*", primBinOpInt (\x y -> VInt (x * y))),
    ("++", primBinOpStr (\x y -> VString (x ++ y))),
    ("max", primBinOpInt (\x y -> VInt (max x y))),
    ("fst", VContinuation (\(VPair x _) -> CReturn x)),
    ("snd", VContinuation (\(VPair _ x) -> CReturn x)) ]

primBinOpInt :: (Integer -> Integer -> Value) -> Value
primBinOpInt op = VContinuation handle_x
  where
    handle_x (VInt x) = CReturn (VContinuation (handle_y x))
    handle_x _        = error "Type error: first argument to primitive was not an integer."
    handle_y x (VInt y) = CReturn (op x y)
    handle_y _ _        = error "Type error: second argument to primitive was not an integer."

primBinOpStr :: (String -> String -> Value) -> Value
primBinOpStr op = VContinuation handle_x
  where
    handle_x (VString x)   = CReturn (VContinuation (handle_y x))
    handle_x _             = error "Type error: first argument was not a string."
    handle_y x (VString y) = CReturn (op x y)
    handle_y v v'          = error ("Type error: second argument was not a string: " ++ show v ++ show v')
