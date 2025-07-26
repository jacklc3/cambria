module Eval where

import Ast
import Data.Map (Map)
import Data.List (foldl', find)
import qualified Data.Map as Map

data Result
    = Pure Value
    | Impure OpName Value (Value -> M Computation) Env
    | RuntimeError String

instance Show Result where
    show (Pure v)          = "Pure " ++ show v
    show (Impure op v _ _) = "Impure " ++ op ++ " " ++ show v
    show (RuntimeError s)  = "Error: " ++ s

eval :: Env -> Computation -> M Result
eval env (CReturn v) = return $ Pure (evalValue env v)

eval env (CApp funVal argVal) =
  case evalValue env funVal of
    VClosure x c cEnv ->
      let newEnv = Map.insert x (evalValue env argVal) cEnv
      in eval newEnv c
    {-
    VRecFun recEnv fName xName body ->
      let evaluatedArg = evalValue env argVal
          recClosure = VRecFun recEnv fName xName body
          newEnv = Map.insert xName evaluatedArg (Map.insert fName recClosure recEnv)
      in eval newEnv body
    -}
    VContinuation k kEnv -> k (evalValue env argVal) >>= (\r -> eval kEnv r)
    _ -> return $ RuntimeError $ "Cannot apply closure or continutation: " ++ show funVal

eval env (CIf cond c1 c2) =
  case evalValue env cond of
    VBool True -> eval env c1
    VBool False -> eval env c2
    _ -> return $ RuntimeError $ "If condition must be a boolean, but got: " ++ show cond

eval env (CSeq var c1 c2) =
  eval env c1 >>= (\r -> case r of
    Pure v -> eval (Map.insert var v env) c2
    Impure op v k opEnv -> return $ Impure op v (\res ->
      k res >>= (\r -> return $ CSeq var r c2)) opEnv
    err@(RuntimeError _) -> return $ err
  )

eval env (COp op v) =
    return $ Impure op (evalValue env v) (return . CReturn) env

eval env (CHandle h c) = evalHandle env h c

evalHandle :: Env -> Handler -> Computation -> M Result
evalHandle env h c =
  eval env c >>= (\r ->
    case r of
      Pure v -> let (x, c') = hReturnClause h
                in c' >>= (\r -> eval (Map.insert x v env) r)
      Impure op v cont opEnv ->
        case findOpClause op (hOpClauses h) of
          Just (x, k, c') ->
            let hCont = VContinuation deepHandle opEnv
                hEnv = Map.insert x v $ Map.insert k hCont env
            in  c' >>= (\r -> eval hEnv r)
          Nothing -> return $ Impure op v deepHandle opEnv
        where deepHandle v' = cont v' >>= (\r -> return $ CHandle h r)
      err@(RuntimeError _) -> return err)
  where
    findOpClause op cs = case find (\(op', _, _, _) -> op == op') cs of
                           Nothing           -> Nothing
                           Just (_, x, k, c) -> Just (x, k, c)

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
    ("fst", VContinuation (\(VPair x _) -> return $ CReturn x) Map.empty),
    ("snd", VContinuation (\(VPair _ x) -> return $ CReturn x) Map.empty) ]

primBinOpInt :: (Integer -> Integer -> Value) -> Value
primBinOpInt op = VContinuation handle_x Map.empty
  where
    handle_x (VInt x) = return $ CReturn (VContinuation (handle_y x) Map.empty)
    handle_x _        = error "Type error: first argument to primitive was not an integer."
    handle_y x (VInt y) = return $ CReturn (op x y)
    handle_y _ _        = error "Type error: second argument to primitive was not an integer."

primBinOpStr :: (String -> String -> Value) -> Value
primBinOpStr op = VContinuation handle_x Map.empty
  where
    handle_x (VString x)   = return $ CReturn (VContinuation (handle_y x) Map.empty)
    handle_x _             = error "Type error: first argument was not a string."
    handle_y x (VString y) = return $ CReturn (op x y)
    handle_y v v'          = error ("Type error: second argument was not a string: " ++ show v ++ show v')
