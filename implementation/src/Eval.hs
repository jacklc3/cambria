{-# LANGUAGE LambdaCase #-}

module Eval where

import Ast
import Environment (def, find)

data Result
  = Pure Value
  | Impure Op Value Value

instance Show Result where
  show (Pure v)         = "Pure " ++ show v
  show (Impure op v f)  = "Impure " ++ op ++ " " ++ show v

eval :: Env -> Computation -> Result
eval env = \case
  CReturn v ->
    Pure (evalValue env v)
  CApp f v ->
    case evalValue env f of
      VClosure x c cEnv ->
        let newEnv = def x (evalValue env v) cEnv
        in  eval newEnv c
      VPrimitive f ->  eval env (f (evalValue env v))
      _            -> error $ "Non-function in application: " ++ show f
  CIf b c1 c2 ->
    case evalValue env b of
      VBool True  -> eval env c1
      VBool False -> eval env c2
      v           -> error $ "Non-boolean in if condition: " ++ show v
  CCase e x1 c1 x2 c2 ->
    case evalValue env e of
      VEither L v -> eval (def x1 v env) c1
      VEither R v -> eval (def x2 v env) c2
      v           -> error $ "Non-either in case analysis: " ++ show v
  CDo x c1 c2 ->
    case eval env c1 of
      Pure v               -> eval (def x v env) c2
      Impure op v f        -> Impure op v (newCont f)
    where
      newCont f@(VClosure y _ _) = VClosure y (CDo x (CApp f (VVar y)) c2) env
      newCont v                  = error $ "Non-closure in impure continuation: " ++ show v
  COp op v ->
    Impure op (evalValue env v) (VClosure "_y" (CReturn (VVar "_y")) env)
  CHandle (VHandler h) c ->
    case eval env c of
      Pure v ->
        let RetClause x cr = retClause h
        in  eval (def x v env) cr
      Impure op v f ->
        case lookup op (opClauses h) of
          Just (OpClause x k cop) ->
            let newEnv = def x v $ def k (deepHandle f) env
            in  eval newEnv cop
          Nothing -> Impure op v (deepHandle f)
        where
          deepHandle (VClosure y c env) = VClosure y (CHandle (VHandler h) c) env
          deepHandle v                  = error $ "Non-closure in impure continuation: " ++ show v
  CHandle v c ->
    error $ "Non-handler in handle statement: " ++ show v

evalValue :: Env -> Value -> Value
evalValue env = \case
  VVar x      -> evalValue env (find x env)
  VPair v1 v2 -> VPair (evalValue env v1) (evalValue env v2)
  VEither s v -> VEither s (evalValue env v)
  VFun x c    -> VClosure x c env
  VRec f x c  -> let env' = def f (VClosure x c env') env in VClosure x c env'
  v           -> v
