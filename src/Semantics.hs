{-# LANGUAGE LambdaCase #-}

module Semantics where

import Syntax
import qualified Data.Map as Map

find :: Ident -> Env -> Value
find x (Env env) =
  case Map.lookup x env of
    Just v  -> v
    Nothing -> error $ "Unbound variable: " ++ x

def :: Ident -> Value -> Env -> Env
def x v (Env env) = Env (Map.insert x v env)

data Result
  = Pure Value
  | Impure Op Value Value

instance Show Result where
  show (Pure v)         = "Pure: " ++ show v
  show (Impure op v _)  = "Impure: !" ++ op ++ " " ++ show v

eval :: Env -> Computation -> Result
eval env = \case
  CReturn v ->
    Pure (evalValue env v)
  CApp f v ->
    case evalValue env f of
      VFunClosure x c cEnv ->
        let newEnv = def x (evalValue env v) cEnv
        in  eval newEnv c
      self@(VRecClosure g x c cEnv) ->
        let newEnv = def x (evalValue env v) (def g self cEnv)
        in  eval newEnv c
      VPrimitive prim -> eval env (prim (evalValue env v))
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
      newCont f@(VFunClosure y _ _) = VFunClosure y (CDo x (CApp f (VVar y)) c2) env
      newCont v                  = error $ "Non-closure in impure continuation: " ++ show v
  CEffect _ _ c -> eval env c
  CAnnot c _ -> eval env c
  COp op v ->
    Impure op (evalValue env v) (VFunClosure "_y" (CReturn (VVar "_y")) env)
  CHandle hv _ c ->
    case evalValue env hv of
      VHandlerClosure h henv -> case finClause h of
        -- Encode finally clause in a do action if it exists
        Just (FinClause x c') ->
          eval env (CDo x (CHandle (VHandlerClosure h{ finClause = Nothing } henv) [] c)
                          (CApp (VFunClosure x c' henv) (VVar x)))
        Nothing ->
          case eval env c of
            Pure v ->
              let RetClause x cr = retClause h
              in  eval (def x v henv) cr
            Impure op v f ->
              case lookup op (opClauses h) of
                Just (OpClause x k cop) ->
                  let newEnv = def x v $ def k (deepHandle f) henv
                  in  eval newEnv cop
                Nothing -> Impure op v (deepHandle f)
              where
                deepHandle (VFunClosure y c' cenv) = VFunClosure y (CHandle (VHandlerClosure h henv) [] c') cenv
                deepHandle v                     = error $ "Non-closure in impure continuation: " ++ show v
      v -> error $ "Non-handler in handle statement: " ++ show v

evalValue :: Env -> Value -> Value
evalValue env = \case
  VVar x      -> evalValue env (find x env)
  VPair v1 v2 -> VPair (evalValue env v1) (evalValue env v2)
  VEither s v -> VEither s (evalValue env v)
  VFun x c    -> VFunClosure x c env
  VRec f x c  -> VRecClosure f x c env
  VHandler h  -> VHandlerClosure h env
  VAnnot v _  -> evalValue env v
  v           -> v
