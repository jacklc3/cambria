module Inference.Unify where

import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set

import Inference.Types
import Inference.Substitutable

unify :: Type -> Type -> Infer Subst
unify TUnit TUnit     = return Map.empty
unify TInt TInt       = return Map.empty
unify TBool TBool     = return Map.empty
unify TDouble TDouble = return Map.empty
unify TString TString = return Map.empty
unify (TPair t1 t2) (TPair t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify (apply s1 t2) (apply s1 t2')
  return (s2 `Map.union` s1)
unify (TEither t1 t2) (TEither t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify (apply s1 t2) (apply s1 t2')
  return (s2 `Map.union` s1)
unify (TFun t1 t2 e2) (TFun t1' t2' e2')
  | e2 == e2' = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 `Map.union` s1)
unify (THandler t1 e1 t2 e2) (THandler t1' e1' t2' e2')
  | e1 == e1' && e2 == e2' = do
    s1 <- unify t1 t2
    s2 <- unify (apply s1 t1) (apply s1 t2)
    return (s2 `Map.union` s1)
unify (TVar u) t = bind u t
unify t (TVar u) = bind u t
unify t1 t2 = throwError $ "Type mismatch: " ++ show t1 ++ " vs " ++ show t2

bind :: String -> Type -> Infer Subst
bind u t
  | t == TVar u = return Map.empty
  | u `Set.member` ftv t = throwError $ "Occurs check fails: " ++ u ++ " in " ++ show t
  | otherwise = return $ Map.singleton u t
