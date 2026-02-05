module Inference.Unify where

import Control.Monad (foldM)
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set

import Inference.Types
import Inference.Substitutable

import Syntax (Ident)

class Unifiable a where
  unify :: a -> a -> Infer Subst

instance Unifiable ValueType where
  unify TUnit TUnit     = return Map.empty
  unify TInt TInt       = return Map.empty
  unify TBool TBool     = return Map.empty
  unify TDouble TDouble = return Map.empty
  unify TString TString = return Map.empty
  unify TName TName     = return Map.empty
  unify (TPair t1 t2) (TPair t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 `Map.union` s1)
  unify (TEither t1 t2) (TEither t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 `Map.union` s1)
  unify (TFun t1 t2) (TFun t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 `Map.union` s1)
  unify (THandler t1 t2) (THandler t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 `Map.union` s1)
  unify (TVar u) t = bind u t
  unify t (TVar u) = bind u t
  unify t1 t2 = throwError $ "Type mismatch: " ++ show t1 ++ " vs " ++ show t2

instance Unifiable CompType where
  unify (TComp t es) (TComp t' es') = do
    s1 <- unify t t'
    s2 <- unifyEffectArities (apply s1 es) (apply s1 es')
    return (s2 `Map.union` s1)

bind :: Ident -> ValueType -> Infer Subst
bind u t
  | t == TVar u = return Map.empty
  | u `Set.member` ftv t = throwError $ "Occurs check fails: " ++ u ++ " in " ++ show t
  | otherwise = return $ Map.singleton u t

instance Unifiable Arity where
  unify (Arity t1 t2) (Arity t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 `Map.union` s1)

unifyEffectArities :: Effects -> Effects -> Infer Subst
unifyEffectArities effs1 effs2 = do
  let commonOps = Map.keys $ Map.intersection effs1 effs2
  foldM unifyOp Map.empty commonOps
  where
    unifyOp s op = do
      let Just ar1 = Map.lookup op effs1
      let Just ar2 = Map.lookup op effs2
      s' <- unify (apply s ar1) (apply s ar2)
      return (s' `Map.union` s)
