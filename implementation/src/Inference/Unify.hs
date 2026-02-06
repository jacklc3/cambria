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
  unify TUnit TUnit     = return mempty
  unify TInt TInt       = return mempty
  unify TBool TBool     = return mempty
  unify TDouble TDouble = return mempty
  unify TString TString = return mempty
  unify TName TName     = return mempty
  unify (TPair t1 t2) (TPair t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 <> s1)
  unify (TEither t1 t2) (TEither t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 <> s1)
  unify (TFun t1 t2) (TFun t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 <> s1)
  unify (THandler t1 t2) (THandler t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 <> s1)
  unify (TVar u) t = bind u t
  unify t (TVar u) = bind u t
  unify t1 t2 = throwError $ "Type mismatch: " ++ show t1 ++ " vs " ++ show t2

instance Unifiable CompType where
  unify (TComp t es) (TComp t' es') = do
    s1 <- unify t t'
    s2 <- unify (apply s1 es) (apply s1 es')
    return (s2 <> s1)

instance Unifiable Arity where
  unify (Arity t1 t2) (Arity t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 <> s1)

instance (Unifiable v, Substitutable v, Ord k) => Unifiable (Map.Map k v) where
  unify m1 m2 = do
    foldM unifyV mempty (Map.intersectionWith (,) m1 m2)
    where
      unifyV s (v1, v2) = do
        s' <- unify (apply s v1) (apply s v2)
        return (s' <> s)

bind :: Ident -> ValueType -> Infer Subst
bind u t
  | t == TVar u = return mempty
  | u `Set.member` ftv t = throwError $ "Occurs check fails: " ++ u ++ " in " ++ show t
  | otherwise = return $ Map.singleton u t
