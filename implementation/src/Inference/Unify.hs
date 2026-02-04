module Inference.Unify where

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
    s1 <- unify t1 t2
    s2 <- unify (apply s1 t1) (apply s1 t2)
    return (s2 `Map.union` s1)
  unify (TVar u) t = bind u t
  unify t (TVar u) = bind u t
  unify t1 t2 = throwError $ "Type mismatch: " ++ show t1 ++ " vs " ++ show t2

instance Unifiable CompType where
  unify (TComp t _) (TComp t' _) = unify t t'

bind :: Ident -> ValueType -> Infer Subst
bind u t
  | t == TVar u = return Map.empty
  | u `Set.member` ftv t = throwError $ "Occurs check fails: " ++ u ++ " in " ++ show t
  | otherwise = return $ Map.singleton u t

agree :: (Ord k, Eq v) => Map.Map k v -> Map.Map k v -> Bool
agree m1 m2 = and (Map.intersectionWith (==) m1 m2)
