module Inference.Unify where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Inference.Substitutable

import Types

extendSubst :: Subst -> Infer ()
extendSubst s' = modify (\st -> st { subst = compose s' (subst st) })

compose :: Subst -> Subst -> Subst
compose s2 s1 = apply Types s2 s1 `Map.union` s2

applySubst :: Substitutable a => a -> Infer a
applySubst t = do
  s <- gets subst
  return (apply Types s t)

class Unifiable a where
  unify :: a -> a -> Infer ()

instance Unifiable ValueType where
  unify t1 t2 = do
    t1' <- applySubst t1
    t2' <- applySubst t2
    unify' t1' t2'
    where
      unify' TUnit TUnit     = return ()
      unify' TInt TInt       = return ()
      unify' TBool TBool     = return ()
      unify' TDouble TDouble = return ()
      unify' TString TString = return ()
      unify' TUnique TUnique     = return ()
      unify' (TParam p) (TParam q)
        | p == q    = return ()
      unify' (TPair a b) (TPair a' b')     = unify a a' >> unify b b'
      unify' (TEither a b) (TEither a' b') = unify a a' >> unify b b'
      unify' (TFun a b) (TFun a' b')       = unify a a' >> unify b b'
      unify' (THandler a b) (THandler a' b') = unify a a' >> unify b b'
      unify' (TVar u) t = bind u t
      unify' t (TVar u) = bind u t
      unify' a b = throwError $ "Type mismatch: " ++ show a ++ " vs " ++ show b

instance Unifiable CompType where
  unify (TComp t es) (TComp t' es') = unify t t' >> unify es es'

instance Unifiable Arity where
  unify (Arity t1 t2) (Arity t1' t2') = unify t1 t1' >> unify t2 t2'

instance (Unifiable v, Substitutable v, Ord k) => Unifiable (Map.Map k v) where
  unify m1 m2 = mapM_ unifyV (Map.intersectionWith (,) m1 m2)
    where unifyV (v1, v2) = unify v1 v2

bind :: Ident -> ValueType -> Infer ()
bind u t
  | t == TVar u = return ()
  | u `Set.member` free Types t = throwError $ "Occurs check fails: " ++ u ++ " in " ++ show t
  | otherwise = extendSubst (Map.singleton u t)
