module Inference.Unify where

import Control.Monad.Except (throwError)
import Control.Monad.State (gets, modify)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Types

import Inference.Monad
import Inference.Effects (addEffectOps)
import Inference.Substitutable

extendSubst :: Subst -> Infer ()
extendSubst (Type s')     = modify (\st ->
  st { typeSubst = Map.map (apply (Type s')) (typeSubst st) <> s' })
extendSubst (Effect s')   = modify (\st ->
  st { effectSubst  = Map.map (apply (Effect s'))  (effectSubst st)  <> s' })
extendSubst (Parameter _) = error "Parameter substitutions should not be added to state"

applySubst :: Substitutable a => a -> Infer a
applySubst t = do
  ts <- gets typeSubst
  es <- gets effectSubst
  return (apply (Effect es) (apply (Type ts) t))

freshEffects :: Infer EffectsType
freshEffects = do
  c <- gets count
  modify (\st -> st { count = succ c })
  return $ Open mempty ("e" ++ show c)

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
      unify' TUnique TUnique = return ()
      unify' (TParam p) (TParam q)
        | p == q = return ()
      unify' (TVar u) t = bind u t
      unify' t (TVar u) = bind u t
      unify' (TPair a b) (TPair a' b')     = unify a a' >> unify b b'
      unify' (TEither a b) (TEither a' b') = unify a a' >> unify b b'
      unify' (TFun a b) (TFun a' b')       = unify a a' >> unify b b'
      unify' (TList a) (TList a')          = unify a a'
      unify' (TMap k v) (TMap k' v')       = unify k k' >> unify v v'
      unify' (THandler a ps b) (THandler a' ps' b') = unify a a' >> unify ps ps' >> unify b b'
      unify' a b = throwError $ "Type mismatch: " ++ show a ++ " vs " ++ show b

instance Unifiable CompType where
  unify (TComp t e) (TComp t' e') = unify t t' >> unify e e'

instance Unifiable Arity where
  unify (Arity t1 t2) (Arity t1' t2') = unify t1 t1' >> unify t2 t2'

instance (Unifiable v, Substitutable v, Ord k) => Unifiable (Map.Map k v) where
  unify m1 m2 = mapM_ (uncurry unify) (Map.intersectionWith (,) m1 m2)

instance Unifiable EffectsType where
  unify r1 r2 = do
    r1' <- applySubst r1
    r2' <- applySubst r2
    unifyEffects r1' r2'

unifyEffects :: EffectsType -> EffectsType -> Infer ()
unifyEffects (Closed m1) (Closed m2) =
  mapM_ (uncurry unify) (Map.intersectionWith (,) m1 m2)
unifyEffects (Open m1 r1) (Closed m2) = do
  mapM_ (uncurry unify) (Map.intersectionWith (,) m1 m2)
  bindEffects r1 (Closed (m2 Map.\\ m1))
unifyEffects (Closed m1) (Open m2 r2) = do
  mapM_ (uncurry unify) (Map.intersectionWith (,) m1 m2)
  bindEffects r2 (Closed (m1 Map.\\ m2))
unifyEffects (Open m1 r1) (Open m2 r2)
  | r1 == r2  = mapM_ (uncurry unify) (Map.intersectionWith (,) m1 m2)
  | otherwise = do
      mapM_ (uncurry unify) (Map.intersectionWith (,) m1 m2)
      r3 <- freshEffects
      bindEffects r1 (addEffectOps (m2 Map.\\ m1) r3)
      bindEffects r2 (addEffectOps (m1 Map.\\ m2) r3)

bindEffects :: Ident -> EffectsType -> Infer ()
bindEffects u e
  | e == Open mempty u = return ()
  | u `Set.member` free EV e = throwError $ "Occurs check fails: " ++ u ++ " in " ++ show e
  | otherwise = extendSubst (Effect (Map.singleton u e))

bind :: Ident -> ValueType -> Infer ()
bind u t
  | t == TVar u = return ()
  | u `Set.member` free TV t = throwError $ "Occurs check fails: " ++ u ++ " in " ++ show t
  | otherwise = extendSubst (Type (Map.singleton u t))
