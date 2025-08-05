module Inference.Unify where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Ast (Op)
import Control.Monad.Except
import Inference.Types
import Inference.Substitutable

type UnifyError = String
type Unify a = Except UnifyError a

unify :: Type -> Type -> Unify Subst
unify (VType v1) (VType v2) = unifyVal v1 v2
unify (CType c1) (CType c2) = unifyComp c1 c2
unify t1 t2 = throwError $ "Cannot unify different kinds of types: " ++ show t1 ++ " and " ++ show t2

unifyVal :: TValue -> TValue -> Unify Subst
unifyVal t1 t2 | t1 == t2 = return emptySubst
unifyVal (TVar a) t = varBind a t
unifyVal t (TVar a) = varBind a t
unifyVal (TPair v1a v1b) (TPair v2a v2b) = do
  s1 <- unifyVal v1a v2a
  s2 <- unifyVal (apply s1 v1b) (apply s1 v2b)
  return $ s2 `compose` s1
unifyVal (TFun p1 r1) (TFun p2 r2) = do
  s1 <- unifyVal p1 p2
  s2 <- unifyComp (apply s1 r1) (apply s1 r2)
  return $ s2 `compose` s1
unifyVal (THandler c1a c1b) (THandler c2a c2b) = do
    s1 <- unifyComp c1a c2a
    s2 <- unifyComp (apply s1 c1b) (apply s1 c2b)
    return $ s2 `compose` s1
unifyVal t1 t2 = throwError $ "Cannot unify value types: " ++ show t1 ++ " and " ++ show t2

unifyComp :: TComp -> TComp -> Unify Subst
unifyComp (TComp v1 e1) (TComp v2 e2) = do
  s1 <- unifyVal v1 v2
  s2 <- unifyEffects (apply s1 e1) (apply s1 e2)
  return $ s2 `compose` s1

unifyEffects :: EffectSet -> EffectSet -> Unify Subst
unifyEffects e1 e2 | e1 == e2 = return emptySubst
unifyEffects (EffectSet ops1 (Just r1)) e2 = varBindEffect r1 (e2 `effectDiff` ops1)
unifyEffects e1 (EffectSet ops2 (Just r2)) = varBindEffect r2 (e1 `effectDiff` ops2)
unifyEffects e1 e2 = throwError $ "Cannot unify effects: " ++ show e1 ++ " and " ++ show e2

varBind :: TVar -> TValue -> Unify Subst
varBind u t
  | t == TVar u = return emptySubst
  | u `Set.member` fst (ftv t) = throwError $ "Occurs check failed: t" ++ show u ++ " in " ++ show t
  | otherwise = return $ Subst (Map.singleton u t) Map.empty

varBindEffect :: EVar -> EffectSet -> Unify Subst
varBindEffect u e
  | Just u == evarPart e && Set.null (opsPart e) = return emptySubst
  | Just u == evarPart e && not (Set.null (opsPart e)) = throwError "Recursive effect unification"
  | otherwise = return $ Subst Map.empty (Map.singleton u e)
  where
    opsPart (EffectSet o _) = o
    evarPart (EffectSet _ r) = r

effectDiff :: EffectSet -> Set.Set Op -> EffectSet
effectDiff (EffectSet ops r) toRemove = EffectSet (ops `Set.difference` toRemove) r
