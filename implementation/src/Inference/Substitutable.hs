{-# LANGUAGE LambdaCase #-}

module Inference.Substitutable where

import Inference.Types
import qualified Data.Map as Map
import qualified Data.Set as Set

data Subst = Subst (Map.Map TVar TValue) (Map.Map EVar EffectSet)

emptySubst :: Subst
emptySubst = Subst Map.empty Map.empty

compose :: Subst -> Subst -> Subst
compose s2@(Subst tmap2 emap2) s1@(Subst tmap1 emap1) =
  Subst (Map.map (apply s2) tmap1 `Map.union` tmap2)
        (Map.map (apply s2) emap1 `Map.union` emap2)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> (Set.Set TVar, Set.Set EVar)

instance Substitutable Type where
  apply s (VType t) = VType (apply s t)
  apply s (CType t) = CType (apply s t)
  ftv (VType t) = ftv t
  ftv (CType t) = ftv t

instance Substitutable TValue where
  apply s@(Subst tmap _) t = case t of
    TVar a -> Map.findWithDefault t a tmap
    TPair t1 t2 -> TPair (apply s t1) (apply s t2)
    TEither t1 t2 -> TEither (apply s t1) (apply s t2)
    TFun t1 t2 -> TFun (apply s t1) (apply s t2)
    THandler c1 c2 -> THandler (apply s c1) (apply s c2)
    _ -> t
  ftv = \case
    TVar a -> (Set.singleton a, Set.empty)
    TPair t1 t2 -> ftv t1 <> ftv t2
    TEither t1 t2 -> ftv t1 <> ftv t2
    TFun t1 t2 -> ftv t1 <> ftv t2
    THandler c1 c2 -> ftv c1 <> ftv c2
    _ -> mempty

instance Substitutable TComp where
  apply s (TComp t e) = TComp (apply s t) (apply s e)
  ftv (TComp t e) = ftv t <> ftv e

instance Substitutable EffectSet where
  apply s@(Subst _ emap) (EffectSet ops (Just ev)) =
    case Map.lookup ev emap of
      Just (EffectSet ops' ev') -> EffectSet (ops `Set.union` ops') ev'
      Nothing -> EffectSet ops (Just ev)
  apply _ es = es
  ftv (EffectSet _ (Just ev)) = (Set.empty, Set.singleton ev)
  ftv _ = mempty

instance Substitutable Scheme where
  apply (Subst tmap emap) (Forall tvs evs t) =
    let s' = Subst (foldr Map.delete tmap tvs) (foldr Map.delete emap evs)
    in Forall tvs evs (apply s' t)
  ftv (Forall tvs evs t) =
    let (ftvs, fevs) = ftv t
    in (ftvs `Set.difference` Set.fromList tvs, fevs `Set.difference` Set.fromList evs)

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)
  ftv l = mconcat (map ftv l)
