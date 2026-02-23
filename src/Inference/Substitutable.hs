module Inference.Substitutable where

import Control.Monad.State (gets, modify)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Types
import Inference.Context (Scheme(..))
import Inference.Monad

data VariableKind = TV | PV | EV

data Subst
  = Type      (Map.Map Ident ValueType)
  | Parameter (Map.Map Ident ValueType)
  | Effect    (Map.Map Ident EffectsType)

extendSubst :: Subst -> Infer ()
extendSubst (Type s')     = modify (\st ->
  st { typeSubst = Map.map (apply (Type s')) (typeSubst st) <> s' })
extendSubst (Effect s')   = modify (\st ->
  st { effectSubst  = Map.map (apply (Effect s'))  (effectSubst st)  <> s' })
extendSubst (Parameter _) =
  error "Parameter substitutions should not be added to state"

applySubst :: Substitutable a => a -> Infer a
applySubst t = do
  ts <- gets typeSubst
  es <- gets effectSubst
  return (apply (Effect es) (apply (Type ts) t))

class Substitutable a where
  apply :: Subst -> a -> a
  free  :: VariableKind -> a -> Set.Set Ident

instance Substitutable ValueType where
  apply (Type s) (TVar a)      = Map.findWithDefault (TVar a) a s
  apply (Parameter s) (TParam p) = Map.findWithDefault (TParam p) p s
  apply s (TPair t1 t2)        = TPair (apply s t1) (apply s t2)
  apply s (TEither t1 t2)      = TEither (apply s t1) (apply s t2)
  apply s (TFun t1 t2)         = TFun (apply s t1) (apply s t2)
  apply s (TList a)            = TList (apply s a)
  apply s (TMap k t)           = TMap (apply s k) (apply s t)
  apply s (THandler t1 ps t2)  = THandler (apply s t1) (apply s ps) (apply s t2)
  apply _ t                    = t

  free k (TPair t1 t2)         = free k t1 <> free k t2
  free k (TEither t1 t2)       = free k t1 <> free k t2
  free k (TFun t1 t2)          = free k t1 <> free k t2
  free k (TList a)             = free k a
  free k (TMap k' t)           = free k k' <> free k t
  free k (THandler t1 ps t2)   = free k t1 <> free k ps <> free k t2
  free TV  (TVar a)            = Set.singleton a
  free PV (TParam p)           = Set.singleton p
  free _ _                     = mempty

instance Substitutable Arity where
  apply s (Arity t1 t2)        = Arity (apply s t1) (apply s t2)
  free k (Arity t1 t2)         = free k t1 <> free k t2

instance (Substitutable v) => Substitutable (Map.Map k v) where
  apply s                      = Map.map (apply s)
  free k                       = foldMap (free k)

instance Substitutable EffectsType where
  apply s (Closed m)           = Closed (apply s m)
  apply (Effect s) (Open m r)  = case Map.lookup r s of
    Just (Closed m')  -> Closed (apply (Effect s) m <> m')
    Just (Open m' r') -> Open (apply (Effect s) m <> m') r'
    Nothing           -> Open (apply (Effect s) m) r
  apply s (Open m r)           = Open (apply s m) r

  free k  (Closed m)           = free k m
  free EV (Open m r)           = Set.insert r (free EV m)
  free k  (Open m _)           = free k m

instance Substitutable CompType where
  apply s (TComp t e)          = TComp (apply s t) (apply s e)
  free k (TComp t e)           = free k t <> free k e

instance Substitutable Scheme where
  apply (Type m) (Forall tvs evs t)   = Forall tvs evs (apply (Type (m `Map.withoutKeys` tvs)) t)
  apply (Effect m) (Forall tvs evs t) = Forall tvs evs (apply (Effect (m `Map.withoutKeys` evs)) t)
  apply s (Forall tvs evs t)          = Forall tvs evs (apply s t)
  free TV (Forall tvs _ t)            = free TV t Set.\\ tvs
  free EV (Forall _ evs t)            = free EV t Set.\\ evs
  free k (Forall _ _ t)               = free k t
