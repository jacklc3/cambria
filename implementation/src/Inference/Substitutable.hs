{-# LANGUAGE FlexibleInstances #-}

module Inference.Substitutable where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Inference.Types
import Syntax (Ident)

type Subst = Map.Map Ident ValueType

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set Ident

instance Substitutable ValueType where
  apply _ TUnit            = TUnit
  apply _ TInt             = TInt
  apply _ TBool            = TBool
  apply _ TDouble          = TDouble
  apply _ TString          = TString
  apply _ TName            = TName
  apply s (TPair t1 t2)    = TPair (apply s t1) (apply s t2)
  apply s (TEither t1 t2)  = TEither (apply s t1) (apply s t2)
  apply s (TFun t1 t2)     = TFun (apply s t1) (apply s t2)
  apply s (THandler t1 t2) = THandler (apply s t1) (apply s t2)
  apply s t@(TVar a)       = Map.findWithDefault t a s

  ftv TUnit                = Set.empty
  ftv TInt                 = Set.empty
  ftv TBool                = Set.empty
  ftv TDouble              = Set.empty
  ftv TString              = Set.empty
  ftv TName                = Set.empty
  ftv (TPair t1 t2)        = ftv t1 `Set.union` ftv t2
  ftv (TEither t1 t2)      = ftv t1 `Set.union` ftv t2
  ftv (TFun t1 t2)         = ftv t1 `Set.union` ftv t2
  ftv (THandler t1 t2)     = ftv t1 `Set.union` ftv t2
  ftv (TVar a)             = Set.singleton a

instance Substitutable CompType where
  apply s (TComp t es)     = TComp (apply s t) es
  ftv (TComp t _)          = ftv t

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as (apply (s `Map.withoutKeys` as) t)
  ftv (Forall as t) = ftv t `Set.difference` as

instance Substitutable Context where
  apply s (Context vs es) = Context (Map.map (apply s) vs) es
  ftv (Context vs _) = foldMap ftv vs
