module Inference.Substitutable where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set

import Types

type Subst = Map.Map Ident ValueType

data InferState = InferState {
  count :: Int,
  subst :: Subst
}

type Infer a = ReaderT Context (StateT InferState (Except String)) a

runInfer :: Context -> Infer a -> Either String a
runInfer ctx m = runExcept (evalStateT (runReaderT m ctx) (InferState 0 mempty))

data Scheme = Forall (Set.Set Ident) ValueType
  deriving (Eq, Show)

data Context = Context {
  variables :: Map.Map Ident Scheme,
  abilities :: Effects
} deriving (Show)

class Substitutable a where
  apply       :: Subst -> a -> a
  ftv         :: a -> Set.Set Ident
  applyParams :: Subst -> a -> a
  ftp         :: a -> Set.Set Ident

instance Substitutable ValueType where
  apply _ (TParam p)       = TParam p
  apply _ TUnit            = TUnit
  apply _ TInt             = TInt
  apply _ TBool            = TBool
  apply _ TDouble          = TDouble
  apply _ TString          = TString
  apply _ TUnique          = TUnique
  apply s (TPair t1 t2)    = TPair (apply s t1) (apply s t2)
  apply s (TEither t1 t2)  = TEither (apply s t1) (apply s t2)
  apply s (TFun t1 t2)     = TFun (apply s t1) (apply s t2)
  apply s (THandler t1 t2) = THandler (apply s t1) (apply s t2)
  apply s t@(TVar a)       = Map.findWithDefault t a s

  ftv (TPair t1 t2)        = ftv t1 <> ftv t2
  ftv (TEither t1 t2)      = ftv t1 <> ftv t2
  ftv (TFun t1 t2)         = ftv t1 <> ftv t2
  ftv (THandler t1 t2)     = ftv t1 <> ftv t2
  ftv (TVar a)             = Set.singleton a
  ftv _                    = mempty

  applyParams _ (TVar a)         = TVar a
  applyParams _ TUnit            = TUnit
  applyParams _ TInt             = TInt
  applyParams _ TBool            = TBool
  applyParams _ TDouble          = TDouble
  applyParams _ TString          = TString
  applyParams _ TUnique            = TUnique
  applyParams s (TPair t1 t2)    = TPair (applyParams s t1) (applyParams s t2)
  applyParams s (TEither t1 t2)  = TEither (applyParams s t1) (applyParams s t2)
  applyParams s (TFun t1 t2)     = TFun (applyParams s t1) (applyParams s t2)
  applyParams s (THandler t1 t2) = THandler (applyParams s t1) (applyParams s t2)
  applyParams s t@(TParam p)     = Map.findWithDefault t p s

  ftp (TPair t1 t2)        = ftp t1 <> ftp t2
  ftp (TEither t1 t2)      = ftp t1 <> ftp t2
  ftp (TFun t1 t2)         = ftp t1 <> ftp t2
  ftp (THandler t1 t2)     = ftp t1 <> ftp t2
  ftp (TParam p)           = Set.singleton p
  ftp _                    = mempty

instance Substitutable Arity where
  apply s (Arity t1 t2)       = Arity (apply s t1) (apply s t2)
  ftv (Arity t1 t2)           = ftv t1 <> ftv t2
  applyParams s (Arity t1 t2) = Arity (applyParams s t1) (applyParams s t2)
  ftp (Arity t1 t2)           = ftp t1 <> ftp t2

instance (Substitutable v) => Substitutable (Map.Map k v) where
  apply s       = Map.map (apply s)
  ftv           = foldMap ftv
  applyParams s = Map.map (applyParams s)
  ftp           = foldMap ftp

instance Substitutable CompType where
  apply s (TComp t es)       = TComp (apply s t) (apply s es)
  ftv (TComp t es)           = ftv t <> ftv es
  applyParams s (TComp t es) = TComp (applyParams s t) (applyParams s es)
  ftp (TComp t es)           = ftp t <> ftp es
