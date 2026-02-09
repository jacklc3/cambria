module Inference.Substitutable where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set

import Types

type Subst = Map.Map Ident ValueType

data Variable = Types | Params deriving (Show)

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
  apply :: Variable -> Subst -> a -> a
  free  :: Variable -> a -> Set.Set Ident

instance Substitutable ValueType where
  apply _ _ TUnit             = TUnit
  apply _ _ TInt              = TInt
  apply _ _ TBool             = TBool
  apply _ _ TDouble           = TDouble
  apply _ _ TString           = TString
  apply _ _ TUnique           = TUnique
  apply v s (TPair t1 t2)     = TPair (apply v s t1) (apply v s t2)
  apply v s (TEither t1 t2)   = TEither (apply v s t1) (apply v s t2)
  apply v s (TFun t1 t2)      = TFun (apply v s t1) (apply v s t2)
  apply v s (THandler t1 t2)  = THandler (apply v s t1) (apply v s t2)
  apply Types s t@(TVar a)    = Map.findWithDefault t a s
  apply Params _ (TVar a)     = TVar a
  apply Types _ (TParam p)    = TParam p
  apply Params s t@(TParam p) = Map.findWithDefault t p s

  free v (TPair t1 t2)       = free v t1 <> free v t2
  free v (TEither t1 t2)     = free v t1 <> free v t2
  free v (TFun t1 t2)        = free v t1 <> free v t2
  free v (THandler t1 t2)    = free v t1 <> free v t2
  free Types (TVar a)        = Set.singleton a
  free Params (TParam p)     = Set.singleton p
  free _ _                   = mempty

instance Substitutable Arity where
  apply v s (Arity t1 t2)    = Arity (apply v s t1) (apply v s t2)
  free v (Arity t1 t2)       = free v t1 <> free v t2

instance (Substitutable v) => Substitutable (Map.Map k v) where
  apply v s                  = Map.map (apply v s)
  free v                     = foldMap (free v)

instance Substitutable CompType where
  apply v s (TComp t es)     = TComp (apply v s t) (apply v s es)
  free v (TComp t es)        = free v t <> free v es
