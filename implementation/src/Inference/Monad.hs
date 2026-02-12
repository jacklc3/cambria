module Inference.Monad where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

import Types (Ident, ValueType, EffectsType)
import Inference.Context (Context)

data InferState = InferState {
  count       :: Int,
  typeSubst   :: Map.Map Ident ValueType,
  effectSubst :: Map.Map Ident EffectsType
}

type Infer a = ReaderT Context (StateT InferState (Except String)) a

runInfer :: Context -> Infer a -> Either String a
runInfer ctx m = runExcept (evalStateT (runReaderT m ctx) (InferState 0 mempty mempty))
