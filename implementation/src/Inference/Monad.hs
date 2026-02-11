module Inference.Monad where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Inference.Context (Context)
import Inference.Substitutable (Subst)

data InferState = InferState {
  count :: Int,
  subst :: Subst
}

type Infer a = ReaderT Context (StateT InferState (Except String)) a

runInfer :: Context -> Infer a -> Either String a
runInfer ctx m = runExcept (evalStateT (runReaderT m ctx) (InferState 0 mempty))
