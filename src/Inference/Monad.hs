module Inference.Monad where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

import Types (Ident, Op, Arity, ValueType(TVar), EffectsType(Open))
import Inference.Context (Context)

data InferState = InferState {
  count       :: Int,
  typeSubst   :: Map.Map Ident ValueType,
  effectSubst :: Map.Map Op EffectsType
}

type Infer a = ReaderT Context (StateT InferState (Except String)) a

fresh :: Infer ValueType
fresh = do
  n <- gets count
  modify (\st -> st { count = succ n })
  return $ TVar $ "t" ++ show n

freshEffects :: Map.Map Op Arity -> Infer EffectsType
freshEffects ops = do
  n <- gets count
  modify (\st -> st { count = succ n })
  return $ Open ops ("e" ++ show n)

runInfer :: Context -> Infer a -> Either String a
runInfer ctx m = runExcept (evalStateT (runReaderT m ctx) (InferState 0 mempty mempty))
