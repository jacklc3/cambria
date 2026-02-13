module Inference.Monad where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

import Types (Ident, ValueType(TVar), EffectsType(Open))
import Inference.Context (Context)

data InferState = InferState {
  count       :: Int,
  typeSubst   :: Map.Map Ident ValueType,
  effectSubst :: Map.Map Ident EffectsType
}

type Infer a = ReaderT Context (StateT InferState (Except String)) a

fresh :: Infer ValueType
fresh = do
  count <- gets count
  modify (\st ->  st { count = succ count } )
  return $ TVar $ "t" ++ show count

freshEffects :: Infer EffectsType
freshEffects = do
  c <- gets count
  modify (\st -> st { count = succ c })
  return $ Open mempty ("e" ++ show c)

runInfer :: Context -> Infer a -> Either String a
runInfer ctx m = runExcept (evalStateT (runReaderT m ctx) (InferState 0 mempty mempty))
