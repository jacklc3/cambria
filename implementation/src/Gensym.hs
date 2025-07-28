module Gensym (
      Gensym(..)
    , gensym
    , SymbolGen
    , runSymbolGen
    , SymbolGenT
    , runSymbolGenT
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict

class Gensym s where
    genzero :: s
    nextsym :: s -> s

newtype SymbolGenT s m a = SymbolGenT { unSymbolGenT :: StateT s m a }

runSymbolGenT :: (Gensym s, Monad m) => SymbolGenT s m a -> m a
runSymbolGenT = flip evalStateT genzero . unSymbolGenT

type SymbolGen s = SymbolGenT s Identity

runSymbolGen :: (Gensym s) => SymbolGen s a -> a
runSymbolGen = runIdentity . runSymbolGenT

gensym :: (Gensym s, Monad m) => SymbolGenT s m s
gensym = SymbolGenT $ do
    sym <- get
    modify nextsym
    return sym

instance Gensym Integer where
    genzero = 0
    nextsym = succ

instance (Monad m) => Functor (SymbolGenT s m) where
    fmap = liftM

instance (Monad m) => Applicative (SymbolGenT s m) where
    pure = SymbolGenT . return
    (<*>) = ap

instance (Monad m) => Monad (SymbolGenT s m) where
    x >>= k = SymbolGenT $ unSymbolGenT x >>= unSymbolGenT . k

instance MonadTrans (SymbolGenT s) where
    lift = SymbolGenT . lift

instance (MonadIO m) => MonadIO (SymbolGenT s m) where
    liftIO = lift . liftIO
