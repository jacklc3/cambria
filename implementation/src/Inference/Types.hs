module Inference.Types where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax (Ident, Op)

type Infer a = ReaderT Context (StateT Int (Except String)) a

runInfer :: Context -> Infer a -> Either String a
runInfer ctx m = runExcept (evalStateT (runReaderT m ctx) 0)

type Effects = Map.Map Op Arity

data Arity = Arity Type Type deriving (Eq, Show)

data Type
  = TVar Ident
  | TUnit
  | TInt
  | TBool
  | TDouble
  | TString
  | TName
  | TPair Type Type
  | TEither Type Type
  | TFun Type Type Effects
  | THandler Type Effects Type Effects
  deriving (Eq)

instance Show Type where
  show (TVar a) = a
  show TInt = "Int"
  show TBool = "Bool"
  show TString = "Str"
  show TUnit = "Unit"
  show (TPair t1 t2) = show t1 ++ " x " ++ show t2
  show (TEither t1 t2) = show t1 ++ " + " ++ show t2
  show (TFun t1 t2 e2) = show t1 ++ " -> " ++ show t2 ++ "!" ++ show e2
  show (THandler t1 e1 t2 e2) = show t1 ++ "!" ++ show e1 ++ " => " ++ show t2 ++ "!" ++ show e2

data Scheme = Forall (Set.Set Ident) Type
  deriving (Eq, Show)

data Context = Context {
  vars    :: Map.Map Ident Scheme,
  effects :: Effects
} deriving (Show)
