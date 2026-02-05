{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inference.Types where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Semigroup (Semigroup)
import Data.Monoid (Monoid)

import Syntax (Ident, Op)

type Infer a = ReaderT Context (StateT Int (Except String)) a

runInfer :: Context -> Infer a -> Either String a
runInfer ctx m = runExcept (evalStateT (runReaderT m ctx) 0)

data ValueType
  = TVar Ident
  | TUnit
  | TInt
  | TBool
  | TDouble
  | TString
  | TName
  | TPair ValueType ValueType
  | TEither ValueType ValueType
  | TFun ValueType CompType
  | THandler CompType CompType
  deriving (Eq)

data CompType = TComp {
  value :: ValueType,
  effects :: Effects
} deriving (Eq)

type Effects = Map.Map Op Arity

data Arity = Arity {
  input  :: ValueType,
  output :: ValueType
} deriving (Eq)

instance Show ValueType where
  show = showType 0
    where
      showType :: Int -> ValueType -> String
      showType _ (TVar a) = a
      showType _ TUnit = "Unit"
      showType _ TInt = "Int"
      showType _ TBool = "Bool"
      showType _ TDouble = "Double"
      showType _ TString = "Str"
      showType _ TName = "Name"
      showType p (TPair t1 t2) = parensIf (p > 3) $ showType 4 t1 ++ " x " ++ showType 4 t2
      showType p (TEither t1 t2) = parensIf (p > 2) $ showType 3 t1 ++ " + " ++ showType 3 t2
      showType p (TFun t1 t2) = parensIf (p > 1) $ showType 2 t1 ++ " -> " ++ show t2
      showType p (THandler t1 t2) = parensIf (p > 0) $ show t1 ++ " => " ++ show t2

parensIf :: Bool -> String -> String
parensIf True s = "(" ++ s ++ ")"
parensIf False s = s

instance Show CompType where
  show (TComp t es) = show t ++ "!" ++ showEffects es
    where
      showEffects es = "{" ++
        intercalate "," (Map.foldrWithKey (\op ar acc ->
          (" " ++ op ++ " : " ++ show ar ++ " ") : acc) [] es) ++ "}"

instance Show Arity where
  show (Arity input output) = show input ++ " ~> " ++ show output

data Scheme = Forall (Set.Set Ident) ValueType
  deriving (Eq, Show)

data Context = Context {
  variables :: Map.Map Ident Scheme,
  abilities :: Effects
} deriving (Show)
