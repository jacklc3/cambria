module Inference.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax (Op)

type TVar = Int
type EVar = Int

data EffectSet = EffectSet (Set.Set Op) (Maybe EVar)
  deriving (Eq, Ord)

data TValue
  = TVar TVar
  | TInt
  | TBool
  | TString
  | TUnit
  | TPair TValue TValue
  | TEither TValue TValue
  | TFun TValue TComp
  | THandler TComp TComp
  deriving (Eq, Ord)

data TComp = TComp TValue EffectSet
  deriving (Eq, Ord)

data Type = VType TValue | CType TComp
  deriving (Eq, Ord)

data Scheme = Forall [TVar] [EVar] Type

instance Show EffectSet where
  show (EffectSet ops Nothing) = "{" ++ Set.foldr (\o acc -> o ++ "," ++ acc) "" ops ++ "}"
  show (EffectSet ops (Just ev)) = "{" ++ Set.foldr (\o acc -> o ++ "," ++ acc) "" ops ++ "|" ++ show ev ++ "}"

instance Show TValue where
  show (TVar a) = "t" ++ show a
  show TInt = "Int"
  show TBool = "Bool"
  show TString = "String"
  show TUnit = "()"
  show (TPair t1 t2) = show t1 ++ " x " ++ show t2
  show (TEither t1 t2) = show t1 ++ " + " ++ show t2
  show (TFun t1 t2) = show t1 ++ " -> " ++ show t2
  show (THandler c1 c2) = show c1 ++ " => " ++ show c2

instance Show TComp where
  show (TComp t e) = show t ++ "!" ++ show e

instance Show Type where
  show (VType t) = show t
  show (CType t) = show t

instance Show Scheme where
  show (Forall tvs evs t) =
    let
      tvarStr = if null tvs then "" else "forall " ++ unwords (map (\v -> "t" ++ show v) tvs)
      evarStr = if null evs then "" else "forall " ++ unwords (map (\v -> "e" ++ show v) evs)
    in tvarStr ++ " " ++ evarStr ++ ". " ++ show t
