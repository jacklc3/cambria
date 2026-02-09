module Types where

import Data.List (intercalate)
import qualified Data.Map as Map

type Ident = String
type Op    = String

data ValueType
  = TVar Ident
  | TParam Ident
  | TUnit
  | TInt
  | TBool
  | TDouble
  | TString
  | TUnique
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
  arg  :: ValueType,
  ret :: ValueType
} deriving (Eq)

instance Show ValueType where
  show = showType 0

showType :: Int -> ValueType -> String
showType _ (TVar a) = a
showType _ (TParam p) = "$" ++ p
showType _ TUnit = "Unit"
showType _ TInt = "Int"
showType _ TBool = "Bool"
showType _ TDouble = "Double"
showType _ TString = "Str"
showType _ TUnique = "Unique"
showType p (TPair t1 t2) = parensIf (p > 3) $ showType 4 t1 ++ " & " ++ showType 4 t2
showType p (TEither t1 t2) = parensIf (p > 2) $ showType 3 t1 ++ " + " ++ showType 3 t2
showType p (TFun t1 t2) = parensIf (p > 1) $ showType 2 t1 ++ " -> " ++ show t2
showType p (THandler t1 t2) = parensIf (p > 0) $ show t1 ++ " => " ++ show t2

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++ ")"
parensIf False s = s

instance Show CompType where
  show (TComp t es) = showType 4 t ++ "!" ++ showEffects es
    where
      showEffects es = "{" ++
        intercalate "," (Map.foldrWithKey (\op ar acc ->
          (" " ++ op ++ " : " ++ show ar ++ " ") : acc) [] es) ++ "}"

instance Show Arity where
  show (Arity arg ret) = show arg ++ " ~> " ++ show ret
