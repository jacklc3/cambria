module Types where

import Data.List (intercalate)
import qualified Data.Map as Map

type Ident = String
type Op    = String

type ParamSubst = Map.Map Ident ValueType

data Subst
  = Type      (Map.Map Ident ValueType)
  | Parameter (Map.Map Ident ValueType)
  | Effect    (Map.Map Ident EffectsType)

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
  | THandler CompType ParamSubst CompType
  | TMap ValueType ValueType
  | TList ValueType
  deriving (Eq)

data CompType = TComp {
  value   :: ValueType,
  effects :: EffectsType
} deriving (Eq)

data EffectsType
  = Closed (Map.Map Op Arity)
  | Open   (Map.Map Op Arity) Ident
  deriving (Eq)

data Arity = Arity {
  arg :: ValueType,
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
showType _ (TMap k v) = "Map " ++ showType 5 k ++ " " ++ showType 5 v
showType _ (TList t) = "List " ++ showType 5 t
showType p (THandler t1 ps t2)
  | Map.null ps = parensIf (p > 0) $ show t1 ++ " => " ++ show t2
  | otherwise   = parensIf (p > 0) $ show t1 ++ " =[" ++ showParamSubst ps ++ "]=> " ++ show t2
  where
    showParamSubst = intercalate ", " . Map.foldrWithKey (\k v acc -> (k ++ ": " ++ show v) : acc) []

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++ ")"
parensIf False s = s

instance Show CompType where
  show (TComp t e) = showType 4 t ++ "!" ++ show e

instance Show EffectsType where
  show (Closed es) = "{" ++ showOps es ++ "}"
  show (Open es r)
    | Map.null es = "{" ++ r ++ "}"
    | otherwise  = "{" ++ showOps es ++ " | " ++ r ++ "}"

showOps :: Map.Map Op Arity -> String
showOps es = intercalate "," (Map.elems format)
  where
    format = Map.mapWithKey (\op ar -> " " ++ op ++ " : " ++ show ar ++ " ") es

instance Show Arity where
  show (Arity arg ret) = show arg ++ " ~> " ++ show ret
