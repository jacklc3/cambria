module Syntax where

import Data.List (intersperse)
import Data.Map (Map)
import Data.Unique (Unique, hashUnique)

type Ident = String
type Op    = String
type Name  = Unique
data Env   = Env (Map Ident Value)
data Side  = L | R

data BaseType
  = BTUnit
  | BTInt
  | BTBool
  | BTDouble
  | BTString
  | BTName
  | BTPair BaseType BaseType
  | BTEither BaseType BaseType
  deriving (Show, Eq)

data Value
  = VInt Integer
  | VDouble Double
  | VBool Bool
  | VString String
  | VUnit
  | VPair Value Value
  | VEither Side Value
  | VHandler Handler
    -- Parsetime-only values
  | VVar Ident
  | VFun Ident Computation
  | VRec Ident Ident Computation
    -- Runtime-only values
  | VName Name
  | VPrimitive (Value -> Computation)
  | VClosure Ident Computation Env

instance Show Value where
  show (VInt i)         = show i
  show (VDouble n)      = show n
  show (VBool b)        = show b
  show (VString s)      = show s
  show VUnit            = "()"
  show (VName a)        = "<a" ++ show (hashUnique a) ++ ">"
  show (VPair v1 v2)    = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
  show (VEither L v)    = "inl " ++ show v
  show (VEither R v)    = "inr " ++ show v
  show (VHandler h)     = show h
  show (VVar v)         = v
  show (VFun x c)       = "(fun " ++ x ++ " -> " ++ show c ++ ")"
  show (VRec f x c)     = "(rec " ++ f ++ " " ++ x ++ " -> " ++ show c ++ ")"
  show (VPrimitive _)   = "<primitive>"
  show (VClosure _ _ _) = "<closure>"

instance Eq Value where
  (VInt i1) == (VInt i2)           = i1 == i2
  (VDouble n1) == (VDouble n2)     = n1 == n2
  (VBool b1) == (VBool b2)         = b1 == b2
  (VString s1) == (VString s2)     = s1 == s2
  VUnit == VUnit                   = True
  (VName a1) == (VName a2)         = a1 == a2
  (VPair v1 v2) == (VPair v3 v4)   = v1 == v3 && v2 == v4
  (VEither L v1) == (VEither L v2) = v1 == v2
  (VEither R v1) == (VEither R v2) = v1 == v2
  (VVar x1) == (VVar x2)           = x1 == x2
  _ == _                           = False

data Computation
  = CReturn Value
  | COp Op Value
  | CDo Ident Computation Computation
  | CIf Value Computation Computation
  | CCase Value Ident Computation Ident Computation
  | CApp Value Value
  | CHandle Value Computation
  | CDeclare Op BaseType BaseType Computation

instance Show Computation where
  show (CReturn v)    = "return " ++ show v
  show (COp op v)     = "!" ++ op ++ " " ++ show v
  show (CDo x c1 c2) = "do " ++ x ++ " <- " ++ show c1 ++ " in " ++ show c2
  show (CIf v c1 c2)  = "if " ++ show v ++ " then " ++ show c1 ++ " else " ++ show c2
  show (CCase v x1 c1 x2 c2) =
    "case " ++ show v ++ " of { inl " ++ show x1 ++ " -> " ++ show c1 ++ ", inr "
    ++ show x2 ++ " -> " ++ show c2 ++ " }"
  show (CApp v1 v2)   = show v1 ++ " " ++ show v2
  show (CHandle h c)  = "with " ++ show h ++ " handle " ++ show c
  show (CDeclare op tArg tRet c) = "declare !" ++ op ++ " : " ++ show tArg ++ " ~> " ++ show tRet ++ " in " ++ show c

data RetClause = RetClause Ident Computation
data OpClause = OpClause Ident Ident Computation
data FinClause = FinClause Ident Computation
data Handler = Handler {
  retClause :: RetClause,
  opClauses :: [(Op, OpClause)],
  finClause :: Maybe FinClause
}

instance Show Handler where
  show (Handler (RetClause xr cr) ocs fc) =
    let
      retStr = "return " ++ xr ++ " -> " ++ show cr
      opStrs = map (\(op, OpClause x k c) ->
        op ++ "(" ++ x ++ "; " ++ k ++ ") -> " ++ show c) ocs
      finStr = case fc of
        Just (FinClause xf cf) -> ", finally " ++ xf ++ " -> " ++ show cf
        Nothing -> ""
    in
      "handler { " ++ (concat $ intersperse ", " (retStr : opStrs)) ++ finStr ++ " }"
