module Ast where

import Data.Map (Map, toList)
import Data.List (intersperse)

type VarName = String
type OpName  = String
type Env = Map VarName Value
type Parameter = Integer
data Side = L | R deriving (Eq)

data Value
  = VInt Integer
  | VBool Bool
  | VString String
  | VUnit
  | VPair Value Value
  | VEither Side Value
  | VVar VarName
  | VFun VarName Computation
  | VHandler Handler
  | VPrimative (Value -> Computation)
  | VParameter Parameter
    -- Runtime-only values
  | VClosure VarName Computation Env

instance Show Value where
  show (VInt i)         = show i
  show (VBool b)        = show b
  show (VString s)      = show s
  show VUnit            = "()"
  show (VPair v1 v2)    = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
  show (VEither L v)    = "inl " ++ show v
  show (VEither R v)    = "inr " ++ show v
  show (VVar v)         = v
  show (VFun x c)       = "(fun " ++ x ++ " -> " ++ show c ++ ")"
  show (VHandler _)     = "<handler>"
  show (VPrimative _)   = "<primative>"
  show (VClosure _ _ _) = "<closure>"
  show (VParameter p)   = "<p" ++ show p ++ ">"

instance Eq Value where
  (VInt i1) == (VInt i2)             = i1 == i2
  (VBool b1) == (VBool b2)           = b1 == b2
  (VString s1) == (VString s2)       = s1 == s2
  VUnit == VUnit                     = True
  (VPair v1 v2) == (VPair v3 v4)     = v1 == v3 && v2 == v4
  (VEither s1 v1) == (VEither s2 v2) = s1 == s2 && v1 == v2
  (VVar x1) == (VVar x2)             = x1 == x2
  (VParameter p1) == (VParameter p2) = p1 == p2
  _ == _ = False

data Computation
  = CReturn Value
  | COp OpName Value
  | CDo VarName Computation Computation
  | CIf Value Computation Computation
  | CCase Value VarName Computation VarName Computation
  | CApp Value Value
  | CHandle Handler Computation

instance Show Computation where
  show (CReturn v)    = "return " ++ show v
  show (COp op v)     = op ++ "(" ++ show v ++ ")"
  show (CDo x c1 c2) = "do " ++ x ++ " <- " ++ show c1 ++ " in " ++ show c2
  show (CIf v c1 c2)  = "if " ++ show v ++ " then " ++ show c1 ++ " else " ++ show c2
  show (CCase v x1 c1 x2 c2) =
    "case " ++ show v ++ " of { inl " ++ show x1 ++ " -> " ++ show c1 ++ ", inr "
    ++ show x2 ++ " -> " ++ show c2 ++ " }"
  show (CApp v1 v2)   = show v1 ++ " " ++ show v2
  show (CHandle h c)  = "with " ++ show h ++ " handle " ++ show c

data RetClause = RetClause VarName Computation
data OpClause = OpClause VarName VarName Computation
data Handler = Handler {
  retClause :: RetClause,
  opClauses :: Map OpName OpClause
}

instance Show Handler where
  show (Handler (RetClause xr cr) opCs) =
    let retStr = "return " ++ xr ++ " -> " ++ show cr
        opStrs = map (\(op, OpClause x k c) ->
          op ++ "(" ++ x ++ "; " ++ k ++ ") -> " ++ show c) (toList opCs)
    in  "handler { " ++ (concat $ intersperse ", " (retStr : opStrs)) ++ " }"
