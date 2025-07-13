module Ast where

import Data.Map (Map)
import Data.List (intersperse)

type VarName = String
type OpName  = String

data Value
    = VInt Integer
    | VBool Bool
    | VString String
    | VUnit
    | VPair Value Value
    | VVar VarName
    | VFun VarName Computation
    -- | VRecFun (Map VarName Value) VarName VarName Computation -- rec fun f x -> body
    | VHandler Handler
      -- Runtime-only values
    | VContinuation (Value -> Computation)

instance Show Value where
    show (VInt i)          = show i
    show (VBool b)         = show b
    show (VString s)       = show s
    show VUnit             = "()"
    show (VPair v1 v2)     = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
    show (VVar v)          = "Var:" ++  v
    show (VFun x c)        = "(fun " ++ x ++ " -> " ++ show c ++ ")"
    -- show (VRecFun _ f x c) = "(rec fun " ++ f ++ " " ++ x ++ " -> " ++ show c ++ ")"
    show (VHandler _)      = "<handler>"
    show (VContinuation _) = "<continuation>"

instance Eq Value where
    (VInt i1) == (VInt i2) = i1 == i2
    (VBool b1) == (VBool b2) = b1 == b2
    (VString s1) == (VString s2) = s1 == s2
    VUnit == VUnit = True
    (VPair v1 v2) == (VPair v3 v4) = v1 == v3 && v2 == v4
    (VVar n1) == (VVar n2) = n1 == n2
    _ == _ = False

data Computation
    = CReturn Value
    | COp OpName Value VarName Computation -- op(v; y, c)
    | CSeq VarName Computation Computation -- do x <- c1 in c2
    | CIf Value Computation Computation
    | CApp Value Value
    | CHandle Handler Computation

instance Show Computation where
    show (CReturn v)      = "return " ++ show v
    show (COp op v y c)   = op ++ "(" ++ show v ++ "; " ++ y ++ ", " ++ show c ++ ")"
    show (CSeq x c1 c2)   = "do " ++ x ++ " <- " ++ show c1 ++ " in " ++ show c2
    show (CIf v c1 c2)    = "if " ++ show v ++ " then " ++ show c1 ++ " else " ++ show c2
    show (CApp v1 v2)     = show v1 ++ " " ++ show v2
    show (CHandle h c)    = "with " ++ show h ++ " handle " ++ show c

data Handler = Handler {
    hReturnClause :: (VarName, Computation),
    hOpClauses    :: [(OpName, VarName, VarName, Computation)]
}

instance Show Handler where
  show (Handler (xr, cr) opCs) =
    let retStr = "return " ++ xr ++ " -> " ++ show cr
        opStrs = map (\(op, x, k, c) -> op ++ "(" ++ x ++ "; " ++ k ++ ") -> " ++ show c) opCs
        allClauses = if null retStr then opStrs else retStr : opStrs
    in "handler { " ++ (concat $ intersperse ", " allClauses) ++ " }"
