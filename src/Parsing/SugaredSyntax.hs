module Parsing.SugaredSyntax where

import Types (Ident, Op, Arity, ValueType, CompType)
import Syntax (Side(..))

-- case clauses take any pattern; binding positions only the irrefutable subset
data Pattern
  = PVar Ident
  | PWild
  | PUnit
  | PPair Pattern Pattern
  | PEither Side Pattern
  | PNil
  | PCons Pattern Pattern
  | PInt Integer
  | PBool Bool
  | PString String

data Assoc = ALeft | ARight | ANone

data OpTarget = TargetVar Ident | TargetOp Op

data SugaredExpr
  = SEVar Ident
  | SEUnit
  | SEBool Bool
  | SEInt Integer
  | SEString String
  | SEPair SugaredExpr SugaredExpr
  | SEEither Side SugaredExpr
  | SEHandler [HandlerClause]
  | SEFun [Pattern] SugaredComp
  | SERec Ident [Pattern] SugaredComp
  | SEComp SugaredComp
  | SEAnnot SugaredExpr ValueType

data SugaredComp
  = SCReturn SugaredExpr
  | SCOp Op SugaredExpr
  | SCDo Pattern SugaredComp SugaredComp
  | SCIf SugaredExpr SugaredComp SugaredComp
  | SCMatch SugaredExpr [(Pattern, SugaredComp)]
  | SCApp SugaredExpr SugaredExpr
  | SCWith SugaredExpr [(String, ValueType)] SugaredComp
  | SCEffect Op Arity SugaredComp
  | SCAnnot SugaredComp CompType
  | SCLetRec [(Ident, [Pattern], SugaredComp)] SugaredComp
  | SCFixity Assoc Int String OpTarget SugaredComp
  | SCOpChain SugaredExpr [(String, SugaredExpr)]

data HandlerClause
  = RC Pattern SugaredComp
  | OC Op Pattern Ident SugaredComp
  | FC Pattern SugaredComp
