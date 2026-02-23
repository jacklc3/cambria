module Parsing.SugaredSyntax where

import Types (Ident, Op, Arity, ValueType(..))
import Syntax (Side(..))

data Pattern
  = PVar Ident
  | PWild
  | PPair Pattern Pattern

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

data SugaredComp
  = SCReturn SugaredExpr
  | SCOp Op SugaredExpr
  | SCDo Pattern SugaredComp SugaredComp
  | SCIf SugaredExpr SugaredComp SugaredComp
  | SCCase SugaredExpr (Pattern, SugaredComp) (Pattern, SugaredComp)
  | SCApp SugaredExpr SugaredExpr
  | SCWith SugaredExpr SugaredComp
  | SCDeclare Op Arity SugaredComp

data HandlerClause
  = RC Pattern SugaredComp
  | OC Op Pattern Ident SugaredComp
  | FC Pattern SugaredComp
  | TC String ValueType
