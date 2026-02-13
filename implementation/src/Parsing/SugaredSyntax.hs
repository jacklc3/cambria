module Parsing.SugaredSyntax where

import Types (Ident, Op, ValueType(..))
import Syntax (Side(..))

data SugaredPattern
  = SPVar Ident
  | SPWild
  | SPPair SugaredPattern SugaredPattern

data SugaredExpr
  = SEVar Ident
  | SEUnit
  | SEBool Bool
  | SEInt Integer
  | SEString String
  | SEPair SugaredExpr SugaredExpr
  | SEEither Side SugaredExpr
  | SEHandler [HandlerClause]
  | SEFun [SugaredPattern] SugaredComp
  | SERec Ident [SugaredPattern] SugaredComp
  | SEComp SugaredComp

data SugaredComp
  = SCReturn SugaredExpr
  | SCOp Op SugaredExpr
  | SCDo SugaredPattern SugaredComp SugaredComp
  | SCIf SugaredExpr SugaredComp SugaredComp
  | SCCase SugaredExpr (Ident, SugaredComp) (Ident, SugaredComp)
  | SCApp SugaredExpr SugaredExpr
  | SCWith SugaredExpr SugaredComp
  | SCDeclare Op ValueType ValueType SugaredComp

data HandlerClause
  = RC SugaredPattern SugaredComp
  | OC Op SugaredPattern Ident SugaredComp
  | FC SugaredPattern SugaredComp
  | TC String ValueType
