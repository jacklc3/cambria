module Parsing.SugaredSyntax where

import Syntax (Ident, Op, Side(..))

data SugaredExpr
  = SEVar Ident
  | SEUnit
  | SEBool Bool
  | SEInt Integer
  | SEString String
  | SEPair SugaredExpr SugaredExpr
  | SEEither Side SugaredExpr
  | SEHandler [HandlerClause]
  | SEFun [Ident] SugaredComp
  | SERec Ident [Ident] SugaredComp
  | SEComp SugaredComp
  deriving (Show)

data SugaredComp
  = SCReturn SugaredExpr
  | SCOp Op SugaredExpr
  | SCDo Ident SugaredComp SugaredComp
  | SCIf SugaredExpr SugaredComp SugaredComp
  | SCCase SugaredExpr (Ident, SugaredComp) (Ident, SugaredComp)
  | SCApp SugaredExpr SugaredExpr
  | SCWith SugaredExpr SugaredComp
  deriving (Show)

data HandlerClause
  = RC Ident SugaredComp
  | OC Op Ident Ident SugaredComp
  | FC Ident SugaredComp
  deriving (Show)
