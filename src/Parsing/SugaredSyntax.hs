module Parsing.SugaredSyntax where

import Types (Ident, Op, Arity, ValueType, CompType)
import Syntax (Side(..))

-- cases take any pattern; binding positions only the irrefutable subset
data Pattern
  = PVar Ident
  | PWild
  | PUnit
  | PPair Pattern Pattern
  | PEither Side Pattern
  | PNil
  | PCons Pattern Pattern
  | PInt Integer
  | PDouble Double
  | PBool Bool
  | PString String

irrefutable :: Pattern -> Bool
irrefutable (PVar _)      = True
irrefutable PWild         = True
irrefutable PUnit         = True
irrefutable (PPair p1 p2) = irrefutable p1 && irrefutable p2
irrefutable _             = False

data Assoc = ALeft | ARight | ANone
  deriving (Eq)

data Fixity = Fixity Assoc Int

data SugaredExpr
  = SEVar Ident
  | SEUnit
  | SEBool Bool
  | SEInt Integer
  | SEDouble Double
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
  | SCFixity Fixity Ident SugaredComp
  | SCOpChain SugaredExpr [(Ident, SugaredExpr)]

data HandlerClause
  = RC Pattern SugaredComp
  | OC Op Pattern Ident SugaredComp
  | FC Pattern SugaredComp
