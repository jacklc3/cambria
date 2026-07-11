{
module Parsing.Parser (
  parse,
) where

import qualified Data.Map as Map

import Parsing.Token
import Parsing.Lexer
import Parsing.SugaredSyntax
import Types (Ident, Op, Arity(Arity), ValueType(..), CompType(..), EffectsType(..))
import Syntax (Side(..))

import Control.Monad.Except
}

%name comp
%tokentype { Token }
%monad { Except String } { (>>=) } { return }
%error { parseError }
%error.expected
%expect 2

%token
  fun                        { Token _ _ TokFun }
  rec                        { Token _ _ TokRec }
  let                        { Token _ _ TokLet }
  and                        { Token _ _ TokKwAnd }
  infixl                     { Token _ _ TokInfixl }
  infixr                     { Token _ _ TokInfixr }
  infix                      { Token _ _ TokInfix }
  handler                    { Token _ _ TokHandler }
  return                     { Token _ _ TokReturn }
  finally                    { Token _ _ TokFinally }
  do                         { Token _ _ TokDo }
  in                         { Token _ _ TokIn }
  if                         { Token _ _ TokIf }
  then                       { Token _ _ TokThen }
  else                       { Token _ _ TokElse }
  with                       { Token _ _ TokWith }
  handle                     { Token _ _ TokHandle }
  inl                        { Token _ _ TokInl }
  inr                        { Token _ _ TokInr }
  case                       { Token _ _ TokCase }
  of                         { Token _ _ TokOf }
  effect                     { Token _ _ TokEffect }
  Unit                       { Token _ _ TokTUnit }
  Void                       { Token _ _ TokTVoid }
  Int                        { Token _ _ TokTInt }
  Bool                       { Token _ _ TokTBool }
  Double                     { Token _ _ TokTDouble }
  Str                        { Token _ _ TokTString }
  Name                       { Token _ _ TokTName }
  Map                        { Token _ _ TokTMap }
  List                       { Token _ _ TokTList }

  '()'                       { Token _ _ TokUnit }
  '=='                       { Token _ _ TokEq }
  '->'                       { Token _ _ TokArrow }
  '<-'                       { Token _ _ TokLeftArrow }
  '+'                        { Token _ _ TokPlus }
  '-'                        { Token _ _ TokMinus }
  '*'                        { Token _ _ TokAsterisk }
  '/'                        { Token _ _ TokSlash }
  '('                        { Token _ _ TokLParen }
  ')'                        { Token _ _ TokRParen }
  '{'                        { Token _ _ TokLBrace }
  '}'                        { Token _ _ TokRBrace }
  '['                        { Token _ _ TokLBracket }
  ']'                        { Token _ _ TokRBracket }
  ','                        { Token _ _ TokComma }
  '_'                        { Token _ _ TokUnderscore }
  ';'                        { Token _ _ TokSemiColon }
  '++'                       { Token _ _ TokConcat }
  '~>'                       { Token _ _ TokSquigglyArrow }
  ':'                        { Token _ _ TokColon }
  '::'                       { Token _ _ TokCons }
  '[]'                       { Token _ _ TokNil }
  '.'                        { Token _ _ TokDot }
  '...'                      { Token _ _ TokEllipsis }
  '='                        { Token _ _ TokEquals }
  '!'                        { Token _ _ TokExclam }
  '=>'                       { Token _ _ TokFatArrow }
  '&&'                       { Token _ _ TokAnd }
  '||'                       { Token _ _ TokOr }
  '<'                        { Token _ _ TokLT }
  '>'                        { Token _ _ TokGT }
  '<='                       { Token _ _ TokLTE }
  '>='                       { Token _ _ TokGTE }
  '/='                       { Token _ _ TokNEq }

  integer                    { Token _ _ (TokInt $$) }
  boolean                    { Token _ _ (TokBool $$) }
  string                     { Token _ _ (TokString $$) }
  var                        { Token _ _ (TokIdent $$) }
  op                         { Token _ _ (TokOp $$) }
  symop                      { Token _ _ (TokSymOp $$) }
  typeparam                  { Token _ _ (TokTypeParam $$) }

%right ';'
%nonassoc APP

%%

comp :: { SugaredComp }
  : compTerm ';' comp                     { SCDo PWild $1 $3 }
  | compTerm %prec ';'                    { $1 }

expr :: { SugaredExpr }
  : value                                 { $1 }
  | comp                                  { SEComp $1 }
  | '(' expr ')'                          { $2 }
  | '(' expr ':' type ')'                 { SEAnnot $2 $4 }

exprApp :: { SugaredExpr }
  : var                                   { SEVar $1 }
  | compApp                               { SEComp $1 }
  | '(' expr ')'                          { $2 }
  | '(' expr ':' type ')'                 { SEAnnot $2 $4 }

exprAtom :: { SugaredExpr }
  : atom                                  { $1 }
  | '(' expr ')'                          { $2 }
  | '(' expr ':' type ')'                 { SEAnnot $2 $4 }

wildvar :: { Ident }
  : var                                   { $1 }
  | '_'                                   { "_" }

-- irrefutable, for binding positions
bpattern :: { Pattern }
  : var                                   { PVar $1 }
  | '_'                                   { PWild }
  | '()'                                  { PUnit }
  | '(' bpattern ',' bpattern ')'         { PPair $2 $4 }
  | '(' bpattern ')'                      { $2 }

bpatterns :: { [Pattern] }
  : bpatterns bpattern                    { $2 : $1 }
  | bpattern                              { [$1] }

-- full patterns, for case clauses
pattern :: { Pattern }
  : patApp '::' pattern                   { PCons $1 $3 }
  | patApp                                { $1 }

patApp :: { Pattern }
  : inl patAtom                           { PEither L $2 }
  | inr patAtom                           { PEither R $2 }
  | patAtom                               { $1 }

patAtom :: { Pattern }
  : var                                   { PVar $1 }
  | '_'                                   { PWild }
  | '()'                                  { PUnit }
  | '[]'                                  { PNil }
  | integer                               { PInt $1 }
  | boolean                               { PBool $1 }
  | string                                { PString $1 }
  | '(' pattern ',' pattern ')'           { PPair $2 $4 }
  | '(' pattern ')'                       { $2 }

value :: { SugaredExpr }
  : atom                                  { $1 }
  | inl exprAtom                          { SEEither L $2 }
  | inr exprAtom                          { SEEither R $2 }
  | fun bpatterns '->' comp               { SEFun (reverse $2) $4 }
  | rec wildvar bpatterns '->' comp       { SERec $2 (reverse $3) $5 }
  | handler '{' handlerClauses '}'        { SEHandler $3 }

atom :: { SugaredExpr }
  : '()'                                  { SEUnit }
  | '[]'                                  { SEVar "[]" }
  | boolean                               { SEBool $1 }
  | integer                               { SEInt $1 }
  | string                                { SEString $1 }
  | '(' expr ',' expr ')'                 { SEPair $2 $4 }
  | var                                   { SEVar $1 }

handlerClauses :: { [HandlerClause] }
  : handlerClauses ',' handlerClause      { $3 : $1 }
  | handlerClause                         { [$1] }
  | {- empty -}                           { [] }

handlerClause :: { HandlerClause }
  : return bpattern '->' comp             { RC $2 $4 }
  | var bpattern wildvar '->' comp        { OC $1 $2 $3 $5 }
  | finally bpattern '->' comp            { FC $2 $4 }

pSubs :: { [(String, ValueType)] }
  : {- empty -}                           { [] }
  | '[' pSubsList ']'                     { $2 }

pSubsList :: { [(String, ValueType)] }
  : typeparam '->' type                   { [($1, $3)] }
  | typeparam '->' type ',' pSubsList     { ($1, $3) : $5 }

compTerm :: { SugaredComp }
  : return expr                           { SCReturn $2 }
  | do bpattern '<-' comp in compTerm     { SCDo $2 $4 $6 }
  | let letBinding in compTerm            { mkLet $2 $4 }
  | let rec recBindings in compTerm       { mkLetRec (reverse $3) $5 }
  | if expr then comp else compTerm       { SCIf $2 $4 $6 }
  | case expr of '{' matchClauses '}'     { SCMatch $2 (reverse $5) }
  | with pSubs expr handle compTerm       { SCWith $3 $2 $5 }
  | effect op ':' type '~>' type '.' compTerm  { SCEffect $2 (Arity $4 $6) $8 }
  | infixl integer chainOp '=' opTarget '.' compTerm { SCFixity ALeft (fromInteger $2) $3 $5 $7 }
  | infixr integer chainOp '=' opTarget '.' compTerm { SCFixity ARight (fromInteger $2) $3 $5 $7 }
  | infix integer chainOp '=' opTarget '.' compTerm  { SCFixity ANone (fromInteger $2) $3 $5 $7 }
  | compInfix                             { $1 }

letBinding :: { (Pattern, SugaredExpr) }
  : bpattern '=' expr                     { ($1, $3) }
  | var bpatterns '=' expr                { (PVar $1, SEFun (reverse $2) (exprToComp $4)) }

recBindings :: { [(Ident, [Pattern], SugaredComp)] }
  : recBindings and recBinding            { $3 : $1 }
  | recBinding                            { [$1] }

recBinding :: { (Ident, [Pattern], SugaredComp) }
  : var bpatterns '=' expr                { ($1, reverse $2, exprToComp $4) }

matchClauses :: { [(Pattern, SugaredComp)] }
  : matchClauses ',' matchClause          { $3 : $1 }
  | matchClause                           { [$1] }

matchClause :: { (Pattern, SugaredComp) }
  : pattern '->' comp                     { ($1, $3) }

opTarget :: { OpTarget }
  : var                                   { TargetVar $1 }
  | op                                    { TargetOp $1 }

compInfix :: { SugaredComp }
  : compApp                               { $1 }
  | opchain                               { SCOpChain (fst $1) (reverse (snd $1)) }

opchain :: { (SugaredExpr, [(String, SugaredExpr)]) }
  : chainOperand chainOp chainOperand     { ($1, [($2, $3)]) }
  | opchain chainOp chainOperand          { (fst $1, ($2, $3) : snd $1) }

chainOp :: { String }
  : '::'                                  { "::" }
  | '=='                                  { "==" }
  | '++'                                  { "++" }
  | '+'                                   { "+" }
  | '-'                                   { "-" }
  | '*'                                   { "*" }
  | '/'                                   { "/" }
  | '=>'                                  { "=>" }
  | '&&'                                  { "&&" }
  | '||'                                  { "||" }
  | '<'                                   { "<" }
  | '>'                                   { ">" }
  | '<='                                  { "<=" }
  | '>='                                  { ">=" }
  | '/='                                  { "/=" }
  | symop                                 { $1 }

chainOperand :: { SugaredExpr }
  : atom                                  { $1 }
  | compApp                               { SEComp $1 }
  | '(' expr ')'                          { $2 }
  | '(' expr ':' type ')'                 { SEAnnot $2 $4 }

compApp :: { SugaredComp }
  : exprApp exprAtom %prec APP            { SCApp $1 $2 }
  | op exprAtom %prec APP                 { SCOp $1 $2 }
  | '(' comp ')'                          { $2 }
  | '(' comp ':' compType ')'             { SCAnnot $2 $4 }

type :: { ValueType }
  : typeSum                               { $1 }
  | typeSum '->' compType                 { TFun $1 $3 }
  | compType '=>' compType                { THandler $1 $3 }

typeSum :: { ValueType }
  : typeProd                              { $1 }
  | typeProd '+' typeProd                 { TEither $1 $3 }

typeProd :: { ValueType }
  : typeAtom                              { $1 }
  | typeAtom '*' typeAtom                 { TPair $1 $3 }

typeAtom :: { ValueType }
  : Unit                                  { TUnit }
  | Void                                  { TVoid }
  | Int                                   { TInt }
  | Bool                                  { TBool }
  | Double                                { TDouble }
  | Str                                   { TString }
  | Name                                  { TName }
  | Map typeAtom typeAtom                 { TMap $2 $3 }
  | List typeAtom                         { TList $2 }
  | typeparam                             { TParam $1 }
  | '(' type ')'                          { $2 }

compType :: { CompType }
  : type '!' '{' effList '}'              { TComp $1 (mkEffects $4) }

effList :: { ([(Op, Arity)], Bool) }
  : {- empty -}                           { ([], False) }
  | '...'                                 { ([], True) }
  | opSig                                 { ([$1], False) }
  | opSig ',' effList                     { ($1 : fst $3, snd $3) }

opSig :: { (Op, Arity) }
  : var ':' type '~>' type                { ($1, Arity $3 $5) }

{

mkEffects :: ([(Op, Arity)], Bool) -> EffectsType
mkEffects (ops, isOpen)
  | isOpen    = Open (Map.fromList ops) ""
  | otherwise = Closed (Map.fromList ops)

exprToComp :: SugaredExpr -> SugaredComp
exprToComp (SEComp c) = c
exprToComp e          = SCReturn e

mkLet :: (Pattern, SugaredExpr) -> SugaredComp -> SugaredComp
mkLet (p, e) body = SCDo p (exprToComp e) body

mkLetRec :: [(Ident, [Pattern], SugaredComp)] -> SugaredComp -> SugaredComp
mkLetRec [(f, ps, c)] body = SCDo (PVar f) (SCReturn (SERec f ps c)) body
mkLetRec defs         body = SCLetRec defs body

parseError :: [Token] -> [String] -> Except String a
parseError (Token (AlexPn _ line col) s _:_) expected =
  throwError $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ ".\n" ++
               "Unexpected token: " ++ s ++ "\n" ++
               "Expected one of: " ++ unwords  expected
parseError [] expected =
  throwError $ "Unexpected end of input.\n" ++
               "Expected one of: " ++ unwords expected

parse :: String -> Either String SugaredComp
parse = runExcept . comp . alexScanTokens

}
