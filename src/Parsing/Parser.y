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

%name expr
%tokentype { Token }
%monad { Except String } { (>>=) } { return }
%error { parseError }
%error.expected
%expect 2

%token
  fun                        { Token _ _ TokFun }
  rec                        { Token _ _ TokRec }
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

  integer                    { Token _ _ (TokInt $$) }
  boolean                    { Token _ _ (TokBool $$) }
  string                     { Token _ _ (TokString $$) }
  var                        { Token _ _ (TokIdent $$) }
  op                         { Token _ _ (TokOp $$) }
  typeparam                  { Token _ _ (TokTypeParam $$) }

%right ';'
%right '::'
%nonassoc '=='
%left '++'
%left '+' '-'
%left '*' '/'
%nonassoc APP

%%

expr :: { SugaredExpr }
  : value                                 { $1 }
  | comp                                  { SEComp $1 }
  | '(' expr ')'                          { $2 }
  | '(' expr ':' type ')'                 { SEAnnot $2 $4 }

exprInfix :: { SugaredExpr }
  : atom                                  { $1 }
  | compInfix                             { SEComp $1 }
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

pattern :: { Pattern }
  : var                                   { PVar $1 }
  | '_'                                   { PWild }
  | '(' pattern ',' pattern ')'           { PPair $2 $4 }

patterns :: { [Pattern] }
  : patterns pattern                      { $2 : $1 }
  | pattern                               { [$1] }

value :: { SugaredExpr }
  : atom                                  { $1 }
  | inl exprAtom                          { SEEither L $2 }
  | inr exprAtom                          { SEEither R $2 }
  | fun patterns '->' comp                { SEFun (reverse $2) $4 }
  | rec wildvar patterns '->' comp        { SERec $2 (reverse $3) $5 }
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
  : return pattern '->' comp              { RC $2 $4 }
  | var pattern wildvar '->' comp         { OC $1 $2 $3 $5 }
  | finally pattern '->' comp             { FC $2 $4 }
  | typeparam '->' type                   { TC $1 $3 }

comp :: { SugaredComp }
  : compTerm ';' comp                     { SCDo PWild $1 $3 }
  | compTerm %prec ';'                    { $1 }

compTerm :: { SugaredComp }
  : return expr                           { SCReturn $2 }
  | do pattern '<-' comp in compTerm      { SCDo $2 $4 $6 }
  | if expr then comp else compTerm       { SCIf $2 $4 $6 }
  | case expr of '{' eitherMatch '}'      { SCCase $2 (fst $5) (snd $5) }
  | with expr handle compTerm             { SCWith $2 $4 }
  | effect op ':' type '~>' type '.' compTerm  { SCEffect $2 (Arity $4 $6) $8 }
  | compInfix                             { $1 }

compInfix :: { SugaredComp }
  : exprInfix '::' exprInfix              { SCApp (SEVar "::") (SEPair $1 $3) }
  | exprInfix '==' exprInfix              { SCApp (SEVar "==") (SEPair $1 $3) }
  | exprInfix '++' exprInfix              { SCApp (SEVar "++") (SEPair $1 $3) }
  | exprInfix '+'  exprInfix              { SCApp (SEVar "+") (SEPair $1 $3) }
  | exprInfix '-'  exprInfix              { SCApp (SEVar "-") (SEPair $1 $3) }
  | exprInfix '*'  exprInfix              { SCApp (SEVar "*") (SEPair $1 $3) }
  | exprInfix '/'  exprInfix              { SCApp (SEVar "/") (SEPair $1 $3) }
  | compApp                               { $1 }

compApp :: { SugaredComp }
  : exprApp exprAtom %prec APP            { SCApp $1 $2 }
  | op exprAtom %prec APP                 { SCOp $1 $2 }
  | '(' comp ')'                          { $2 }
  | '(' comp ':' compType ')'             { SCAnnot $2 $4 }

eitherMatch :: { ((Pattern, SugaredComp), (Pattern, SugaredComp)) }
  : inlMatch ',' inrMatch                 { ($1, $3) }
  | inrMatch ',' inlMatch                 { ($3, $1) }

inlMatch :: { (Pattern, SugaredComp) }
  : inl pattern '->' comp                 { ($2, $4) }

inrMatch :: { (Pattern, SugaredComp) }
  : inr pattern '->' comp                 { ($2, $4) }

type :: { ValueType }
  : typeSum                               { $1 }
  | typeSum '->' compType                 { TFun $1 $3 }
  | compType '=>' compType                { THandler $1 mempty $3 }

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

parseError :: [Token] -> [String] -> Except String a
parseError (Token (AlexPn _ line col) s _:_) expected =
  throwError $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ ".\n" ++
               "Unexpected token: " ++ s ++ "\n" ++
               "Expected one of: " ++ unwords  expected
parseError [] expected =
  throwError $ "Unexpected end of input.\n" ++
               "Expected one of: " ++ unwords expected

parse :: String -> Either String SugaredExpr
parse = runExcept . expr . alexScanTokens

}
