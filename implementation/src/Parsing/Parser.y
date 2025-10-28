{
module Parsing.Parser (
  parse,
) where

import Parsing.Token
import Parsing.Lexer
import Parsing.SugaredSyntax
import Syntax (Ident, Op, Side(..))

import Control.Monad.Except
}

%name expr
%tokentype { Token }
%monad { Except String } { (>>=) } { return }
%error { parseError }
%error.expected
%expect 1

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

  integer                    { Token _ _ (TokInt $$) }
  boolean                    { Token _ _ (TokBool $$) }
  string                     { Token _ _ (TokString $$) }
  var                        { Token _ _ (TokIdent $$) }
  op                         { Token _ _ (TokOp $$) }

%right ';'
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

exprInfix :: { SugaredExpr }
  : atom                                  { $1 }
  | compInfix                             { SEComp $1 }
  | '(' expr ')'                          { $2 }

exprApp :: { SugaredExpr }
  : var                                   { SEVar $1 }
  | compApp                               { SEComp $1 }
  | '(' expr ')'                          { $2 }

exprAtom :: { SugaredExpr }
  : atom                                  { $1 }
  | '(' expr ')'                          { $2 }

nvar :: { Ident }
  : var                                   { $1 }
  | '_'                                   { "_" }

nvars :: { [Ident] }
  : nvars nvar                            { $2 : $1 }
  | nvar                                  { [$1] }

value :: { SugaredExpr }
  : atom                                  { $1 }
  | inl exprAtom                          { SEEither L $2 }
  | inr exprAtom                          { SEEither R $2 }
  | fun nvars '->' comp                   { SEFun (reverse $2) $4 }
  | rec nvar nvars '->' comp              { SERec $2 (reverse $3) $5 }
  | handler '{' handlerClauses '}'        { SEHandler $3 }

atom :: { SugaredExpr }
  : '()'                                  { SEUnit }
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
  : return nvar '->' comp                 { RC $2 $4 }
  | var '(' nvar ';' nvar ')' '->' comp   { OC $1 $3 $5 $8 }
  | finally nvar '->' comp                { FC $2 $4 }

comp :: { SugaredComp }
  : compTerm ';' comp                     { SCDo "_" $1 $3 }
  | compTerm %prec ';'                    { $1 }

compTerm :: { SugaredComp }
  : return expr                           { SCReturn $2 }
  | do nvar '<-' comp in compTerm         { SCDo $2 $4 $6 }
  | if expr then comp else compTerm       { SCIf $2 $4 $6 }
  | case expr of '{' eitherMatch '}'      { SCCase $2 (fst $5) (snd $5) }
  | with expr handle compTerm             { SCWith $2 $4 }
  | compInfix                             { $1 }

compInfix :: { SugaredComp }
  : exprInfix '==' exprInfix              { SCApp (SEVar "==") (SEPair $1 $3) }
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

eitherMatch :: { ((Ident, SugaredComp), (Ident, SugaredComp)) }
  : inlMatch ',' inrMatch                 { ($1, $3) }
  | inrMatch ',' inlMatch                 { ($3, $1) }

inlMatch :: { (Ident, SugaredComp) }
  : inl nvar '->' comp                    { ($2, $4) }

inrMatch :: { (Ident, SugaredComp) }
  : inr nvar '->' comp                    { ($2, $4) }

{

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
