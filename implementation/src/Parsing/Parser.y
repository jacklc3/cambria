{

module Parsing.Parser (
  parseExpr,
) where

import Parsing.Lexer
import Parsing.SugaredSyntax
import Syntax (Ident, Op, Side(..))

import Control.Monad.Except

}

%name expr
%tokentype { Token }
%monad { Except String } { (>>=) } { return }
%error { parseError }

%token
  fun                        { TokFun }
  rec                        { TokRec }
  handler                    { TokHandler }
  return                     { TokReturn }
  finally                    { TokFinally }
  do                         { TokDo }
  in                         { TokIn }
  if                         { TokIf }
  then                       { TokThen }
  else                       { TokElse }
  with                       { TokWith }
  handle                     { TokHandle }
  inl                        { TokInl }
  inr                        { TokInr }
  case                       { TokCase }
  of                         { TokOf }

  '()'                       { TokUnit }
  '=='                       { TokEq }
  '->'                       { TokArrow }
  '<-'                       { TokLeftArrow }
  '+'                        { TokPlus }
  '-'                        { TokMinus }
  '*'                        { TokAsterisk }
  '/'                        { TokSlash }
  '('                        { TokLParen }
  ')'                        { TokRParen }
  '{'                        { TokLBrace }
  '}'                        { TokRBrace }
  ','                        { TokComma }
  '!'                        { TokExclam }
  '_'                        { TokUnderscore }
  ';'                        { TokSemiColon }
  '++'                       { TokConcat }

  int                        { TokInt $$ }
  bool                       { TokBool $$ }
  string                     { TokString $$ }
  ident                      { TokIdent $$ }

%right ';'
%nonassoc '=='
%left '++'
%left '+' '-'
%left '*' '/'

%%

expr :: { SugaredExpr }
  : value                                 { $1 }
  | comp                                  { SEComp $1 }
  | '(' expr ')'                          { $2 }
  | var                                   { SEVar $1 }

var :: { Ident }
  : ident                                 { $1 }

op :: { Op }
  : ident                                 { $1 }

nvar :: { Ident }
  : var                                   { $1 }
  | '_'                                   { "_" }

nvars :: { [Ident] }
  : nvars nvar                            { $2 : $1 }
  | nvar                                  { [$1] }

value :: { SugaredExpr }
  : '()'                                  { SEUnit }
  | bool                                  { SEBool $1 }
  | int                                   { SEInt $1 }
  | string                                { SEString $1 }
  | '(' expr ',' expr ')'                 { SEPair $2 $4 }
  | inl expr                              { SEEither L $2 }
  | inr expr                              { SEEither R $2 }
  | fun nvars '->' comp                   { SEFun (reverse $2) $4 }
  | rec nvar nvars '->' comp              { SERec $2 (reverse $3) $5 }
  | handler '{' nhandlerClauses '}'       { SEHandler (reverse $3) }

handlerClauses :: { [HandlerClause] }
  : handlerClauses ',' handlerClause      { $3 : $1 }
  | handlerClause                         { [$1] }

nhandlerClauses :: { [HandlerClause] }
  : handlerClauses                        { $1 }
  | {- empty -}                           { [] }

handlerClause :: { HandlerClause }
  : return nvar '->' comp                 { RC $2 $4 }
  | op '(' nvar ';' nvar ')' '->' comp    { OC $1 $3 $5 $8 }
  | finally nvar '->' comp                { FC $2 $4 }

guardedExpr :: { SugaredExpr }
  : value                                 { $1 }
  | '(' expr ')'                          { $2 }
  | var                                   { SEVar $1 }

guardedExprs :: { [SugaredExpr] }
  : guardedExprs guardedExpr              { $2 : $1 }
  | guardedExpr                           { [$1] }

comp :: { SugaredComp }
  : comp ';'  comp                        { SCDo "_" $1 $3 }
  | expr '==' expr                        { SCApp (SEVar "==") [SEPair $1 $3] }
  | expr '++' expr                        { SCApp (SEVar "++") [SEPair $1 $3] }
  | expr '+'  expr                        { SCApp (SEVar "+") [SEPair $1 $3] }
  | expr '-'  expr                        { SCApp (SEVar "-") [SEPair $1 $3] }
  | expr '*'  expr                        { SCApp (SEVar "*") [SEPair $1 $3] }
  | expr '/'  expr                        { SCApp (SEVar "/") [SEPair $1 $3] }
  | var guardedExprs                      { SCApp (SEVar $1) (reverse $2) }
  | '(' expr ')' guardedExprs             { SCApp $2 (reverse $4) }
  | return expr                           { SCReturn $2 }
  | '!' op expr                           { SCOp $2 $3 }
  | do nvar '<-' comp in comp             { SCDo $2 $4 $6 }
  | if expr then comp else comp           { SCIf $2 $4 $6 }
  | caseMatch                             { $1 }
  | with expr handle comp                 { SCWith $2 $4 }
  | '(' comp ')'                          { $2 }

caseMatch :: { SugaredComp }
  : case expr of '{' inlMatch ',' inrMatch '}'     { SCCase $2 $5 $7 }
  | case expr of '{' inrMatch ',' inlMatch '}'     { SCCase $2 $7 $5 }

inlMatch :: { (Ident, SugaredComp) }
  : inl nvar '->' comp                    { ($2, $4) }

inrMatch :: { (Ident, SugaredComp) }
  : inr nvar '->' comp                    { ($2, $4) }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of input"

parseExpr :: String -> Either String SugaredExpr
parseExpr input =
  let tokenStream = scanTokens input in
  runExcept (expr tokenStream)

}
