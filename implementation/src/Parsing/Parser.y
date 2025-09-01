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
  fun                        { Token _ TokFun }
  rec                        { Token _ TokRec }
  handler                    { Token _ TokHandler }
  return                     { Token _ TokReturn }
  finally                    { Token _ TokFinally }
  do                         { Token _ TokDo }
  in                         { Token _ TokIn }
  if                         { Token _ TokIf }
  then                       { Token _ TokThen }
  else                       { Token _ TokElse }
  with                       { Token _ TokWith }
  handle                     { Token _ TokHandle }
  inl                        { Token _ TokInl }
  inr                        { Token _ TokInr }
  case                       { Token _ TokCase }
  of                         { Token _ TokOf }

  '()'                       { Token _ TokUnit }
  '=='                       { Token _ TokEq }
  '->'                       { Token _ TokArrow }
  '<-'                       { Token _ TokLeftArrow }
  '+'                        { Token _ TokPlus }
  '-'                        { Token _ TokMinus }
  '*'                        { Token _ TokAsterisk }
  '/'                        { Token _ TokSlash }
  '('                        { Token _ TokLParen }
  ')'                        { Token _ TokRParen }
  '{'                        { Token _ TokLBrace }
  '}'                        { Token _ TokRBrace }
  ','                        { Token _ TokComma }
  '!'                        { Token _ TokExclam }
  '_'                        { Token _ TokUnderscore }
  ';'                        { Token _ TokSemiColon }
  '++'                       { Token _ TokConcat }

  int                        { Token _ (TokInt $$) }
  bool                       { Token _ (TokBool $$) }
  string                     { Token _ (TokString $$) }
  ident                      { Token _ (TokIdent $$) }
  op                         { Token _ (TokOp $$) }

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
  | var                                   { SEVar $1 }

var :: { Ident }
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
  | ident '(' nvar ';' nvar ')' '->' comp { OC $1 $3 $5 $8 }
  | finally nvar '->' comp                { FC $2 $4 }

guardedExpr :: { SugaredExpr }
  : '()'                                  { SEUnit }
  | bool                                  { SEBool $1 }
  | int                                   { SEInt $1 }
  | string                                { SEString $1 }
  | '(' expr ',' expr ')'                 { SEPair $2 $4 }
  | handler '{' nhandlerClauses '}'       { SEHandler (reverse $3) }
  | var                                   { SEVar $1 }
  | '(' expr ')'                          { $2 }

guardedExprs :: { [SugaredExpr] }
  : guardedExprs guardedExpr    { $2 : $1 }
  | guardedExpr                           { [$1] }

comp :: { SugaredComp }
  : comp ';'  comp                        { SCDo "_" $1 $3 }
  | expr '==' expr                        { SCApp (SEVar "==") [SEPair $1 $3] }
  | expr '++' expr                        { SCApp (SEVar "++") [SEPair $1 $3] }
  | expr '+'  expr                        { SCApp (SEVar "+") [SEPair $1 $3] }
  | expr '-'  expr                        { SCApp (SEVar "-") [SEPair $1 $3] }
  | expr '*'  expr                        { SCApp (SEVar "*") [SEPair $1 $3] }
  | expr '/'  expr                        { SCApp (SEVar "/") [SEPair $1 $3] }
  | guardedExpr guardedExprs %prec APP    { SCApp $1 (reverse $2) }
  | return expr                           { SCReturn $2 }
  | op guardedExpr %prec APP              { SCOp (tail $1) $2 }
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
parseError (Token (AlexPn _ line col) tok:_) =
  throwError $ "Parse error at " ++ show line ++
  ":" ++ show col ++ ", with token: " ++ show tok
parseError [] =
  throwError "Unexpected end of input"

parseExpr :: String -> Either String SugaredExpr
parseExpr input =
  let tokenStream = scanTokens input in
  runExcept (expr tokenStream)

}
