{
module Parsing.Parser (
  parseExpr,
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
  '_'                        { Token _ TokUnderscore }
  ';'                        { Token _ TokSemiColon }
  '++'                       { Token _ TokConcat }

  integer                    { Token _ (TokInt $$) }
  boolean                    { Token _ (TokBool $$) }
  string                     { Token _ (TokString $$) }
  var                        { Token _ (TokIdent $$) }
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
  | op exprAtom %prec APP                 { SCOp $1 $2 }
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
parseError (Token (AlexPn _ line col) tok:_) expected =
  throwError $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ ".\n" ++
               "Unexpected token: " ++ show tok ++ "\n" ++
               "Expected one of: " ++ unwords  expected
parseError [] expected =
  throwError $ "Unexpected end of input.\n" ++
               "Expected one of: " ++ unwords expected

parseExpr :: String -> Either String SugaredExpr
parseExpr = runExcept . expr . alexScanTokens

}
