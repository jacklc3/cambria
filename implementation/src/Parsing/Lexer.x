{
module Parsing.Lexer where

import Parsing.Token
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$whitechar  = [\ \t\n\r]

tokens :-
  $whitechar+                ;
  "--".*                     ;

  fun                        { \p s -> Token p s TokFun }
  rec                        { \p s -> Token p s TokRec }
  handler                    { \p s -> Token p s TokHandler }
  return                     { \p s -> Token p s TokReturn }
  finally                    { \p s -> Token p s TokFinally }
  do                         { \p s -> Token p s TokDo }
  in                         { \p s -> Token p s TokIn }
  if                         { \p s -> Token p s TokIf }
  then                       { \p s -> Token p s TokThen }
  else                       { \p s -> Token p s TokElse }
  with                       { \p s -> Token p s TokWith }
  handle                     { \p s -> Token p s TokHandle }
  inl                        { \p s -> Token p s TokInl }
  inr                        { \p s -> Token p s TokInr }
  case                       { \p s -> Token p s TokCase }
  of                         { \p s -> Token p s TokOf }

  "()"                       { \p s -> Token p (quotes s) TokUnit }
  "&&"                       { \p s -> Token p (quotes s) TokAnd }
  "||"                       { \p s -> Token p (quotes s) TokOr }
  "=="                       { \p s -> Token p (quotes s) TokEq }
  "/="                       { \p s -> Token p (quotes s) TokNEq }
  "<="                       { \p s -> Token p (quotes s) TokLTE }
  ">="                       { \p s -> Token p (quotes s) TokGTE }
  "<"                        { \p s -> Token p (quotes s) TokLT }
  ">"                        { \p s -> Token p (quotes s) TokGT }
  "->"                       { \p s -> Token p (quotes s) TokArrow }
  "<-"                       { \p s -> Token p (quotes s) TokLeftArrow }
  "="                        { \p s -> Token p (quotes s) TokEquals }
  "+"                        { \p s -> Token p (quotes s) TokPlus }
  "-"                        { \p s -> Token p (quotes s) TokMinus }
  "*"                        { \p s -> Token p (quotes s) TokAsterisk }
  "/"                        { \p s -> Token p (quotes s) TokSlash }
  "("                        { \p s -> Token p (quotes s) TokLParen }
  ")"                        { \p s -> Token p (quotes s) TokRParen }
  "{"                        { \p s -> Token p (quotes s) TokLBrace }
  "}"                        { \p s -> Token p (quotes s) TokRBrace }
  ","                        { \p s -> Token p (quotes s) TokComma }
  "!"                        { \p s -> Token p (quotes s) TokExclam }
  "_"                        { \p s -> Token p (quotes s) TokUnderscore }
  ";"                        { \p s -> Token p (quotes s) TokSemiColon }
  "++"                       { \p s -> Token p (quotes s) TokConcat }

  $digit+                    { \p s -> Token p s (TokInt (read s)) }
  true                       { \p s -> Token p s (TokBool True) }
  false                      { \p s -> Token p s (TokBool False) }
  $alpha[$alpha$digit\_\']*  { \p s -> Token p s (TokIdent s) }
  \"(\\.|[^\"])*\"           { \p s -> Token p s (TokString (unescape (init (tail s)))) }
  !$alpha[$alpha$digit\_\']* { \p s -> Token p s (TokOp (tail s)) }

{

quotes :: String -> String
quotes s = "'" ++ s ++ "'"

unescape :: String -> String
unescape []                  = []
unescape ('\\' : 'n' : cs)   = '\n' : unescape cs
unescape ('\\' : 't' : cs)   = '\t' : unescape cs
unescape ('\\' : '"' : cs)   = '"'  : unescape cs
unescape ('\\' : '\\' : cs)  = '\\' : unescape cs
unescape (c:cs)              = c    : unescape cs

data Token = Token AlexPosn String TokenKind

}
