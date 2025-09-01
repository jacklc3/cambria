{
module Parsing.Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$whitechar  = [\ \t\n\r]

tokens :-
  $whitechar+                ;
  "--".*                     ;

  fun                        { \p _ -> Token p TokFun }
  rec                        { \p _ -> Token p TokRec }
  handler                    { \p _ -> Token p TokHandler }
  return                     { \p _ -> Token p TokReturn }
  finally                    { \p _ -> Token p TokFinally }
  do                         { \p _ -> Token p TokDo }
  in                         { \p _ -> Token p TokIn }
  if                         { \p _ -> Token p TokIf }
  then                       { \p _ -> Token p TokThen }
  else                       { \p _ -> Token p TokElse }
  with                       { \p _ -> Token p TokWith }
  handle                     { \p _ -> Token p TokHandle }
  inl                        { \p _ -> Token p TokInl }
  inr                        { \p _ -> Token p TokInr }
  case                       { \p _ -> Token p TokCase }
  of                         { \p _ -> Token p TokOf }

  "()"                       { \p _ -> Token p TokUnit }
  "&&"                       { \p _ -> Token p TokAnd }
  "||"                       { \p _ -> Token p TokOr }
  "=="                       { \p _ -> Token p TokEq }
  "/="                       { \p _ -> Token p TokNEq }
  "<="                       { \p _ -> Token p TokLTE }
  ">="                       { \p _ -> Token p TokGTE }
  "<"                        { \p _ -> Token p TokLT }
  ">"                        { \p _ -> Token p TokGT }
  "->"                       { \p _ -> Token p TokArrow }
  "<-"                       { \p _ -> Token p TokLeftArrow }
  "="                        { \p _ -> Token p TokEquals }
  "+"                        { \p _ -> Token p TokPlus }
  "-"                        { \p _ -> Token p TokMinus }
  "*"                        { \p _ -> Token p TokAsterisk }
  "/"                        { \p _ -> Token p TokSlash }
  "("                        { \p _ -> Token p TokLParen }
  ")"                        { \p _ -> Token p TokRParen }
  "{"                        { \p _ -> Token p TokLBrace }
  "}"                        { \p _ -> Token p TokRBrace }
  ","                        { \p _ -> Token p TokComma }
  "!"                        { \p _ -> Token p TokExclam }
  "_"                        { \p _ -> Token p TokUnderscore }
  ";"                        { \p _ -> Token p TokSemiColon }
  "++"                       { \p _ -> Token p TokConcat }

  $digit+                    { \p s -> Token p (TokInt (read s)) }
  true                       { \p _ -> Token p (TokBool True) }
  false                      { \p _ -> Token p (TokBool False) }
  $alpha[$alpha$digit\_\']*  { \p s -> Token p (TokIdent s) }
  \"(\\.|[^\"])*\"           { \p s -> Token p (tokString s) }
  !$alpha[$alpha$digit\_\']* { \p s -> Token p (TokOp s) }

{

tokString :: String -> BaseToken
tokString s = TokString $ unescape (init (tail s))

unescape :: String -> String
unescape [] = []
unescape ('\\' : 'n' : cs) = '\n' : unescape cs
unescape ('\\' : 't' : cs) = '\t' : unescape cs
unescape ('\\' : '"' : cs) = '"'  : unescape cs
unescape ('\\' : '\\' : cs) = '\\' : unescape cs
unescape (c : cs) = c : unescape cs

data BaseToken
  = TokInt Integer
  | TokBool Bool
  | TokString String
  | TokIdent String
  | TokOp String
  | TokFun
  | TokRec
  | TokHandler
  | TokReturn
  | TokFinally
  | TokDo
  | TokIn
  | TokIf
  | TokThen
  | TokElse
  | TokWith
  | TokHandle
  | TokInl
  | TokInr
  | TokCase
  | TokOf
  | TokUnit
  | TokAnd
  | TokOr
  | TokEq
  | TokNEq
  | TokLTE
  | TokGTE
  | TokLT
  | TokGT
  | TokArrow
  | TokLeftArrow
  | TokEquals
  | TokPlus
  | TokMinus
  | TokAsterisk
  | TokSlash
  | TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokComma
  | TokExclam
  | TokUnderscore
  | TokSemiColon
  | TokConcat
  deriving (Eq, Show)

data Token = Token AlexPosn BaseToken deriving (Eq, Show)

scanTokens = alexScanTokens

}
