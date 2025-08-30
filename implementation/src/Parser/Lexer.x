{
module Parser.Lexer (
  Token(..),
  scanTokens
) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$whitechar  = [\ \t\n\r]

tokens :-
  $whitechar+                ;
  "--".*                     ;

  fun                        { const TokFun }
  rec                        { const TokRec }
  handler                    { const TokHandler }
  return                     { const TokReturn }
  finally                    { const TokFinally }
  do                         { const TokDo }
  in                         { const TokIn }
  if                         { const TokIf }
  then                       { const TokThen }
  else                       { const TokElse }
  with                       { const TokWith }
  handle                     { const TokHandle }
  inl                        { const TokInl }
  inr                        { const TokInr }
  case                       { const TokCase }
  of                         { const TokOf }

  "()"                       { const TokUnit }
  "&&"                       { const TokAnd }
  "||"                       { const TokOr }
  "=="                       { const TokEq }
  "/="                       { const TokNEq }
  "<="                       { const TokLTE }
  ">="                       { const TokGTE }
  "<"                        { const TokLT }
  ">"                        { const TokGT }
  "->"                       { const TokArrow }
  "<-"                       { const TokLeftArrow }
  "="                        { const TokEquals }
  "+"                        { const TokPlus }
  "-"                        { const TokMinus }
  "*"                        { const TokAsterisk }
  "/"                        { const TokSlash }
  "("                        { const TokLParen }
  ")"                        { const TokRParen }
  "{"                        { const TokLBrace }
  "}"                        { const TokRBrace }
  ","                        { const TokComma }
  "!"                        { const TokExclam }
  "_"                        { const TokUnderscore }
  ";"                        { const TokSemiColon }
  "++"                       { const TokConcat }

  $digit+                    { TokInt . read }
  true                       { const $ TokBool True }
  false                      { const $ TokBool False }
  $alpha[$alpha$digit\_\']*  { TokIdent }
  \"(\\.|[^\"])*\"           { tokString }

{

tokString :: String -> Token
tokString s = TokString $ unescape (init (tail s))

unescape :: String -> String
unescape [] = []
unescape ('\\' : 'n' : cs) = '\n' : unescape cs -- Newline
unescape ('\\' : 't' : cs) = '\t' : unescape cs -- Tab
unescape ('\\' : '"' : cs) = '"'  : unescape cs -- Double quote
unescape ('\\' : '\\' : cs) = '\\' : unescape cs -- Backslash
unescape (c : cs) = c : unescape cs

data Token
  = TokInt Integer
  | TokBool Bool
  | TokString String
  | TokIdent String
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
  deriving (Eq,Show)

scanTokens = alexScanTokens

}
