module Parsing.Token where

data TokenKind
  = TokInt Integer
  | TokBool Bool
  | TokString String
  | TokIdent String
  | TokOp String
  -- Keywords
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
  -- Type names
  | TokTUnit
  | TokTVoid
  | TokTInt
  | TokTBool
  | TokTDouble
  | TokTString
  | TokTName
  | TokTMap
  | TokTList
  -- Symbols
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
  | TokEffect
  | TokSquigglyArrow
  | TokColon
  | TokDot
  | TokCons
  | TokNil
  | TokTypeParam String
  | TokEllipsis
  | TokFatArrow
