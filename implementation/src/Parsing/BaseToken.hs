{-# LANGUAGE LambdaCase #-}

module Parsing.BaseToken where

data BaseToken
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

instance Show BaseToken where
  show t = "'" ++ showTok t ++ "'"
    where
      showTok = \case
        TokInt i      -> show i
        TokBool True  -> "true"
        TokBool False -> "false"
        TokIdent x    -> x
        TokString s   -> "\"" ++ s ++ "\""
        TokOp op      -> "!" ++ op
        -- Keywords
        TokFun        -> "fun"
        TokRec        -> "rec"
        TokHandler    -> "handler"
        TokReturn     -> "return"
        TokFinally    -> "finally"
        TokDo         -> "do"
        TokIn         -> "in"
        TokIf         -> "if"
        TokThen       -> "then"
        TokElse       -> "else"
        TokWith       -> "with"
        TokHandle     -> "handle"
        TokInl        -> "inl"
        TokInr        -> "inr"
        TokCase       -> "case"
        TokOf         -> "of"
        -- Symbols
        TokUnit       -> "()"
        TokAnd        -> "&&"
        TokOr         -> "||"
        TokEq         -> "=="
        TokNEq        -> "/="
        TokLTE        -> "<="
        TokGTE        -> ">="
        TokLT         -> "<"
        TokGT         -> ">"
        TokArrow      -> "->"
        TokLeftArrow  -> "<-"
        TokEquals     -> "="
        TokPlus       -> "+"
        TokMinus      -> "-"
        TokAsterisk   -> "*"
        TokSlash      -> "/"
        TokLParen     -> "("
        TokRParen     -> ")"
        TokLBrace     -> "{"
        TokRBrace     -> "}"
        TokComma      -> ","
        TokExclam     -> "!"
        TokUnderscore -> "_"
        TokSemiColon  -> ";"
        TokConcat     -> "++"
