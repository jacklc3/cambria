{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser (parseProgram) where

import Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Text (Text, pack, unpack)
import Data.List (foldl', foldl1')
import Data.String (fromString)

type Parser = Parsec Void Text

-- Space consumer: skips spaces, tabs, newlines, and comments (not specified, so none).
sc :: Parser ()
sc = L.space space1 empty empty

-- Lexeme: a parser that consumes trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbol: parses a literal string and consumes trailing whitespace.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Parens: parses something enclosed in parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Braces: parses something enclosed in braces.
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- A list of reserved words that cannot be used as identifiers.
reservedWords :: [String]
reservedWords =
  [ "true", "false", "fun", "rec", "handler", "return", "do", "in",
    "if", "then", "else", "with", "handle"
  ]

-- Identifier parser: parses a valid identifier, ensuring it's not a reserved word.
pIdentifier :: Parser String
pIdentifier = (lexeme. try) $ do
  name <- (:) <$> letterChar <*> many alphaNumChar
  if name `elem` reservedWords
    then fail $ "keyword " ++ show name ++ " cannot be an identifier"
    else return name

-- Parser for all Value types
pValue :: Parser Value
pValue = choice [
    pUnit,
    pBool,
    pInteger,
    pString,
    pPair,
    pVar,
    pFun,
    pHandler]

pVar :: Parser Value
pVar = do
    v <- pIdentifier
    return $ VVar v

pUnit :: Parser Value
pUnit = VUnit <$ symbol "()"

pBool :: Parser Value
pBool = (VBool True <$ symbol "true") <|> (VBool False <$ symbol "false")

pInteger :: Parser Value
pInteger = VInt <$> lexeme L.decimal

pString :: Parser Value
pString = VString <$> (char '"' *> manyTill L.charLiteral (char '"'))

pPair :: Parser Value
pPair = parens $ do
  v1 <- pValue
  symbol ","
  v2 <- pValue
  return $ VPair v1 v2

pFun :: Parser Value
pFun = do
  symbol "fun"
  var <- pIdentifier
  symbol "->"
  body <- pComputation
  return $ VFun var body

pHandler :: Parser Value
pHandler = VHandler <$> (symbol "handler" *> braces (pHandlerBody))

pHandlerBody :: Parser Handler
pHandlerBody = do
  clauses <- sepBy (pReturnClause <|> pOpClause) (symbol ",")
  let retClause = extractReturn clauses
      opClauses = foldl' extractOps [] clauses
  return $ Handler retClause opClauses
  where
    extractReturn ((Left ret):_) = ret
    extractReturn (_:xs)         = extractReturn xs
    extractReturn []             = error "No return in handler"
    extractOps acc (Right op)    = op : acc
    extractOps acc _             = acc

pReturnClause :: Parser (Either (VarName, Computation) (OpName, VarName, VarName, Computation))
pReturnClause = Left <$> (symbol "return" *> pClauseBody)

pOpClause :: Parser (Either (VarName, Computation) (OpName, VarName, VarName, Computation))
pOpClause = do
  opName <- pIdentifier
  (param, k) <- parens $ do
    p <- pIdentifier
    symbol ";"
    c <- pIdentifier
    return (p, c)
  symbol "->"
  body <- pComputation
  return $ Right (opName, param, k, body)

pClauseBody :: Parser (VarName, Computation)
pClauseBody = do
  var <- pIdentifier
  symbol "->"
  body <- pComputation
  return (var, body)

-- A computation term is a non-application computation or a value wrapped in CReturn
pComputation :: Parser Computation
pComputation = parens ops <|> ops
    where ops = choice [pReturn, pWith, pIf, pDo, pOpArgs, pApp]

pApp :: Parser Computation
pApp = do
  f <- pValue
  a <- pValue
  return $ CApp f a

pReturn :: Parser Computation
pReturn = CReturn <$> (symbol "return" *> pValue)

pIf :: Parser Computation
pIf = do
  symbol "if"
  cond <- pValue
  symbol "then"
  c1 <- pComputation
  symbol "else"
  c2 <- pComputation
  return $ CIf cond c1 c2

pDo :: Parser Computation
pDo = do
  symbol "do"
  var <- pIdentifier
  symbol "<-"
  c1 <- pComputation
  symbol "in"
  c2 <- pComputation
  return $ CSeq var c1 c2

pWith :: Parser Computation
pWith = do
  symbol "with"
  h <- pValue
  symbol "handle"
  c <- pComputation
  case h of
    VHandler handler -> return $ CHandle handler c
    _ -> fail "Expected a handler value in 'with' expression"

pOpArgs :: Parser Computation
pOpArgs = do
    symbol "#"
    op <- pIdentifier
    (v, y, c) <- parens $ do
        val <- pValue
        symbol ";"
        var <- pIdentifier
        symbol "."
        comp <- pComputation
        return (val, var, comp)
    return (COp op v y c)

-- Top-level parser for a whole program
pProgram :: Parser Computation
pProgram = between sc eof pComputation

-- Main export function
parseProgram :: String -> String -> Either (ParseErrorBundle Text Void) Computation
parseProgram filename content = runParser pProgram filename (pack content)
