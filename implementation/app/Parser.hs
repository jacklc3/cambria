{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProgram) where

import Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Text (Text, pack)
import Data.List (foldl', foldl1')

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

reservedWords :: [String]
reservedWords =
  [ "true", "false", "fun", "handler", "return", "do", "in",
    "if", "then", "else", "with", "handle", "_"
  ]

pIdentifier :: Parser String
pIdentifier = (lexeme . try) $ do
  name <- (:) <$> letterChar <*> many alphaNumChar
  if name `elem` reservedWords
    then fail $ "keyword " ++ show name ++ " cannot be an identifier"
    else return name

desugar2 :: (Value -> Value -> Computation) -> Computation -> Computation -> Computation
desugar2 f (CReturn v1) (CReturn v2) = f v1 v2
desugar2 f (CReturn v1) c2           = CSeq "_a" c2 (f v1 (VVar "_a"))
desugar2 f c1           (CReturn v2) = CSeq "_f" c1 (f (VVar "_f") v2)
desugar2 f c1           c2           = CSeq "_f" c1 (CSeq "_a" c2 (f (VVar "_f") (VVar "_a")))

operatorTable :: [[Operator Parser Computation]]
operatorTable =
    [ [ InfixL (return (desugar2 CApp)) ] -- Function application
    , [ InfixL (symbol ";" >> return (\c1 c2 -> CSeq "_" c1 c2)) ] -- Sequencing
    ]

pComputation :: Parser Computation
pComputation = makeExprParser pTerm operatorTable

pReturn :: Parser Computation
pReturn = CReturn <$> (symbol "return" *> pValue)

pTerm :: Parser Computation
pTerm = choice
    [ try $ parens pComputation
    , try pIf
    , try pDo
    , try pWith
    , try pFun
    , pOp
    , try pReturn
    , try pPairComp
    , CReturn <$> pValue
    ]

pValue :: Parser Value
pValue = choice
    [ pUnit
    , pBool
    , pInteger
    , pString
    , pHandler
    , pVar
    ]

pVar :: Parser Value
pVar = VVar <$> pIdentifier

pUnit :: Parser Value
pUnit = VUnit <$ symbol "()"

pBool :: Parser Value
pBool = (VBool True <$ symbol "true") <|> (VBool False <$ symbol "false")

pInteger :: Parser Value
pInteger = VInt <$> lexeme L.decimal

pString :: Parser Value
pString = VString <$> (char '"' *> manyTill L.charLiteral (char '"'))

pPairComp :: Parser Computation
pPairComp = symbol "return" *> (parens $ do
  c1 <- pComputation
  symbol ","
  c2 <- pComputation
  return $ desugar2 (\v1 v2 -> CReturn (VPair v1 v2)) c1 c2)

pHandler :: Parser Value
pHandler = VHandler <$> (symbol "handler" *> braces pHandlerBody)

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

pFun :: Parser Computation
pFun = do
    symbol "fun"
    vars <- some pIdentifier
    symbol "->"
    body <- pComputation
    return $ CReturn $ desugar vars body
  where
    desugar [v] b = VFun v b
    desugar (v:vs) b = VFun v (CReturn (desugar vs b))
    desugar [] _ = error "fun with no arguments, this should have failed at the parser"

pIf :: Parser Computation
pIf = do
  symbol "if"
  cb <- pComputation
  symbol "then"
  c1 <- pComputation
  symbol "else"
  c2 <- pComputation
  return $ CSeq "_b" cb (CIf (VVar "_b") c1 c2)

pOp :: Parser Computation
pOp = do
  symbol "#"
  op <- pIdentifier
  c <- pTerm
  return $ CSeq "_p" c (COp op (VVar "_p"))

pProgram :: Parser Computation
pProgram = between sc eof pComputation

parseProgram :: String -> String -> Either (ParseErrorBundle Text Void) Computation
parseProgram filename content = runParser pProgram filename (pack content)
