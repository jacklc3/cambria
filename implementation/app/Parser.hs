{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProgram) where

import Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Text (Text, pack, unpack)
import Data.List (foldl', foldl1', find, reverse)

type Parser = Parsec Void Text

data ValComp = V Value | C Computation | VCPair ValComp ValComp
data CompScope = Seq | Infix | App | Expr | Paren deriving (Eq, Ord)

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
    "if", "then", "else", "with", "handle"
  ]

pIdentifier :: Parser String
pIdentifier = (lexeme . try) $ do
  name <- (:) <$> letterChar <*> many alphaNumChar
  if name `elem` reservedWords
    then fail $ "keyword " ++ show name ++ " cannot be an identifier"
    else return name

-- Takes a list of value/computation terms and desugars them by sequencing any computations
desugar :: [ValComp] -> ([Value] -> Computation) -> Computation
desugar xs f = go xs [] 0
  where
    hiddenVar :: Int -> VarName
    hiddenVar n = "_" ++ show n
    go :: [ValComp] -> [Value] -> Int -> Computation
    go (x:xs) vs n = let (v, k, n') = goi x n
                     in  k (go xs (v:vs) n')
    go []     vs n = f (reverse vs)
    goi :: ValComp -> Int -> (Value, Computation -> Computation, Int)
    goi (V v) n        = (v, id, n)
    goi (C c) n        = (VVar (hiddenVar n), \c' -> CSeq (hiddenVar n) c c', succ n)
    goi (VCPair x y) n = let (v1, k1, n1) = goi x n
                             (v2, k2, n2) = goi y n1
                         in  (VPair v1 v2, k1 . k2, n2)

pComputation :: CompScope -> Parser Computation
pComputation cs = choice $ seq ++ infi ++ app ++ expr ++ [try (parens (pComputation Seq))]
  where
    seq  = if cs > Seq   then [] else [try pSeq]
    infi = if cs > Infix then [] else [try (pInfixOps ["++", "*", "+", "-"])]
    app  = if cs > App   then [] else [try pApp]
    expr = if cs > Expr  then [] else [pIf, pDo, pWith, pFun, pOp, pReturn]


pValComp :: CompScope -> Parser ValComp
pValComp sc = choice
  [ try $ V <$> pUnit
  , try pPair
  , V <$> pBool
  , V <$> pInteger
  , V <$> pString
  , V <$> pHandler
  , try $ C <$> pComputation sc
  , V <$> pVar
  , parens (pValComp Seq)
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
pString = VString <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pFun :: Parser Computation
pFun = do
    symbol "fun"
    vars <- some pIdentifier
    symbol "->"
    body <- pComputation Seq
    return $ CReturn $ desugarVars vars body
  where
    desugarVars [v] b    = VFun v b
    desugarVars (v:vs) b = VFun v (CReturn (desugarVars vs b))
    desugarVars [] _     = error "fun with no arguments, this should have failed at the parser"

pPair :: Parser ValComp
pPair = parens $ do
  x1 <- pValComp Seq
  symbol ","
  x2 <- pValComp Seq
  return $ VCPair x1 x2

pHandler :: Parser Value
pHandler = VHandler <$> (symbol "handler" *> braces pHandlerBody)

pHandlerBody :: Parser Handler
pHandlerBody = do
  clauses <- sepBy (pReturnClause <|> pOpClause) (symbol ",")
  let opClauses = foldl' extractOps [] clauses
  case find isLeft clauses of
    Just (Left retClause) -> return $ Handler retClause opClauses
    Nothing               -> fail   $ "no return in handler"
  where
    isLeft (Left _) = True
    isLeft _        = False
    extractOps acc (Right op) = op : acc
    extractOps acc _          = acc
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
  body <- pComputation Seq
  return $ Right (opName, param, k, body)

pClauseBody :: Parser (VarName, Computation)
pClauseBody = do
  var <- pIdentifier
  symbol "->"
  body <- pComputation Seq
  return (var, body)

pInfixOps :: [Text] -> Parser Computation
pInfixOps ops = do
    x  <- pValComp App
    op <- choice (fmap symbol ops)
    x' <- pValComp App
    xs <- many (symbol op >> pValComp App)
    return $ foldInfix op (x:x':xs)
  where
    foldInfix :: Text -> [ValComp] -> Computation
    foldInfix op (x:y:z:xs) = foldInfix op ((C (desugar [x,y] (sequenceInfix op))) : z : xs)
    foldInfix op [x,y]      = desugar [x,y] (sequenceInfix op)
    foldInfix _  _          = error "infix operator with less than 2 arguments, this should have failed at the parser"
    sequenceInfix op [v1, v2] = CSeq "_f" (CApp (VVar (unpack op)) v1) (CApp (VVar "_f") v2)

pSeq :: Parser Computation
pSeq = do
  x  <- pComputation Infix
  xs <- some (symbol ";" >> pComputation Infix)
  return $ foldl1' (\c1 c2 -> CSeq "_" c1 c2) (x:xs)

pReturn :: Parser Computation
pReturn = do
  symbol "return"
  x <- pValComp Paren
  return $ desugar [x] (\[v] -> CReturn v)

pDo :: Parser Computation
pDo = do
  symbol "do"
  var <- pIdentifier
  symbol "<-"
  c1 <- pComputation Infix
  symbol "in"
  c2 <- pComputation Infix
  return $ CSeq var c1 c2

pWith :: Parser Computation
pWith = do
  symbol "with"
  h <- pHandler
  symbol "handle"
  c <- pComputation Seq
  case h of
    VHandler handler -> return $ CHandle handler c
    _ -> fail "Expected a handler value in 'with' expression"

pIf :: Parser Computation
pIf = do
  symbol "if"
  x <- pValComp Infix
  symbol "then"
  c1 <- pComputation Infix
  symbol "else"
  c2 <- pComputation Infix
  return $ desugar [x] (\[v] -> CIf v c1 c2)

pOp :: Parser Computation
pOp = do
  symbol "#"
  op <- pIdentifier
  x <- pValComp Paren
  return $ desugar [x] (\[v] -> COp op v)

pApp :: Parser Computation
pApp = do
    f <- (try (V <$> pVar) <|> parens (pValComp Seq))
    vs <- some (pValComp Paren)
    return $ desugarApp f vs
  where
    desugarApp f (x:xs) = foldl' (\acc x -> desugar [C acc, x] appVec2) (desugar [f,x] appVec2) xs
    desugarApp _ []     = error "fun with no arguments, this should have failed at the parser"
    appVec2 [v1,v2] = CApp v1 v2

pProgram :: Parser Computation
pProgram = between sc eof (pComputation Seq)

parseProgram :: String -> String -> Either (ParseErrorBundle Text Void) Computation
parseProgram filename content = runParser pProgram filename (pack content)
