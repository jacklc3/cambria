{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProgram) where

import Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Either (partitionEithers)
import Data.List (foldl', foldl1', reverse)
import Data.Map (fromList)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)

type Parser = Parsec Void Text
data CompScope = Seq | Infix | App | Expr | Paren deriving (Eq, Ord)

data ValComp
  = V Value
  | C Computation
  | VCPair ValComp ValComp
  | VCEither Side ValComp

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
    "if", "then", "else", "with", "handle", "inl", "inr",
    "case", "of"
  ]

pIdentifier :: Parser String
pIdentifier = (lexeme . try) $ do
  name <- (:) <$> letterChar <*> many alphaNumChar
  if name `elem` reservedWords
    then fail $ "keyword " ++ show name ++ " cannot be an identifier"
    else return name

-- Takes a list of ValComp terms and desugars them by sequencing any computations
desugar :: [ValComp] -> ([Value] -> Computation) -> Computation
desugar xs f = gos xs [] 0
  where
    hiddenVar :: Int -> VarName
    hiddenVar n = "_" ++ show n
    gos :: [ValComp] -> [Value] -> Int -> Computation
    gos (x:xs) vs n = let (v, k, n') = go x n
                      in  k (gos xs (v:vs) n')
    gos []     vs n = f (reverse vs)
    go :: ValComp -> Int -> (Value, Computation -> Computation, Int)
    go (V v) n          = (v, id, n)
    go (C c) n          = (VVar (hiddenVar n), \c' -> CDo (hiddenVar n) c c', succ n)
    go (VCPair x y) n   = let (v1, k1, n1) = go x n
                              (v2, k2, n2) = go y n1
                          in  (VPair v1 v2, k1 . k2, n2)
    go (VCEither s x) n = let (v, k, n') = go x n
                          in  (VEither s v, k, n')

pComputation :: CompScope -> Parser Computation
pComputation cs = choice $ seq ++ infi ++ app ++ expr ++ [try (parens (pComputation Seq))]
  where
    seq  = if cs > Seq   then [] else [try pSeq]
    infi = if cs > Infix then [] else [try (pInfixOps ["++", "*", "+", "-", "=="])]
    app  = if cs > App   then [] else [try pApp]
    expr = if cs > Expr  then [] else [pIf, pCase, pDo, pWith, pFun, pOp, pReturn]


pValComp :: CompScope -> Parser ValComp
pValComp cs = choice
  [ try $ V <$> pUnit
  , try pPair
  , pEither
  , V <$> pBool
  , V <$> pInteger
  , V <$> pString
  , V <$> pHandler
  , try $ C <$> pComputation cs
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
    xs <- symbol "fun" *> some pIdentifier
    c <- symbol "->" *> pComputation Seq
    return $ CReturn $ desugarVars xs c
  where
    desugarVars [x]    c = VFun x c
    desugarVars (x:xs) c = VFun x (CReturn (desugarVars xs c))
    desugarVars []     _ = error "fun with no arguments, this should have failed at the parser"

pEither :: Parser ValComp
pEither = pInl <|> pInr
  where
    pInl = VCEither L <$> (symbol "inl" *> pValComp Paren)
    pInr = VCEither R <$> (symbol "inr" *> pValComp Paren)

pPair :: Parser ValComp
pPair = parens $ VCPair <$> pValComp Seq <* symbol "," <*> pValComp Seq

pHandler :: Parser Value
pHandler = VHandler <$> (symbol "handler" *> braces (do
  clauses <- sepBy (pRetClause <|> pOpClause) (symbol ",")
  let (retClauses, opClauses) = partitionEithers clauses
  case retClauses of
    [retClause] -> return $ Handler retClause (fromList opClauses)
    _           -> fail   $ "handler must have one return clause"))

pOpClause :: Parser (Either RetClause (OpName, OpClause))
pOpClause = do
  op <- pIdentifier
  (x, k) <- parens $ (,) <$> pIdentifier <* symbol ";" <*> pIdentifier
  c <- symbol "->" *> pComputation Seq
  return $ Right (op, OpClause x k c)

pRetClause :: Parser (Either RetClause (OpName, OpClause))
pRetClause = do
  x <- symbol "return" *> pIdentifier
  c <- symbol "->" *> pComputation Seq
  return $ Left (RetClause x c)

-- TODO: Make the order of operations matter
pInfixOps :: [Text] -> Parser Computation
pInfixOps ops = do
    x  <- pValComp App
    op <- choice (fmap symbol ops)
    x' <- pValComp App
    xs <- many (symbol op >> pValComp App)
    return $ foldInfix op (x:x':xs)
  where
    foldInfix op (x:y:z:xs) = foldInfix op ((C (desugar [x,y] (sequenceInfix op))) : z : xs)
    foldInfix op [x,y]      = desugar [x,y] (sequenceInfix op)
    foldInfix _  _          = error "infix operator with less than 2 arguments, this should have failed at the parser"
    sequenceInfix op [v1, v2] = CDo "_f" (CApp (VVar (unpack op)) v1) (CApp (VVar "_f") v2)

pSeq :: Parser Computation
pSeq = do
  x  <- pComputation Infix
  xs <- some (symbol ";" *> pComputation Infix)
  return $ foldl1' (\c1 c2 -> CDo "_" c1 c2) (x:xs)

pReturn :: Parser Computation
pReturn = do
  x <- symbol "return" *> pValComp Paren
  return $ desugar [x] (\[v] -> CReturn v)

pDo :: Parser Computation
pDo = do
  var <- symbol "do" *> pIdentifier
  c1 <- symbol "<-" *> pComputation Infix
  c2 <- symbol "in" *> pComputation Infix
  return $ CDo var c1 c2

pWith :: Parser Computation
pWith = do
  h <- symbol "with" *> pHandler
  c <- symbol "handle" *> pComputation Seq
  case h of
    VHandler handler -> return $ CHandle handler c
    _ -> fail "Expected a handler value in 'with' expression"

pIf :: Parser Computation
pIf = do
  x <- symbol "if" *> pValComp Infix
  c1 <- symbol "then" *> pComputation Infix
  c2 <- symbol "else" *> pComputation Infix
  return $ desugar [x] (\[v] -> CIf v c1 c2)

pCase :: Parser Computation
pCase = do
  x <- symbol "case" *> pValComp Infix <* symbol "of"
  ((y1,c1),(y2,c2)) <- braces $ leftRight <|> rightLeft
  return $ desugar [x] (\[v] -> CCase v y1 c1 y2 c2)
    where
      pIn s = do
        y <- symbol s *> pIdentifier
        c <- symbol "->" *> pComputation Seq
        return (y,c)
      leftRight = (,) <$> pIn "inl" <* symbol "," <*> pIn "inr"
      rightLeft = flip (,) <$> pIn "inr" <* symbol "," <*> pIn "inl"

pOp :: Parser Computation
pOp = do
  op <- symbol "#" *> pIdentifier
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
    appVec2 [v1,v2]     = CApp v1 v2

pProgram :: Parser Computation
pProgram = between sc eof (pComputation Seq)

parseProgram :: String -> String -> Either (ParseErrorBundle Text Void) Computation
parseProgram filename content = runParser pProgram filename (pack content)
