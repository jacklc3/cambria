{-# LANGUAGE LambdaCase #-}

module Parser.Desugar where

import Control.Monad.State

import Ast
import Parser.SugaredAst

type Fresh a = State Integer a

newVar :: Fresh Ident
newVar = do
  n <- get
  modify succ
  return ("_" ++ show n)

desugar :: SugaredExpr -> Computation
desugar s = evalState (desugarComp (makeComp s)) 0
  where
    makeComp (SEComp c) = c
    makeComp v          = SCReturn v

desugarComp :: SugaredComp -> Fresh Computation
desugarComp = \case
  SCReturn e -> desugarExpr CReturn e
  SCOp op e  -> desugarExpr (COp op) e
  SCDo x c1 c2 -> do
    c1' <- desugarComp c1
    c2' <- desugarComp c2
    return (CDo x c1' c2')
  SCIf e c1 c2 -> do
    c1' <- desugarComp c1
    c2' <- desugarComp c2
    desugarExpr (\v -> CIf v c1' c2') e
  SCCase e (x1, c1) (x2, c2) -> do
    c1' <- desugarComp c1
    c2' <- desugarComp c2
    desugarExpr (\v -> CCase v x1 c1' x2 c2') e
  SCApp e [e'] -> desugarApp CApp e e'
  SCApp e (e':es) -> desugarComp (SCApp (SEComp (SCApp e [e'])) es)
  SCWith e c -> do
    c' <- desugarComp c
    desugarExpr (\v -> CHandle v c') e

desugarApp :: (Value -> Value -> Computation) -> SugaredExpr -> SugaredExpr -> Fresh Computation
desugarApp f e1 e2 = do
  (k1, v1) <- desugarExpr' e1
  (k2, v2) <- desugarExpr' e2
  return (k1 (k2 (f v1 v2)))

desugarExpr :: (Value -> Computation) -> SugaredExpr -> Fresh Computation
desugarExpr f e = do
  (k, v) <- desugarExpr' e
  return (k (f v))

desugarExpr' :: SugaredExpr -> Fresh (Computation -> Computation, Value)
desugarExpr' = \case
  SEVar x     -> return (id, VVar x)
  SEUnit      -> return (id, VUnit)
  SEBool b    -> return (id, VBool b)
  SEInt i     -> return (id, VInt i)
  SEString s  -> return (id, VString s)
  SEPair e1 e2 -> do
    (k1, v1) <- desugarExpr' e1
    (k2, v2) <- desugarExpr' e2
    return (k1 . k2, VPair v1 v2)
  SEEither s e -> do
    (k, v) <- desugarExpr' e
    return (k, VEither s v)
  SEHandler cs -> do
    handler <- desugarHandler cs
    return (id, VHandler handler)
  SEFun (x:xs) c -> do
    c' <- desugarComp c
    return (id, VFun x (desugarVars xs c'))
  SERec g (x:xs) c -> do
    c' <- desugarComp c
    return (id, VRec g x (desugarVars xs c'))
  SEComp c -> do
    x  <- newVar
    c' <- desugarComp c
    return (CDo x c', VVar x)

desugarVars :: [Ident] -> Computation -> Computation
desugarVars xs c = foldr (\x c' -> CReturn (VFun x c')) c xs

desugarHandler :: [HandlerClause] -> Fresh Handler
desugarHandler cs = undefined
