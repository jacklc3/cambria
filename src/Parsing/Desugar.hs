{-# LANGUAGE LambdaCase #-}

module Parsing.Desugar (
  desugar
) where

import Syntax
import Parsing.SugaredSyntax

import Control.Monad (foldM)
import Control.Monad.State (State, evalState, get, modify)

type Fresh a = State Integer a

fresh :: Fresh Ident
fresh = do
  n <- get
  modify succ
  return ("_" ++ show n)

desugar :: SugaredExpr -> Computation
desugar (SEComp c) = evalState (desugarComp c) 0
desugar v          = evalState (desugarComp (SCReturn v)) 0

desugarComp :: SugaredComp -> Fresh Computation
desugarComp = \case
  SCReturn e -> do
    (k, v) <- desugarExpr e
    return $ k (CReturn v)
  SCOp op e  -> do
    (k, v) <- desugarExpr e
    return $ k (COp op v)
  SCDo p s1 s2 -> do
    c1 <- desugarComp s1
    c2 <- desugarComp s2
    (x, c2') <- desugarPattern p c2
    return $ CDo x c1 c2'
  SCIf e s1 s2 -> do
    (k, v) <- desugarExpr e
    c1 <- desugarComp s1
    c2 <- desugarComp s2
    return $ k (CIf v c1 c2)
  SCCase e (p1, s1) (p2, s2) -> do
    (k1, v) <- desugarExpr e
    c1 <- desugarComp s1
    (x1, c1') <- desugarPattern p1 c1
    c2 <- desugarComp s2
    (x2, c2') <- desugarPattern p2 c2
    return $ k1 (CCase v x1 c1' x2 c2')
  SCApp e1 e2 -> do
    (k1, v1) <- desugarExpr e1
    (k2, v2) <- desugarExpr e2
    return $ (k1 . k2) (CApp v1 v2)
  SCWith e s -> do
    (k, v) <- desugarExpr e
    c <- desugarComp s
    return $ k (CHandle v c)
  SCDeclare op ar s -> do
    c <- desugarComp s
    return (CDeclare op ar c)

desugarExpr :: SugaredExpr -> Fresh (Computation -> Computation, Value)
desugarExpr = \case
  SEVar x     -> return (id, VVar x)
  SEUnit      -> return (id, VUnit)
  SEBool b    -> return (id, VBool b)
  SEInt i     -> return (id, VInt i)
  SEString s  -> return (id, VString s)
  SEPair e1 e2 -> do
    (k1, v1) <- desugarExpr e1
    (k2, v2) <- desugarExpr e2
    return (k1 . k2, VPair v1 v2)
  SEEither s e -> do
    (k, v) <- desugarExpr e
    return (k, VEither s v)
  SEHandler cs -> do
    handler <- desugarHandler cs
    return (id, VHandler handler)
  SEFun (p:ps) s -> do
    c <- desugarComp s
    c1 <- desugarArguments ps c
    (x, c2) <- desugarPattern p c1
    return (id, VFun x c2)
  SERec g (p:ps) s -> do
    c <- desugarComp s
    c1 <- desugarArguments ps c
    (x, c2) <- desugarPattern p c1
    return (id, VRec g x c2)
  SEComp s -> do
    tmp  <- fresh
    c <- desugarComp s
    return (CDo tmp c, VVar tmp)

desugarHandler :: [HandlerClause] -> Fresh Handler
desugarHandler cs = do
  (rc, ocs, fc, ps) <- foldM f (Nothing,[],Nothing,[]) cs
  rc' <- case rc of
    Just rc -> return rc
    Nothing -> do
      tmp <- fresh
      return $ RetClause tmp (CReturn (VVar tmp))
  return $ Handler rc' ocs fc ps
    where
      f (_, ocs, fc, ps) (RC p s) = do
        c <- desugarComp s
        (x, c') <- desugarPattern p c
        return (Just (RetClause x c'), ocs, fc, ps)
      f (rc, ocs, fc, ps) (OC op p f s) = do
        c <- desugarComp s
        (x, c') <- desugarPattern p c
        return (rc, (op, OpClause x f c') : ocs, fc, ps)
      f (rc, ocs, _, ps) (FC p s) = do
        c <- desugarComp s
        (x, c') <- desugarPattern p c
        return (rc, ocs, Just (FinClause x c'), ps)
      f (rc, ocs, fc, ps) (TC p t) =
        return (rc, ocs, fc, (p, t) : ps)

desugarPattern :: Pattern -> Computation -> Fresh (Ident, Computation)
desugarPattern PWild c = return ("_", c)
desugarPattern (PVar x) c = return (x, c)
desugarPattern (PPair p1 p2) c = do
  tmp <- fresh
  (x2, c1) <- desugarPattern p2 c
  (x1, c2) <- desugarPattern p1 c1
  return (tmp, CDo x1 (CApp (VVar "fst") (VVar tmp)) $
    CDo x2 (CApp (VVar "snd") (VVar tmp)) c2)

desugarArguments :: [Pattern] -> Computation -> Fresh Computation
desugarArguments [] c = return c
desugarArguments (p:ps) c = do
  c1 <- desugarArguments ps c
  (x, c2) <- desugarPattern p c1
  return (CReturn (VFun x c2))
