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
desugar s = evalState (desugarComp (liftValue s)) 0
  where
    liftValue (SEComp c) = c
    liftValue v          = SCReturn v

-- | Given a pattern and the name of a variable holding the matched value,
--   wrap a body computation with bindings for all pattern variables.
desugarPatBind :: SugaredPattern -> Ident -> Computation -> Fresh Computation
desugarPatBind (SPVar x) v body
  | x == v    = return body
  | otherwise = return $ CDo x (CReturn (VVar v)) body
desugarPatBind SPWild _ body = return body
desugarPatBind (SPPair p1 p2) v body = do
  t1 <- fresh
  t2 <- fresh
  inner <- desugarPatBind p2 t2 body
  outer <- desugarPatBind p1 t1 inner
  return $ CDo t1 (CApp (VVar "fst") (VVar v))
         $ CDo t2 (CApp (VVar "snd") (VVar v))
         $ outer

-- | Decompose a pattern into a variable name and a body-wrapping function.
desugarPat :: SugaredPattern -> Fresh (Ident, Computation -> Fresh Computation)
desugarPat (SPVar x) = return (x, return)
desugarPat SPWild    = do tmp <- fresh; return (tmp, return)
desugarPat pat       = do tmp <- fresh; return (tmp, desugarPatBind pat tmp)

-- | Apply a pattern to a body, then build a result with the bound name.
withPat :: SugaredPattern -> Computation -> (Ident -> Computation -> a) -> Fresh a
withPat pat body k = do
  (x, wrap) <- desugarPat pat
  body' <- wrap body
  return (k x body')

desugarComp :: SugaredComp -> Fresh Computation
desugarComp = \case
  SCReturn e -> desugarExpr CReturn e
  SCOp op e  -> desugarExpr (COp op) e
  SCDo pat c1 c2 -> do
    c1' <- desugarComp c1
    c2' <- desugarComp c2
    withPat pat c2' (\x body -> CDo x c1' body)
  SCIf e c1 c2 -> do
    c1' <- desugarComp c1
    c2' <- desugarComp c2
    desugarExpr (\v -> CIf v c1' c2') e
  SCCase e (x1, c1) (x2, c2) -> do
    c1' <- desugarComp c1
    c2' <- desugarComp c2
    desugarExpr (\v -> CCase v x1 c1' x2 c2') e
  SCApp e e' -> desugarApp CApp e e'
  SCWith e c -> do
    c' <- desugarComp c
    desugarExpr (\v -> CHandle v c') e
  SCDeclare op tArg tRet c -> do
    c' <- desugarComp c
    return (CDeclare op tArg tRet c')

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
  SEFun (p:ps) c -> do
    c' <- desugarComp c
    body <- desugarPatVars ps c'
    v <- withPat p body VFun
    return (id, v)
  SERec g (p:ps) c -> do
    c' <- desugarComp c
    body <- desugarPatVars ps c'
    v <- withPat p body (\x b -> VRec g x b)
    return (id, v)
  SEComp c -> do
    x  <- fresh
    c' <- desugarComp c
    return (CDo x c', VVar x)

-- | Desugar a list of function argument patterns into curried functions.
desugarPatVars :: [SugaredPattern] -> Computation -> Fresh Computation
desugarPatVars ps c = foldrM (\p c' -> CReturn <$> withPat p c' VFun) c ps

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ z []     = return z
foldrM f z (x:xs) = do
  z' <- foldrM f z xs
  f x z'

desugarHandler :: [HandlerClause] -> Fresh Handler
desugarHandler cs = do
  (rc, ocs, fc, tcs) <- foldM f (Nothing,[],Nothing,[]) cs
  rc' <- case rc of
    Just rc -> return rc
    Nothing -> do
      x <- fresh
      return $ RetClause x (CReturn (VVar x))
  return $ Handler rc' ocs fc tcs
    where
      f (_, ocs, fc, tcs) (RC pat c) = do
        c' <- desugarComp c
        rc' <- withPat pat c' RetClause
        return (Just rc', ocs, fc, tcs)
      f (rc, ocs, fc, tcs) (OC op pat k c) = do
        c' <- desugarComp c
        oc' <- withPat pat c' (\x b -> OpClause x k b)
        return (rc, (op, oc'):ocs, fc, tcs)
      f (rc, ocs, _, tcs) (FC pat c) = do
        c' <- desugarComp c
        fc' <- withPat pat c' FinClause
        return (rc, ocs, Just fc', tcs)
      f (rc, ocs, fc, tcs) (TC name ty) =
        return (rc, ocs, fc, (name, ty) : tcs)
