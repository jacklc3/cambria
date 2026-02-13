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

-- | Desugar a do-binding with a pattern: if the pattern is a simple variable,
--   emit a plain CDo; otherwise bind to a temp and destructure.
desugarPatDo :: SugaredPattern -> Computation -> Computation -> Fresh Computation
desugarPatDo (SPVar x) c1 body = return $ CDo x c1 body
desugarPatDo SPWild c1 body = do
  tmp <- fresh
  return $ CDo tmp c1 body
desugarPatDo pat c1 body = do
  tmp <- fresh
  inner <- desugarPatBind pat tmp body
  return $ CDo tmp c1 inner

-- | Desugar a function argument pattern: if the pattern is a simple variable,
--   emit a plain VFun; otherwise bind to a temp and destructure.
desugarPatFun :: SugaredPattern -> Computation -> Fresh Value
desugarPatFun (SPVar x) body = return $ VFun x body
desugarPatFun pat body = do
  tmp <- fresh
  inner <- desugarPatBind pat tmp body
  return $ VFun tmp inner

desugarComp :: SugaredComp -> Fresh Computation
desugarComp = \case
  SCReturn e -> desugarExpr CReturn e
  SCOp op e  -> desugarExpr (COp op) e
  SCDo pat c1 c2 -> do
    c1' <- desugarComp c1
    c2' <- desugarComp c2
    desugarPatDo pat c1' c2'
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
    v <- desugarPatFun p body
    return (id, v)
  SERec g (p:ps) c -> do
    c' <- desugarComp c
    body <- desugarPatVars ps c'
    case p of
      SPVar x -> return (id, VRec g x body)
      _ -> do
        tmp <- fresh
        inner <- desugarPatBind p tmp body
        return (id, VRec g tmp inner)
  SEComp c -> do
    x  <- fresh
    c' <- desugarComp c
    return (CDo x c', VVar x)

-- | Desugar a list of function argument patterns into curried functions.
desugarPatVars :: [SugaredPattern] -> Computation -> Fresh Computation
desugarPatVars ps c = foldrM (\p c' -> CReturn <$> desugarPatFun p c') c ps

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
        rc' <- desugarPatRet pat c'
        return (Just rc', ocs, fc, tcs)
      f (rc, ocs, fc, tcs) (OC op pat k c) = do
        c' <- desugarComp c
        oc' <- desugarPatOp pat k c'
        return (rc, (op, oc'):ocs, fc, tcs)
      f (rc, ocs, _, tcs) (FC pat c) = do
        c' <- desugarComp c
        fc' <- desugarPatFin pat c'
        return (rc, ocs, Just fc', tcs)
      f (rc, ocs, fc, tcs) (TC name ty) =
        return (rc, ocs, fc, (name, ty) : tcs)

desugarPatRet :: SugaredPattern -> Computation -> Fresh RetClause
desugarPatRet (SPVar x) body = return $ RetClause x body
desugarPatRet pat body = do
  tmp <- fresh
  inner <- desugarPatBind pat tmp body
  return $ RetClause tmp inner

desugarPatOp :: SugaredPattern -> Ident -> Computation -> Fresh OpClause
desugarPatOp (SPVar x) k body = return $ OpClause x k body
desugarPatOp pat k body = do
  tmp <- fresh
  inner <- desugarPatBind pat tmp body
  return $ OpClause tmp k inner

desugarPatFin :: SugaredPattern -> Computation -> Fresh FinClause
desugarPatFin (SPVar x) body = return $ FinClause x body
desugarPatFin pat body = do
  tmp <- fresh
  inner <- desugarPatBind pat tmp body
  return $ FinClause tmp inner
