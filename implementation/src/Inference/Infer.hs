{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Inference.Infer where

import Control.Monad (unless, forM_)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax
import Inference.Types
import Inference.Substitutable
import Inference.Unify
import Inference.Initialisation (initialCtx)

infer :: Computation -> Either String Type
infer c = runInfer initialCtx (inferComp c)

fresh :: Infer Type
fresh = do
  n <- get
  modify succ
  return $ TVar $ "t" ++ show n

lookupVar :: Ident -> Infer Type
lookupVar x = do
  vars <- asks vars
  case Map.lookup x vars of
    Just scheme -> instantiate scheme
    Nothing     -> throwError $ "Unbound variable: " ++ x

lookupOp :: Op -> Infer Arity
lookupOp op = do
  effects <- asks effects
  case Map.lookup op effects of
    Just arity -> return arity
    Nothing    -> throwError $ "Unknown operation: " ++ op

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  let asIncList = Set.toAscList as
  as' <- mapM (const fresh) asIncList
  let s = Map.fromDistinctAscList $ zip asIncList as'
  return $ apply s t

extend :: Ident -> Scheme -> Infer a -> Infer a
extend x sc m = local (\ctx -> ctx{ vars = Map.insert x sc (vars ctx) }) m

withEffects :: Effects -> Infer a -> Infer a
withEffects es m = local (\ctx -> ctx{ effects = es }) m

effectsSubset :: Effects -> Effects -> Infer ()
effectsSubset e1 e2 =
  unless (e1 `Map.isSubmapOf` e2) $ 
    throwError $ "Requires effects " ++ show e2 ++ " supersceed available effects " ++ show e2

inferComp :: Computation -> Infer Type
inferComp = \case
  CReturn v -> inferValue v
  CApp f v -> do
    tf <- inferValue f
    tv <- inferValue v
    case tf of
      TFun t1 t2 e2 -> do
        effects <- asks effects
        effectsSubset e2 effects
        s <- unify t1 tv
        return $ apply s t2
      _ -> throwError $ "Applying non-function type: " ++ show tf
  COp op v -> do
    Arity tIn tOut <- lookupOp op
    checkValue v tIn
    return tOut
  CDo x c1 c2 -> do
    t1 <- inferComp c1
    extend x (Forall Set.empty t1) (inferComp c2)
  CIf v c1 c2 -> do
    checkValue v TBool
    t1 <- inferComp c1
    t2 <- inferComp c2
    s <- unify t1 t2
    return $ apply s t1
  CCase v x1 c1 x2 c2 -> do
    tv <- inferValue v
    case tv of
      TEither tl tr -> do
        t1 <- extend x1 (Forall Set.empty tl) (inferComp c1)
        t2 <- extend x2 (Forall Set.empty tr) (inferComp c2)
        s <- unify t1 t2
        return $ apply s t1
      _ -> throwError $ "Case analysis on non-either type: " ++ show tv
  CHandle v c -> do
    tv <- inferValue v
    case tv of
      THandler t1 e1 t2 e2 -> do
        effects <- asks effects
        effectsSubset e2 effects
        checkComp (e1 `Map.union` effects) t1 c
        return t2
      _ -> throwError $ "Handling with non-handler type: " ++ show tv

checkComp :: Effects -> Type -> Computation -> Infer ()
checkComp es t = \case
  CReturn v -> checkValue v t
  CDo x c1 c2 -> do
    t1 <- withEffects es (inferComp c1)
    extend x (Forall Set.empty t1) (checkComp es t c2)
  CHandle v c -> do
    tv <- inferValue v
    case tv of
      THandler t1 e1 t2 e2 -> do
        effectsSubset e2 es
        s <- unify t2 t
        checkComp (e1 `Map.union` es) t1 c
      _ -> throwError $ "Handliing with non-handler type: " ++ show tv
  c -> do
    t' <- withEffects es (inferComp c)
    unify t t'
    return ()

inferValue :: Value -> Infer Type
inferValue = \case
  VVar x    -> lookupVar x
  VInt _    -> return TInt
  VBool _   -> return TBool
  VString _ -> return TString
  VDouble _ -> return TDouble
  VUnit     -> return TUnit
  VName _   -> return TName
  VPair v1 v2 -> do
    t1 <- inferValue v1
    t2 <- inferValue v2
    return $ TPair t1 t2
  VEither L v1 -> do
    t1 <- inferValue v1
    t2 <- fresh
    return $ TEither t1 t2
  VEither R v2 -> do
    t1 <- fresh
    t2 <- inferValue v2
    return $ TEither t1 t2
  VFun x c -> do
    t1 <- fresh
    effects <- asks effects
    t2 <- extend x (Forall Set.empty t1) (withEffects effects (inferComp c))
    return $ TFun t1 t2 effects
  VRec f x c -> do
    t1 <- fresh
    t2 <- fresh
    effects <- asks effects
    let tf = TFun t1 t2 effects
    tBody <- extend f (Forall Set.empty tf)
      $ extend x (Forall Set.empty t1)
      $ withEffects effects (inferComp c)
    s <- unify tBody t2
    return $ apply s tf
  VHandler (Handler (RetClause xr cr) opClauses finallyClause) -> do
    hInType  <- fresh 
    hOutType <- fresh
    effects <- asks effects
    extend xr (Forall Set.empty hInType) (checkComp effects hOutType cr)
    let handledOps = Set.fromList (map fst opClauses)
    forM_ opClauses $ \(op, OpClause x k cop) -> do
      Arity tIn tOut <- lookupOp op
      extend x (Forall Set.empty tIn) $ 
        extend k (Forall Set.empty (TFun tOut hOutType effects)) $
          checkComp effects hOutType  cop
          -- This is wrong
    return $ THandler hInType effects hOutType effects
  VPrimitive _ -> throwError "Cannot typecheck runtime primitive"
  VClosure _ _ _ -> throwError "Cannot typecheck runtime closure"

checkValue :: Value -> Type -> Infer ()
checkValue v t = do
  t' <- inferValue v
  unify t' t
  return ()
