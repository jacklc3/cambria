{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Inference.Infer where

import Control.Monad (foldM, unless)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax
import Inference.Types
import Inference.Substitutable
import Inference.Unify (unify)
import Inference.Initialisation (initialCtx)

infer :: Computation -> Either String CompType
infer c = runInfer initialCtx (inferComp c)

fresh :: Infer ValueType
fresh = do
  n <- get
  modify succ
  return $ TVar $ "t" ++ show n

lookupVar :: Ident -> Infer ValueType
lookupVar x = do
  variables <- asks variables
  case Map.lookup x variables of
    Just scheme -> instantiate scheme
    Nothing     -> throwError $ "Unbound variable: " ++ x

lookupOp :: Op -> Infer Arity
lookupOp op = do
  effects <- asks abilities
  case Map.lookup op effects of
    Just arity -> return arity
    Nothing    -> throwError $ "Unknown operation: " ++ op

instantiate :: Scheme -> Infer ValueType
instantiate (Forall as t) = do
  s <- traverse (const fresh) (Map.fromSet id as)
  return $ apply s t

extendVariable :: Ident -> Scheme -> Infer a -> Infer a
extendVariable x sc = local (\ctx -> ctx{ variables = Map.insert x sc (variables ctx) })

extendAbilities :: Effects -> Infer a -> Infer a
extendAbilities effects = local (\ctx -> ctx{ abilities = effects <> abilities ctx })

addEffects :: Effects -> CompType -> CompType
addEffects es t = t{ effects = es <> effects t}

inferComp :: Computation -> Infer CompType
inferComp = \case
  CReturn v -> do
    tv <- inferValue v
    return $ TComp tv mempty
  CApp f v -> do
    tf <- inferValue f
    case tf of
      TFun t1 t2 -> do
        tv <- inferValue v
        s <- unify t1 tv
        return $ apply s t2
      _ -> throwError $ "Applying non-function type: " ++ show tf
  COp op v -> do
    ar <- lookupOp op
    tv <- inferValue v
    s <- unify tv (arg ar)
    return $ apply s (TComp (ret ar) (Map.singleton op ar))
  CDo x c1 c2 -> do
    t1 <- inferComp c1
    t2 <- extendVariable x (Forall mempty (value t1)) (inferComp c2)
    return $ addEffects (effects t1) t2
  CIf v c1 c2 -> do
    checkValue v TBool
    t1 <- inferComp c1
    t2 <- inferComp c2
    s <- unify t1 t2
    return $ apply s (addEffects (effects t2) t1)
  CCase v x1 c1 x2 c2 -> do
    tv <- inferValue v
    case tv of
      TEither tl tr -> do
        t1 <- extendVariable x1 (Forall mempty tl) (inferComp c1)
        t2 <- extendVariable x2 (Forall mempty tr) (inferComp c2)
        s <- unify t1 t2
        return $ apply s (addEffects (effects t2) t1)
      _ -> throwError $ "Case analysis on non-either type: " ++ show tv
  CHandle v c -> do
    tv <- inferValue v
    case tv of
      THandler tIn tOut -> do
        tc <- extendAbilities (effects tIn) (inferComp c)
        s <- unify tc tIn
        return $ apply s (addEffects (effects tc Map.\\ effects tIn) tOut)
      _ -> throwError $ "Handling with non-handler type: " ++ show tv

checkComp :: CompType -> Computation -> Infer ()
checkComp t c = do
  t' <- inferComp c
  s <- unify t t'
  checkEffectUsage (apply s t') (apply s t)

checkEffectUsage :: CompType -> CompType -> Infer ()
checkEffectUsage t1 t2 = do
  unless (effects t1 `Map.isSubmapOf` effects t2) $
    throwError $ "Computation " ++ show t1 ++ " incompatible with " ++ show t2

inferValue :: Value -> Infer ValueType
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
    t2 <- extendVariable x (Forall mempty t1) (inferComp c)
    return $ TFun t1 t2
  VRec f x c -> do
    t1 <- fresh
    t2 <- fresh
    let tf = TFun t1 (TComp t2 mempty)
    tBody <- extendVariable f (Forall mempty tf)
      $ extendVariable x (Forall mempty t1)
      $ inferComp c
    s <- unify tBody (TComp t2 mempty)
    return $ apply s tf
  VHandler (Handler (RetClause xr cr) opClauses finallyClause) -> do
    hInVal <- fresh
    hOutVal <- fresh
    retOut <- extendVariable xr (Forall mempty hInVal) (inferComp cr)
    s1 <- unify retOut (TComp hOutVal mempty)

    let processOpClause (ops, hOut, s) (op, OpClause x k cOp) = do
          opArg <- fresh
          opRet <- fresh
          opOut <- extendVariable x (Forall mempty opArg) $
            extendVariable k (Forall mempty (TFun opRet hOut)) $
            inferComp cOp
          s' <- unify opOut hOut
          let newS = s' <> s
          let newOps = apply newS (Map.insert op (Arity opArg opRet) ops)
          let newHOut = apply newS (addEffects (effects opOut) hOut)
          return (newOps, newHOut, newS)

    (ops, opsOut, s2) <- foldM processOpClause (mempty, apply s1 retOut, s1) opClauses

    (finOut, s3) <- case finallyClause of
      Nothing -> return (opsOut, s2)
      Just (FinClause xf cf) -> do
        finOut <- extendVariable xf (Forall mempty (value opsOut)) (inferComp cf)
        s <- unify (effects finOut) (effects opsOut)
        return (finOut, s)
    return $ apply s3 (THandler (TComp (apply s2 hInVal) ops) (addEffects (effects opsOut) finOut))
  VPrimitive _ -> throwError "Cannot typecheck runtime primitive"
  VClosure _ _ _ -> throwError "Cannot typecheck runtime closure"

checkValue :: Value -> ValueType -> Infer ()
checkValue v t = do
  t' <- inferValue v
  unify t' t
  return ()
