{-# LANGUAGE LambdaCase #-}

module Inference.Infer where

import Control.Monad (foldM, unless)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax
import Inference.Substitutable
import Inference.Unify (unify, applySubst)
import Inference.Initialisation (initialCtx)

infer :: Computation -> Either String CompType
infer c = runInfer initialCtx (inferComp c)

fresh :: Infer ValueType
fresh = do
  st <- get
  put st { count = count st + 1 }
  return $ TVar $ "t" ++ show (count st)

lookupVar :: Ident -> Infer ValueType
lookupVar x = do
  variables <- asks variables
  case Map.lookup x variables of
    Just scheme -> do
      t <- instantiate scheme
      applySubst t
    Nothing     -> throwError $ "Unbound variable: " ++ x

lookupOp :: Op -> Infer Arity
lookupOp op = do
  effects <- asks abilities
  case Map.lookup op effects of
    Just arity -> applySubst arity
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
    tArg <- fresh
    tRet <- fresh
    unify tf (TFun tArg (TComp tRet mempty))
    tArg' <- applySubst tArg
    checkValue v tArg'
    ~(TFun _ ct) <- applySubst tf
    return ct
  COp op v -> do
    ar <- lookupOp op
    checkValue v (arg ar)
    applySubst (TComp (ret ar) (Map.singleton op ar))
  CDo x c1 c2 -> do
    t1 <- inferComp c1
    t2 <- extendVariable x (Forall mempty (value t1)) (inferComp c2)
    applySubst (addEffects (effects t1) t2)
  CIf v c1 c2 -> do
    checkValue v TBool
    t1 <- inferComp c1
    t2 <- inferComp c2
    unify t1 t2
    applySubst (addEffects (effects t2) t1)
  CCase v x1 c1 x2 c2 -> do
    tl <- fresh
    tr <- fresh
    checkValue v (TEither tl tr)
    tl' <- applySubst tl
    tr' <- applySubst tr
    t1 <- extendVariable x1 (Forall mempty tl') (inferComp c1)
    t2 <- extendVariable x2 (Forall mempty tr') (inferComp c2)
    unify t1 t2
    applySubst (addEffects (effects t2) t1)
  CDeclare op arg ret c -> do
    extendAbilities (Map.singleton op (Arity arg ret)) (inferComp c)
  CHandle v c -> do
    tv <- inferValue v
    -- Extract type instantiations from handler value
    let tInsts = case v of
          VHandler h -> typeInsts h
          _          -> []
        pSubst = Map.fromList tInsts
    tInVal <- fresh
    tOutVal <- fresh
    unify tv (THandler (TComp tInVal mempty) (TComp tOutVal mempty))
    ~(THandler tIn tOut) <- applySubst tv
    tc <- extendAbilities (effects tIn) (inferComp c)
    -- Apply param substitution to body effects before unifying
    let tc' = applyParams pSubst tc
    -- Check that all ops mentioning instantiated params are handled
    let affectedOps = Map.keysSet $ Map.filter
          (\ar -> not $ Set.null $ ftp ar `Set.intersection` Map.keysSet pSubst)
          (effects tc)
        handledOps = Map.keysSet (effects tIn)
        missingOps = affectedOps Set.\\ handledOps
    unless (Set.null missingOps) $
      throwError $ "Handler instantiates type parameter(s) but does not handle operation(s): "
        ++ show (Set.toList missingOps)
    unify tc' tIn
    applySubst (addEffects (effects tc' Map.\\ effects tIn) tOut)

checkComp :: CompType -> Computation -> Infer ()
checkComp expected c = do
  t <- inferComp c
  unify expected t
  t' <- applySubst t
  ctx <- asks abilities
  let illegal = effects t' Map.\\ (effects expected <> ctx)
  unless (Map.null illegal) $
    throwError $ "Unexpected effects: " ++ show (TComp (value t') illegal)
      ++ " not in " ++ show expected

inferValue :: Value -> Infer ValueType
inferValue = \case
  VVar x    -> lookupVar x
  VInt _    -> return TInt
  VBool _   -> return TBool
  VString _ -> return TString
  VDouble _ -> return TDouble
  VUnit     -> return TUnit
  VUnique _ -> return TUnique
  VPair v1 v2 -> do
    t1 <- inferValue v1
    t2 <- inferValue v2
    applySubst (TPair t1 t2)
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
    applySubst (TFun t1 t2)
  VRec f x c -> do
    t1 <- fresh
    t2 <- fresh
    tBody <- extendVariable f (Forall mempty (TFun t1 (TComp t2 mempty)))
             $ extendVariable x (Forall mempty t1)
             $ inferComp c
    unify (value tBody) t2
    applySubst (TFun t1 tBody)
  VHandler (Handler (RetClause xr cr) opClauses finClause _) -> do
    let processOpClause (ops, hOut) (op, OpClause x k cOp) = do
          opArg <- fresh
          opRet <- fresh
          opOut <- extendVariable x (Forall mempty opArg)
                   $ extendVariable k (Forall mempty (TFun opRet hOut))
                   $ inferComp cOp
          unify opOut hOut
          hOut' <- applySubst (addEffects (effects opOut) hOut)
          ops' <- applySubst (Map.insert op (Arity opArg opRet) ops)
          return (ops', hOut')

    hInVal <- fresh
    retOut <- extendVariable xr (Forall mempty hInVal) (inferComp cr)
    (ops, opsOut) <- foldM processOpClause (mempty, retOut) opClauses
    finOut <- case finClause of
      Nothing -> return opsOut
      Just (FinClause xf cf) -> do
        finOut <- extendVariable xf (Forall mempty (value opsOut)) (inferComp cf)
        unify (effects finOut) (effects opsOut)
        return $ addEffects (effects opsOut) finOut
    applySubst (THandler (TComp hInVal ops) finOut)
  VPrimitive _ -> throwError "Cannot typecheck runtime primitive"
  VClosure _ _ _ -> throwError "Cannot typecheck runtime closure"

checkValue :: Value -> ValueType -> Infer ()
checkValue (VFun x c) (TFun tv tc)     = extendVariable x (Forall mempty tv) (checkComp tc c)
checkValue (VRec f x c) (TFun tv tc)   = extendVariable f (Forall mempty (TFun tv tc))
                                         $ extendVariable x (Forall mempty tv)
                                         $ checkComp tc c
checkValue (VPair v1 v2) (TPair t1 t2) = checkValue v1 t1 >> checkValue v2 t2
checkValue (VEither L v) (TEither t _) = checkValue v t
checkValue (VEither R v) (TEither _ t) = checkValue v t
checkValue v t = do
  t' <- inferValue v
  unify t' t
