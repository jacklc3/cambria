{-# LANGUAGE LambdaCase #-}

module Inference.Infer where

import Control.Monad (foldM, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, local)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax
import Inference.Monad
import Inference.Unify (unify)
import Inference.Context
import Inference.Effects
import Inference.Substitutable

infer :: Computation -> Either String CompType
infer c = runInfer initialCtx $ do
  tc <- inferComp c
  let prims = Map.fromList primitiveOps
  mapM_ (\(op, ar) -> case Map.lookup op prims of
    Just ar' -> unify ar ar'
    Nothing  -> return ()) (Map.toList (effectOps (effects tc)))
  tc' <- applySubst tc
  return tc'{ effects = closeEffects (effects tc') }
    where
      closeEffects (Closed m) = Closed m
      closeEffects (Open m _) = Closed m

lookupVariable :: Ident -> Infer ValueType
lookupVariable x = do
  variables <- ask
  case Map.lookup x variables of
    Just sc -> do
      t <- instantiate sc
      applySubst t
    Nothing     -> throwError $ "Unbound variable: " ++ x

instantiate :: Scheme -> Infer ValueType
instantiate (Forall tv ev t) = do
  st <- traverse (const fresh) (Map.fromSet id tv)
  se <- traverse (const (freshEffects mempty)) (Map.fromSet id ev)
  return $ apply (Effect se) $ apply (Type st) t

extendVariable :: Ident -> Scheme -> Infer a -> Infer a
extendVariable x sc = local (Map.insert x sc)

generalizeComp :: CompType -> Infer Scheme
generalizeComp tc = do
  ctx <- ask
  ctx' <- applySubst ctx
  tc' <- applySubst tc
  let genTV = free TV (value tc') Set.\\ (free TV ctx' <> free TV (effects tc'))
      genEV = free EV (value tc') Set.\\ (free EV ctx' <> free EV (effects tc'))
  return $ Forall genTV genEV (value tc')

inferComp :: Computation -> Infer CompType
inferComp = \case
  CReturn v -> do
    tv <- inferValue v
    e <- freshEffects mempty
    return $ TComp tv e
  CApp f v -> do
    tf <- inferValue f
    t1 <- fresh
    t2 <- fresh
    e2 <- freshEffects mempty
    unify tf (TFun t1 (TComp t2 e2))
    t1' <- applySubst t1
    checkValue v t1'
    applySubst (TComp t2 e2)
  COp op v -> do
    tv <- inferValue v
    t1 <- fresh
    e1 <- freshEffects mempty
    applySubst (TComp t1 (addEffectOps (Map.singleton op (Arity tv t1)) e1))
  CDo x c1 c2 -> do
    t1 <- inferComp c1
    sc <- generalizeComp t1
    t2 <- extendVariable x sc (inferComp c2)
    unify (effects t1) (effects t2)
    applySubst t2
  CIf v c1 c2 -> do
    checkValue v TBool
    t1 <- inferComp c1
    t2 <- inferComp c2
    unify t1 t2
    applySubst t1
  CCase v x1 c1 x2 c2 -> do
    tl <- fresh
    tr <- fresh
    checkValue v (TEither tl tr)
    tl' <- applySubst tl
    tr' <- applySubst tr
    t1 <- extendVariable x1 (Forall mempty mempty tl') (inferComp c1)
    t2 <- extendVariable x2 (Forall mempty mempty tr') (inferComp c2)
    unify t1 t2
    applySubst t1
  CDeclare op ar c -> do
    tc <- inferComp c
    case Map.lookup op (effectOps (effects tc)) of
      Just ar' -> unify ar ar' >> applySubst tc
      Nothing -> return tc
  CHandle v c -> do
    tv <- inferValue v
    tInVal <- fresh
    tOutVal <- fresh
    eIn <- freshEffects mempty
    eOut <- freshEffects mempty
    unify tv (THandler (TComp tInVal eIn) mempty (TComp tOutVal eOut))
    ~(THandler tIn ps tOut) <- applySubst tv
    tc <- inferComp c
    let missingOps = Map.filterWithKey (\op ar ->
          not (Map.keysSet ps `Set.disjoint` free PV ar)
          && op `Map.notMember` effectOps (effects tIn)) (effectOps (effects tc))
    unless (Map.null missingOps) $
      throwError $ "Handler instantiates type parameters " ++ show (Map.keys ps)
        ++ " but does not handle operations " ++ showOps missingOps
    unify (nullifyEffects (effects tIn)) (effects tOut)
    tIn' <- applySubst tIn
    unify (apply (Parameter ps) tc) tIn'
    applySubst tOut

inferValue :: Value -> Infer ValueType
inferValue = \case
  VVar x    -> lookupVariable x
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
    t2 <- extendVariable x (Forall mempty mempty t1) (inferComp c)
    applySubst (TFun t1 t2)
  VRec f x c -> do
    t1 <- fresh
    t2 <- fresh
    e2 <- freshEffects mempty
    tBody <- extendVariable f (Forall mempty mempty (TFun t1 (TComp t2 e2)))
             $ extendVariable x (Forall mempty mempty t1)
             $ inferComp c
    unify tBody (TComp t2 e2)
    applySubst (TFun t1 tBody)
  VHandler (Handler (RetClause xr cr) opClauses finClause pSubst) -> do
    let processOpClause (ops, hOut) (op, OpClause x k cOp) = do
          opArg <- fresh
          opRet <- fresh
          opOut <- extendVariable x (Forall mempty mempty opArg)
                   $ extendVariable k (Forall mempty mempty (TFun opRet hOut))
                   $ inferComp cOp
          unify opOut hOut
          hOut' <- applySubst hOut
          ops' <- applySubst (Map.insert op (Arity opArg opRet) ops)
          return (ops', hOut')

    hInVal <- fresh
    retOut <- extendVariable xr (Forall mempty mempty hInVal) (inferComp cr)
    (ops, opsOut) <- foldM processOpClause (mempty, retOut) opClauses
    eIn <- freshEffects ops
    finOut <- case finClause of
      Nothing -> return opsOut
      Just (FinClause xf cf) -> do
        finOut <- extendVariable xf (Forall mempty mempty (value opsOut)) (inferComp cf)
        unify (effects finOut) (effects opsOut)
        applySubst finOut

    applySubst (THandler (TComp hInVal eIn) (Map.fromList pSubst) finOut)
  VPrimitive _ -> throwError "Cannot typecheck runtime primitive"
  VClosure _ _ _ -> throwError "Cannot typecheck runtime closure"

checkValue :: Value -> ValueType -> Infer ()
checkValue v t = do
  t' <- inferValue v
  unify t' t
