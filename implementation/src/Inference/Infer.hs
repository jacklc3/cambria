{-# LANGUAGE LambdaCase #-}

module Inference.Infer where

import Control.Monad (foldM, unless)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets, modify)
import Control.Monad.Reader (asks, local)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax
import Inference.Monad
import Inference.Unify (unify, applySubst)
import Inference.Context
import Inference.Substitutable

infer :: Computation -> Either String CompType
infer c = runInfer initialCtx $ do
  tc <- inferComp c
  unify (effects tc) (Map.fromList primitiveOps)
  applySubst tc

fresh :: Infer ValueType
fresh = do
  count <- gets count
  modify (\st ->  st { count = succ count } )
  return $ TVar $ "t" ++ show count

lookupVar :: Ident -> Infer ValueType
lookupVar x = do
  variables <- asks variables
  case Map.lookup x variables of
    Just scheme -> do
      t <- instantiate scheme
      applySubst t
    Nothing     -> throwError $ "Unbound variable: " ++ x

instantiate :: Scheme -> Infer ValueType
instantiate (Forall as t) = do
  s <- traverse (const fresh) (Map.fromSet id as)
  return $ apply Types s t

extendVariable :: Ident -> Scheme -> Infer a -> Infer a
extendVariable x sc =
  local (\ctx -> ctx{ variables = Map.insert x sc (variables ctx) })

mergeEffects :: Effects -> CompType -> Infer CompType
mergeEffects es t = do
  unify (effects t) es
  applySubst t{ effects = es <> effects t }

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
    tv <- inferValue v
    tRet <- fresh
    applySubst (TComp tRet (Map.singleton op (Arity tv tRet)))
  CDo x c1 c2 -> do
    t1 <- inferComp c1
    t2 <- extendVariable x (Forall mempty (value t1)) (inferComp c2)
    mergeEffects (effects t1) t2
  CIf v c1 c2 -> do
    checkValue v TBool
    t1 <- inferComp c1
    t2 <- inferComp c2
    unify t1 t2
    mergeEffects (effects t2) t1
  CCase v x1 c1 x2 c2 -> do
    tl <- fresh
    tr <- fresh
    checkValue v (TEither tl tr)
    tl' <- applySubst tl
    tr' <- applySubst tr
    t1 <- extendVariable x1 (Forall mempty tl') (inferComp c1)
    t2 <- extendVariable x2 (Forall mempty tr') (inferComp c2)
    unify t1 t2
    mergeEffects (effects t2) t1
  CDeclare op arg ret c -> do
    tc <- inferComp c
    unify (Map.singleton op (Arity arg ret)) (effects tc)
    applySubst tc
  CHandle v c -> do
    tv <- inferValue v
    tInVal <- fresh
    tOutVal <- fresh
    unify tv (THandler (TComp tInVal mempty) mempty (TComp tOutVal mempty))
    ~(THandler tIn ps tOut) <- applySubst tv
    tc <- inferComp c
    let missingOps = Map.filterWithKey (\op ar ->
          not (Map.keysSet ps `Set.disjoint` free Params ar)
          && op `Map.notMember` effects tIn) (effects tc)
    unless (Map.null missingOps) $
      throwError $ "Handler instantiates type parameters " ++ show (Map.keys ps)
        ++ " but does not handle operations " ++ showEffects missingOps
    let tc' = apply Params ps tc
    unify tc' tIn
    mergeEffects (effects tc' Map.\\ effects tIn) tOut

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
  VHandler (Handler (RetClause xr cr) opClauses finClause tInsts) -> do
    let processOpClause (ops, hOut) (op, OpClause x k cOp) = do
          opArg <- fresh
          opRet <- fresh
          opOut <- extendVariable x (Forall mempty opArg)
                   $ extendVariable k (Forall mempty (TFun opRet hOut))
                   $ inferComp cOp
          unify opOut hOut
          hOut' <- mergeEffects (effects opOut) hOut
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
        mergeEffects (effects opsOut) finOut
    applySubst (THandler (TComp hInVal ops) (Map.fromList tInsts) finOut)
  VPrimitive _ -> throwError "Cannot typecheck runtime primitive"
  VClosure _ _ _ -> throwError "Cannot typecheck runtime closure"

checkValue :: Value -> ValueType -> Infer ()
checkValue v t = do
  t' <- inferValue v
  unify t' t
