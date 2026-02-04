{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Inference.Infer where

import Control.Monad (unless, forM_, forM)
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

infer :: Computation -> Either String CompType
infer c = runInfer initialCtx (inferComp c)

fresh :: Infer ValueType
fresh = do
  n <- get
  modify succ
  return $ TVar $ "t" ++ show n

lookupVar :: Ident -> Infer ValueType
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

instantiate :: Scheme -> Infer ValueType
instantiate (Forall as t) = do
  let asIncList = Set.toAscList as
  as' <- mapM (const fresh) asIncList
  let s = Map.fromDistinctAscList $ zip asIncList as'
  return $ apply s t

extend :: Ident -> Scheme -> Infer a -> Infer a
extend x sc = local (\ctx -> ctx{ vars = Map.insert x sc (vars ctx) })

forceEffects :: Effects -> Infer a -> Infer a
forceEffects es = local (\ctx -> ctx{ effects = es })

checkEffects :: CompType -> Infer ()
checkEffects (TComp _ es) = do
  effects <- asks effects
  unless (es `Map.isSubmapOf` effects) $
    throwError $ "Invoked effects " ++ show es ++ " superscedes available effects " ++ show effects

inferComp :: Computation -> Infer CompType
inferComp = \case
  CReturn v -> do
    tv <- inferValue v
    return $ TComp tv Map.empty
  CApp f v -> do
    tf <- inferValue f
    case tf of
      TFun t1 t2 -> do
        tv <- inferValue v
        s <- unify t1 tv
        -- No checkEffects here; we propagate the effects of t2 up.
        return $ apply s t2
      _ -> throwError $ "Applying non-function type: " ++ show tf
  COp op v -> do
    ar@(Arity tIn tOut) <- lookupOp op
    checkValue v tIn
    return (TComp tOut (Map.singleton op ar))
  CDo x c1 c2 -> do
    t1@(TComp tv e1) <- inferComp c1
    t2@(TComp tr e2) <- extend x (Forall Set.empty tv) (inferComp c2)
    return (TComp tr (e1 `Map.union` e2))
  CIf v c1 c2 -> do
    checkValue v TBool
    t1@(TComp tr1 e1) <- inferComp c1
    t2@(TComp tr2 e2) <- inferComp c2
    s <- unify t1 t2 -- Unification ignores effects
    return (TComp (apply s tr1) (e1 `Map.union` e2))
  CCase v x1 c1 x2 c2 -> do
    tv <- inferValue v
    case tv of
      TEither tl tr -> do
        t1@(TComp tr1 e1) <- extend x1 (Forall Set.empty tl) (inferComp c1)
        t2@(TComp tr2 e2) <- extend x2 (Forall Set.empty tr) (inferComp c2)
        s <- unify t1 t2
        return (TComp (apply s tr1) (e1 `Map.union` e2))
      _ -> throwError $ "Case analysis on non-either type: " ++ show tv
  CHandle v c -> do
    tv <- inferValue v
    case tv of
      THandler t1 t2 -> do
        checkComp t1 c
        return t2
      _ -> throwError $ "Handling with non-handler type: " ++ show tv

checkComp :: CompType -> Computation -> Infer ()
checkComp (TComp t es) c = forceEffects es $ case c of
  CReturn v -> checkValue v t
  CDo x c1 c2 -> do
    t1@(TComp tv _) <- inferComp c1
    checkEffects t1
    extend x (Forall Set.empty tv) (checkComp (TComp t es) c2)
  CHandle v c' -> do
    tv <- inferValue v
    case tv of
      THandler t1 t2 -> do
        unify t2 (TComp t es)
        checkEffects t2
        -- The handled computation c' is allowed to use:
        -- 1. The effects handled by t1 (hInEffs)
        -- 2. The ambient effects es
        let (TComp tv1 hInEffs) = t1
        let expandedEffects = hInEffs `Map.union` es
        checkComp (TComp tv1 expandedEffects) c'
      _ -> throwError $ "Handling with non-handler type: " ++ show tv
  _ -> do
    t' <- inferComp c
    unify (TComp t es) t'
    checkEffects t'
    where
      valType (TComp v _) = v

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
    t2 <- extend x (Forall Set.empty t1) (inferComp c)
    return $ TFun t1 t2
  VRec f x c -> do
    t1 <- fresh
    t2 <- fresh
    let tf = TFun t1 (TComp t2 Map.empty)
    tBody <- extend f (Forall Set.empty tf)
      $ extend x (Forall Set.empty t1)
      $ inferComp c
    s <- unify tBody (TComp t2 Map.empty)
    return $ apply s tf
  VHandler (Handler (RetClause xr cr) opClauses finallyClause) -> do
    hInVal <- fresh
    hOutVal <- fresh

    TComp tr er <- extend xr (Forall Set.empty hInVal) (inferComp cr)
    unify tr hOutVal
    opResults <- forM opClauses $ \(op, OpClause x k cop) -> do
      opIn <- fresh
      opOut <- fresh
      let tk = TFun opOut (TComp hOutVal Map.empty)
      TComp top eop <- extend x (Forall Set.empty opIn) $
        extend k (Forall Set.empty tk) $
          inferComp cop
      unify top hOutVal
      return (op, Arity opIn opOut, eop)

    (finEff, finalVal) <- case finallyClause of
        Nothing -> return (Map.empty, hOutVal)
        Just (FinClause xf cf) -> do
            -- Finally transforms the handler result (hOutVal) into the final result
            TComp tf ef <- extend xf (Forall Set.empty hOutVal) (inferComp cf)
            return (ef, tf)
    -- Collect all handled operations (Input Effects)
    let handledOps = Map.fromList [ (op, ar) | (op, ar, _) <- opResults ]
    -- Collect all used operations (Output Effects)
    let opEffs = foldr Map.union Map.empty (map (\(_,_,e) -> e) opResults)
    let outEffects = er `Map.union` opEffs `Map.union` finEff
    -- hInComp: The computation type this handler can handle
    let hInComp = TComp hInVal handledOps
    -- hOutComp: The computation type this handler produces
    let hOutComp = TComp finalVal outEffects
    return $ THandler hInComp hOutComp

  VPrimitive _ -> throwError "Cannot typecheck runtime primitive"
  VClosure _ _ _ -> throwError "Cannot typecheck runtime closure"

checkValue :: Value -> ValueType -> Infer ()
checkValue v t = do
  t' <- inferValue v
  unify t' t
  return ()
