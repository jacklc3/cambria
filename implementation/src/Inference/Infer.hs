{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Inference.Infer where

import Control.Monad (forM_, forM, foldM)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax
import Inference.Types
import Inference.Substitutable
import Inference.Unify (unify, unifyEffectArities)
import Inference.Initialisation (initialCtx)

infer :: Computation -> Either String CompType
infer c = runInfer initialCtx (inferComp c)

fresh :: Infer ValueType
fresh = do
  n <- get
  modify succ
  return $ TVar $ "t" ++ show n

findVar :: Ident -> Infer ValueType
findVar x = do
  vars <- asks vars
  case Map.lookup x vars of
    Just scheme -> instantiate scheme
    Nothing     -> throwError $ "Unbound variable: " ++ x

findOp :: Op -> Infer Arity
findOp op = do
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
        return $ apply s t2
      _ -> throwError $ "Applying non-function type: " ++ show tf
  COp op v -> do
    ar@(Arity tIn tOut) <- findOp op
    checkValue v tIn
    return (TComp tOut (Map.singleton op ar))
  CDo x c1 c2 -> do
    TComp tv e1 <- inferComp c1
    TComp tr e2 <- extend x (Forall Set.empty tv) (inferComp c2)
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
      THandler (TComp hInVal hInEffs) (TComp hOutVal hOutEffs) -> do
        -- Infer the handled computation (collecting effects upward)
        TComp cVal cEffs <- inferComp c
        -- Unify the computation's value type with handler's expected input type
        s1 <- unify cVal hInVal
        -- For effects that appear in both, unify their arities
        s2 <- unifyEffectArities (apply s1 cEffs) (apply s1 hInEffs)
        let s = s2 `Map.union` s1
        -- Effects that pass through (not handled by this handler)
        let passThrough = apply s cEffs `Map.difference` apply s hInEffs
        -- Final effects = pass through + handler's output effects
        let outEffs = passThrough `Map.union` apply s hOutEffs
        return $ TComp (apply s hOutVal) outEffs
      _ -> throwError $ "Handling with non-handler type: " ++ show tv

checkComp :: CompType -> Computation -> Infer ()
checkComp (TComp expectedVal allowedEffs) c = do
  TComp actualVal usedEffs <- inferComp c
  unify expectedVal actualVal
  -- Verify each used effect is allowed (with matching arity via unification)
  forM_ (Map.toList usedEffs) $ \(op, usedArity) ->
    case Map.lookup op allowedEffs of
      Nothing -> throwError $ "Effect " ++ op ++ " is not allowed in this context. Allowed: " ++ show (Map.keys allowedEffs)
      Just allowedArity -> do
        unify usedArity allowedArity
        return ()

inferValue :: Value -> Infer ValueType
inferValue = \case
  VVar x    -> findVar x
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
    -- Create fresh type variables for handler input and intermediate output
    hInVal <- fresh   -- Type of values entering the handler (return clause input)
    hOutVal <- fresh  -- Type of values after return/op clauses (before finally)

    -- 1. Infer return clause
    -- xr has type hInVal, body should produce hOutVal
    TComp retOutVal retEffs <- extend xr (Forall Set.empty hInVal) (inferComp cr)
    s1 <- unify retOutVal hOutVal

    -- Apply substitution to track the refined types
    let hInVal1 = apply s1 hInVal
    let hOutVal1 = apply s1 hOutVal
    let retEffs1 = apply s1 retEffs

    -- 2. Process each operation clause, accumulating substitutions
    let processOpClause (ops, effs, s) (opName, OpClause x k cop) = do
          -- Apply current substitution to get the current handler output type
          let hOutValCurr = apply s hOutVal1

          -- Create fresh type variables for this operation's arity
          opIn <- fresh
          opOut <- fresh

          -- The continuation k has type: opOut -> Comp hOutValCurr {}
          -- (continuation returns to handler output with no additional effects)
          let kType = TFun opOut (TComp hOutValCurr Map.empty)

          -- Infer the operation body
          TComp opBodyOut opBodyEffs <-
            extend x (Forall Set.empty opIn) $
            extend k (Forall Set.empty kType) $
            inferComp cop

          -- The body output should match the handler output
          s' <- unify opBodyOut hOutValCurr
          let sCombined = s' `Map.union` s

          -- Apply the combined substitution to the arity
          let arity = Arity (apply sCombined opIn) (apply sCombined opOut)
          let ops' = Map.insert opName arity ops
          let effs' = effs `Map.union` (apply sCombined opBodyEffs)

          return (ops', effs', sCombined)

    (handledOps, opEffs, s2) <- foldM processOpClause (Map.empty, Map.empty, s1) opClauses

    -- Apply accumulated substitution
    let hInVal2 = apply s2 hInVal1
    let hOutVal2 = apply s2 hOutVal1
    let retEffs2 = apply s2 retEffs1

    -- 3. Process finally clause
    (finalOutVal, finEffs, s3) <- case finallyClause of
        Nothing -> return (hOutVal2, Map.empty, s2)
        Just (FinClause xf cf) -> do
            -- Finally transforms the handler intermediate output (hOutVal2) into final result
            TComp finOut finEffs <- extend xf (Forall Set.empty hOutVal2) (inferComp cf)
            return (finOut, finEffs, s2)

    -- Apply final substitution to all effects
    let allOutEffs = apply s3 retEffs2 `Map.union` apply s3 opEffs `Map.union` finEffs

    -- hInComp: The computation type this handler can handle
    let hInComp = TComp (apply s3 hInVal2) (apply s3 handledOps)
    -- hOutComp: The computation type this handler produces
    let hOutComp = TComp finalOutVal allOutEffs

    return $ THandler hInComp hOutComp

  VPrimitive _ -> throwError "Cannot typecheck runtime primitive"
  VClosure _ _ _ -> throwError "Cannot typecheck runtime closure"

checkValue :: Value -> ValueType -> Infer ()
checkValue v t = do
  t' <- inferValue v
  unify t' t
  return ()
