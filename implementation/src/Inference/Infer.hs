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
  let asIncList = Set.toAscList as
  as' <- mapM (const fresh) asIncList
  let s = Map.fromDistinctAscList $ zip asIncList as'
  return $ apply s t

defVar :: Ident -> Scheme -> Infer a -> Infer a
defVar x sc = local (\ctx -> ctx{ variables = Map.insert x sc (variables ctx) })

extendAbilities :: Effects -> Infer a -> Infer a
extendAbilities effs = local (\ctx -> ctx{ abilities = effs <> abilities ctx })

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
    s <- unify tv (input ar)
    return $ apply s (TComp (output ar) (Map.singleton op ar))
  CDo x c1 c2 -> do
    TComp tv e1 <- inferComp c1
    TComp tr e2 <- defVar x (Forall mempty tv) (inferComp c2)
    return (TComp tr (e1 <> e2))
  CIf v c1 c2 -> do
    checkValue v TBool
    t1 <- inferComp c1
    t2 <- inferComp c2
    s <- unify t1 t2
    return $ apply s (TComp (value t1) (effects t1 <> effects t2))
  CCase v x1 c1 x2 c2 -> do
    tv <- inferValue v
    case tv of
      TEither tl tr -> do
        t1 <- defVar x1 (Forall mempty tl) (inferComp c1)
        t2 <- defVar x2 (Forall mempty tr) (inferComp c2)
        s <- unify t1 t2
        return $ apply s (TComp (value t1) (effects t1 <> effects t2))
      _ -> throwError $ "Case analysis on non-either type: " ++ show tv
  CHandle v c -> do
    tv <- inferValue v
    case tv of
      THandler tIn tOut -> do
        tc <- extendAbilities (effects tIn) (inferComp c)
        s <- unify tc tIn
        let newEffects = (effects tc Map.\\ effects tIn) <> effects tOut
        return $ apply s (TComp (value tOut) newEffects)
      _ -> throwError $ "Handling with non-handler type: " ++ show tv

checkComp :: CompType -> Computation -> Infer ()
checkComp t c = do
  t' <- inferComp c
  s <- unify t t'
  checkEffectUsage (apply s t') (apply s t)

checkEffectUsage :: CompType -> CompType -> Infer ()
checkEffectUsage t1 t2 = do
  unless (effects t1 `Map.isSubmapOf` effects t2)
    $ throwError $ "Computation " ++ show t1 ++ " incompatible with " ++ show t2

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
    t2 <- defVar x (Forall mempty t1) (inferComp c)
    return $ TFun t1 t2
  VRec f x c -> do
    t1 <- fresh
    t2 <- fresh
    let tf = TFun t1 (TComp t2 mempty)
    tBody <- defVar f (Forall mempty tf)
      $ defVar x (Forall mempty t1)
      $ inferComp c
    s <- unify tBody (TComp t2 mempty)
    return $ apply s tf
  VHandler (Handler (RetClause xr cr) opClauses finallyClause) -> do
    -- Create fresh type variables for handler input and intermediate output
    hInVal <- fresh   -- Type of values entering the handler (return clause input)
    hOutVal <- fresh  -- Type of values after return/op clauses (before finally)

    -- 1. Infer return clause
    -- xr has type hInVal, body should produce hOutVal
    TComp retOutVal retEffs <- defVar xr (Forall mempty hInVal) (inferComp cr)
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
          let kType = TFun opOut (TComp hOutValCurr mempty)

          -- Infer the operation body
          TComp opBodyOut opBodyEffs <-
            defVar x (Forall mempty opIn) $
            defVar k (Forall mempty kType) $
            inferComp cop

          -- The body output should match the handler output
          s' <- unify opBodyOut hOutValCurr
          let sCombined = s' <> s

          -- Apply the combined substitution to the arity
          let arity = Arity (apply sCombined opIn) (apply sCombined opOut)
          let ops' = Map.insert opName arity ops
          let effs' = effs <> (apply sCombined opBodyEffs)

          return (ops', effs', sCombined)

    (handledOps, opEffs, s2) <- foldM processOpClause (mempty, mempty, s1) opClauses

    -- Apply accumulated substitution
    let hInVal2 = apply s2 hInVal1
    let hOutVal2 = apply s2 hOutVal1
    let retEffs2 = apply s2 retEffs1

    -- 3. Process finally clause
    (finalOutVal, finEffs, s3) <- case finallyClause of
        Nothing -> return (hOutVal2, mempty, s2)
        Just (FinClause xf cf) -> do
            -- Finally transforms the handler intermediate output (hOutVal2) into final result
            TComp finOut finEffs <- defVar xf (Forall mempty hOutVal2) (inferComp cf)
            return (finOut, finEffs, s2)

    -- Apply final substitution to all effects
    let allOutEffs = apply s3 retEffs2 <> apply s3 opEffs <> finEffs

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
