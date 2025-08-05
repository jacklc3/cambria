{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Inference.Infer where

import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Ast
import Inference.Types
import Inference.Substitutable
import Inference.Unify

data Context = Context {
  vars :: Map.Map Ident Scheme,
  ops  :: Map.Map Op Scheme
}

data InferState = InferState {
  count :: Int
}

type Infer a = (ReaderT Context (StateT InferState (Except TypeError))) a
type TypeError = String

runInfer :: Context -> Infer Type -> Either TypeError Scheme
runInfer ctx m =
  let
    (res, _) = runState (runReaderT m ctx) (InferState 0)
  in case res of
    Left err -> Left err
    Right ty -> Right $ generalize ctx ty

generalize :: Context -> Type -> Scheme
generalize ctx t =
  let
    (freeTVars, freeEVars) = ftv t `setDiff` ftv ctx
  in
    Forall (Set.toList freeTVars) (Set.toList freeEVars) t

fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ VType . TVar $ count s

freshEffect :: Infer EffectSet
freshEffect = do
    s <- get
    put s { count = count s + 1}
    return $ EffectSet Set.empty (Just $ count s)

instantiate :: Scheme -> Infer Type
instantiate (Forall tvs evs t) = do
  freshTVars <- mapM (const fresh) tvs
  freshEVars <- mapM (const freshEffect) evs
  let tSubst = Subst (Map.fromList (zip tvs (map (\(VType v) -> v) freshTVars))) Map.empty
  let eSubst = Subst Map.empty (Map.fromList (zip evs freshEVars))
  return $ apply (tSubst `compose` eSubst) t

lookupVar :: Ident -> Infer Type
lookupVar name = do
  ctx <- asks vars
  case Map.lookup name ctx of
    Just s  -> instantiate s
    Nothing -> throwError $ "Unbound variable: " ++ name

lookupOp :: Op -> Infer Type
lookupOp name = do
  ctx <- asks ops
  case Map.lookup name ctx of
    Just s  -> instantiate s
    Nothing -> throwError $ "Unknown operation: " ++ name

inferComp :: Computation -> Infer Type
inferComp = \case
  CReturn v -> do
    t <- inferValue v
    emptyEffects <- freshEffect
    return $ CType $ TComp t (EffectSet Set.empty Nothing)

  COp op v -> do
    -- This corresponds to a generic effect call, op v.
    -- The type of op is A -> B. The type of `op v` is B!{op}. [cite: 55, 56]
    opTy <- lookupOp op
    (opParamTy, opResultCompTy) <- case opTy of
        VType (TFun p r) -> return (p, r)
        _ -> throwError $ "Operation " ++ op ++ " is not a function in signature."

    valTy <- inferValue v
    sub1 <- liftUnify $ unify (VType valTy) (VType opParamTy)
    let (TComp resultValTy _) = opResultCompTy
    -- The result computation type has the value type from the signature,
    -- but its effect is explicitly the operation that was called.
    let finalCompTy = TComp resultValTy (EffectSet (Set.singleton op) Nothing)

    return $ CType $ apply sub1 finalCompTy

  CDo x c1 c2 -> do
    CType (TComp t1 e1) <- inferComp c1
    
    ctx <- asks vars
    let scheme = generalize (Context ctx Map.empty) (VType t1)
    let extendedCtx = local (\e -> e { vars = Map.insert x scheme (vars e) })

    CType (TComp t2 e2) <- extendedCtx (inferComp c2)
    
    e_union <- effectUnion e1 e2
    return $ CType (TComp t2 e_union)
    
  CIf v c1 c2 -> do
    vt <- inferValue v
    _ <- liftUnify $ unify (VType vt) (VType TBool)
    
    CType (TComp t1 e1) <- inferComp c1
    CType (TComp t2 e2) <- inferComp c2

    s <- liftUnify $ unify (VType t1) (VType t2)
    
    let t_unified = apply s t1
    e_unified <- effectUnion (apply s e1) (apply s e2)

    return $ CType (TComp t_unified e_unified)

  CApp v1 v2 -> do
    t1 <- inferValue v1
    t2 <- inferValue v2
    VType retValTy <- fresh
    retEff <- freshEffect
    let retCompTy = TComp retValTy retEff
    
    s <- liftUnify $ unify (VType t1) (VType (TFun t2 retCompTy))
    return $ CType $ apply s retCompTy

  CHandle h c -> do
    CType tc <- inferComp c
    th <- inferValue h
    VType td <- CType <$> (TComp <$> ((\(VType v) -> v) <$> fresh) <*> freshEffect)

    s <- liftUnify $ unify (VType th) (VType (THandler tc ((\(CType d) -> d) td)))
    return $ apply s td

  CCase v x1 c1 x2 c2 -> do
    vt <- inferValue v
    VType ta <- fresh
    VType tb <- fresh
    _ <- liftUnify $ unify (VType vt) (VType (TEither ta tb))

    -- Infer left branch
    let schemeA = Forall [] [] (VType ta)
    let ctxA = local (\e -> e { vars = Map.insert x1 schemeA (vars e) })
    CType (TComp t1 e1) <- ctxA (inferComp c1)

    -- Infer right branch
    let schemeB = Forall [] [] (VType tb)
    let ctxB = local (\e -> e { vars = Map.insert x2 schemeB (vars e) })
    CType (TComp t2 e2) <- ctxB (inferComp c2)
    
    s <- liftUnify $ unify (VType t1) (VType t2)
    
    let t_unified = apply s t1
    e_unified <- effectUnion (apply s e1) (apply s e2)

    return $ CType (TComp t_unified e_unified)


-- Core inference function for values
inferValue :: Value -> Infer TValue
inferValue = \case
  VInt _ -> return TInt
  VBool _ -> return TBool
  VString _ -> return TString
  VUnit -> return TUnit
  VVar name -> (\(VType v) -> v) <$> lookupVar name
  VPair v1 v2 -> TPair <$> inferValue v1 <*> inferValue v2
  VEither L v -> TEither <$> inferValue v <*> ((\(VType t) -> t) <$> fresh)
  VEither R v -> TEither <$> ((\(VType t) -> t) <$> fresh) <*> inferValue v

  VFun x c -> do
    -- Function rule [cite: 304]
    VType tx <- fresh
    let scheme = Forall [] [] (VType tx)
    let extendedCtx = local (\e -> e { vars = Map.insert x scheme (vars e) })
    CType tc <- extendedCtx (inferComp c)
    return $ TFun tx tc

  VHandler handler -> do
    -- Handler rule [cite: 313]
    -- Handler type is A!Delta => B!Delta'
    VType tA <- fresh
    eDelta <- freshEffect
    VType tB <- fresh
    eDeltaPrime <- freshEffect
    let handledCompTy = TComp tA eDelta
    let resultCompTy = TComp tB eDeltaPrime

    -- Type check the return clause: return x -> cr
    let RetClause xr cr = retClause handler
    let retScheme = Forall [] [] (VType tA)
    let ctxWithReturn = local (\e -> e { vars = Map.insert xr retScheme (vars e) })
    CType crTy <- ctxWithReturn (inferComp cr)
    s1 <- liftUnify $ unify (CType crTy) (CType resultCompTy)

    -- Type check operation clauses
    effectsFromClauses <- foldM (handleOpClause (apply s1 resultCompTy)) (Set.empty) (opClauses handler)

    -- The resulting effect set delta' must contain effects from the handler's implementation
    -- and any unhandled effects from the original computation.
    -- delta' = effects_from_clauses U (delta \ handled_ops)
    let handledOps = Set.fromList $ map fst (opClauses handler)
    let unhandledEffects = eDelta `effectDiff` handledOps
    finalEffects <- effectUnion eDeltaPrime unhandledEffects
    s2 <- liftUnify $ unifyEffects finalEffects eDeltaPrime

    return $ apply (s2 `compose` s1) (THandler handledCompTy resultCompTy)

  _ -> throwError "Runtime values cannot be inferred."

handleOpClause :: TComp -> Set.Set Op -> (Op, OpClause) -> Infer (Set.Set Op)
handleOpClause resultCompTy accumulatedEffects (op, OpClause x k c_op) = do
  -- Type of op is Ai -> Bi
  opTy <- lookupOp op
  (TFun (tAi) (TComp (tBi) _)) <- case opTy of
    VType (TFun p (TComp r _)) -> return $ TFun p (TComp r (EffectSet (Set.singleton op) Nothing))
    _ -> throwError $ "Operation " ++ op ++ " is not a function in signature."

  -- Continuation k has type Bi -> B!Delta'
  let tK = TFun tBi resultCompTy

  let xScheme = Forall [] [] (VType tAi)
  let kScheme = generalize (Context Map.empty Map.empty) (VType tK)

  let extendedCtx = local (\e -> e { vars = Map.insert x xScheme . Map.insert k kScheme $ vars e })
  CType opClauseTy <- extendedCtx (inferComp c_op)

  -- The clause body must have the same type as the handler's result type
  _ <- liftUnify $ unify (CType opClauseTy) (CType resultCompTy)

  -- This part is subtle. We need to extract the concrete effects introduced by c_op.
  -- For this simplified implementation, we'll assume they are part of the overall effect unification.
  return accumulatedEffects

liftUnify :: Unify a -> Infer a
liftUnify = liftEither . runExcept

effectUnion :: EffectSet -> EffectSet -> Infer EffectSet
effectUnion (EffectSet o1 r1) (EffectSet o2 r2) =
  let o' = o1 `Set.union` o2
  in case (r1, r2) of
    (Nothing, Nothing) -> return $ EffectSet o' Nothing
    (Just v, Nothing) -> return $ EffectSet o' (Just v)
    (Nothing, Just v) -> return $ EffectSet o' (Just v)
    (Just v1, Just v2) | v1 == v2 -> return $ EffectSet o' (Just v1)
    _ -> throwError $ "Cannot unify two different effect variables: " ++ show r1 ++ " and " ++ show r2

-- Helper for set difference on ftv results
setDiff :: (Ord a, Ord b) => ((Set.Set a, Set.Set b)) -> ((Set.Set a, Set.Set b)) -> ((Set.Set a, Set.Set b))
(s1a, s1b) `setDiff` (s2a, s2b) = (s1a `Set.difference` s2a, s1b `Set.difference` s2b)

instance Substitutable Context where
  apply s (Context vars ops) = Context (Map.map (apply s) vars) (Map.map (apply s) ops)
  ftv (Context vars ops) = ftv (Map.elems vars) <> ftv (Map.elems ops)
