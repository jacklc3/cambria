{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Inference.Infer where

import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax
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
    res = runExcept (evalStateT (runReaderT m ctx) (InferState 0))
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

-- This helper function makes pattern matching on fresh variables safer.
-- Although our `fresh` function always returns a VType, the type checker
-- requires us to handle all cases.
withFreshVType :: (TValue -> Infer a) -> Infer a
withFreshVType cont = do
    freshTy <- fresh
    case freshTy of
        VType t -> cont t
        _ -> throwError "Internal error: fresh did not return a VType."

inferComp :: Computation -> Infer Type
inferComp = \case
  CReturn v -> do
    t <- inferValue v
    return $ CType $ TComp t (EffectSet Set.empty Nothing)

  COp op v -> do
    opTy <- lookupOp op
    (opParamTy, opResultCompTy) <- case opTy of
        VType (TFun p r) -> return (p, r)
        _ -> throwError $ "Operation " ++ op ++ " is not a function in signature."

    valTy <- inferValue v
    sub1 <- liftUnify $ unify (VType valTy) (VType opParamTy)
    let (TComp resultValTy _) = opResultCompTy
    let finalCompTy = TComp resultValTy (EffectSet (Set.singleton op) Nothing)

    return $ CType $ apply sub1 finalCompTy

  CDo x c1 c2 -> do
    c1Ty <- inferComp c1
    case c1Ty of
      CType (TComp t1 e1) -> do
        ctx <- asks vars
        let scheme = generalize (Context ctx Map.empty) (VType t1)
        let extendedCtx = local (\e -> e { vars = Map.insert x scheme (vars e) })

        c2Ty <- extendedCtx (inferComp c2)
        case c2Ty of
          CType (TComp t2 e2) -> do
            e_union <- effectUnion e1 e2
            return $ CType (TComp t2 e_union)
          _ -> throwError "Expected a computation type in do-expression."
      _ -> throwError "Expected a computation type in do-expression."

  CIf v c1 c2 -> do
    vt <- inferValue v
    _ <- liftUnify $ unify (VType vt) (VType TBool)

    c1Ty <- inferComp c1
    c2Ty <- inferComp c2

    case (c1Ty, c2Ty) of
      (CType (TComp t1 e1), CType (TComp t2 e2)) -> do
        s <- liftUnify $ unify (VType t1) (VType t2)

        let t_unified = apply s t1
        e_unified <- effectUnion (apply s e1) (apply s e2)

        return $ CType (TComp t_unified e_unified)
      _ -> throwError "Both branches of if-expression must have computation types."

  CApp v1 v2 -> do
    t1 <- inferValue v1
    t2 <- inferValue v2
    withFreshVType $ \retValTy -> do
        retEff <- freshEffect
        let retCompTy = TComp retValTy retEff

        s <- liftUnify $ unify (VType t1) (VType (TFun t2 retCompTy))
        return $ CType $ apply s retCompTy

  CHandle h c -> do
    cTy <- inferComp c
    case cTy of
      CType tc -> do
        th <- inferValue h
        withFreshVType $ \freshValTy -> do
            freshEff <- freshEffect
            let resultCompTy = TComp freshValTy freshEff
            let td = CType resultCompTy

            s <- liftUnify $ unify (VType th) (VType (THandler tc resultCompTy))
            return $ apply s td
      _ -> throwError "Expected a computation type in handle expression."


  CCase v x1 c1 x2 c2 -> do
    vt <- inferValue v
    withFreshVType $ \ta ->
      withFreshVType $ \tb -> do
        _ <- liftUnify $ unify (VType vt) (VType (TEither ta tb))

        -- Infer left branch
        let schemeA = Forall [] [] (VType ta)
        let ctxA = local (\e -> e { vars = Map.insert x1 schemeA (vars e) })
        c1Ty <- ctxA (inferComp c1)

        -- Infer right branch
        let schemeB = Forall [] [] (VType tb)
        let ctxB = local (\e -> e { vars = Map.insert x2 schemeB (vars e) })
        c2Ty <- ctxB (inferComp c2)

        case (c1Ty, c2Ty) of
            (CType (TComp t1 e1), CType (TComp t2 e2)) -> do
                s <- liftUnify $ unify (VType t1) (VType t2)
                let t_unified = apply s t1
                e_unified <- effectUnion (apply s e1) (apply s e2)
                return $ CType (TComp t_unified e_unified)
            _ -> throwError "Both branches of a case expression must have computation types."


-- Core inference function for values
inferValue :: Value -> Infer TValue
inferValue = \case
  VInt _ -> return TInt
  VBool _ -> return TBool
  VString _ -> return TString
  VUnit -> return TUnit
  VVar name -> do
    varType <- lookupVar name
    case varType of
        VType v -> return v
        _ -> throwError $ "Variable " ++ name ++ " is not a value type."
  VPair v1 v2 -> TPair <$> inferValue v1 <*> inferValue v2
  VEither L v -> do
    t1 <- inferValue v
    withFreshVType $ \t2 -> return $ TEither t1 t2
  VEither R v -> do
    withFreshVType $ \t1 -> do
        t2 <- inferValue v
        return $ TEither t1 t2

  VFun x c -> do
    withFreshVType $ \tx -> do
        let scheme = Forall [] [] (VType tx)
        let extendedCtx = local (\e -> e { vars = Map.insert x scheme (vars e) })
        cTy <- extendedCtx (inferComp c)
        case cTy of
          CType tc -> return $ TFun tx tc
          _ -> throwError "Function body must have a computation type."

  VHandler handler -> do
    withFreshVType $ \tA ->
      withFreshVType $ \tB -> do
        eDelta <- freshEffect
        eDeltaPrime <- freshEffect
        let handledCompTy = TComp tA eDelta
        let resultCompTy = TComp tB eDeltaPrime

        -- Type check the return clause: return x -> cr
        let RetClause xr cr = retClause handler
        let retScheme = Forall [] [] (VType tA)
        let ctxWithReturn = local (\e -> e { vars = Map.insert xr retScheme (vars e) })
        crTy <- ctxWithReturn (inferComp cr)
        case crTy of
            CType crComp -> do
                s1 <- liftUnify $ unify (CType crComp) (CType resultCompTy)
                -- Type check operation clauses
                _ <- foldM (handleOpClause (apply s1 resultCompTy)) (Set.empty) (opClauses handler)
                let handledOps = Set.fromList $ map fst (opClauses handler)
                let unhandledEffects = eDelta `effectDiff` handledOps
                finalEffects <- effectUnion eDeltaPrime unhandledEffects
                s2 <- liftUnify $ unifyEffects finalEffects eDeltaPrime
                return $ apply (s2 `compose` s1) (THandler handledCompTy resultCompTy)
            _ -> throwError "Handler return clause must have a computation type"

  _ -> throwError "Runtime values cannot be inferred."

handleOpClause :: TComp -> Set.Set Op -> (Op, OpClause) -> Infer (Set.Set Op)
handleOpClause resultCompTy accumulatedEffects (op, OpClause x k c_op) = do
  opTy <- lookupOp op
  case opTy of
    VType (TFun tAi (TComp tBi _)) -> do
      let tK = TFun tBi resultCompTy

      let xScheme = Forall [] [] (VType tAi)
      let kScheme = generalize (Context Map.empty Map.empty) (VType tK)

      let extendedCtx = local (\e -> e { vars = Map.insert x xScheme . Map.insert k kScheme $ vars e })
      opClauseTy <- extendedCtx (inferComp c_op)

      case opClauseTy of
        CType opClauseComp -> do
          -- The clause body must have the same type as the handler's result type
          _ <- liftUnify $ unify (CType opClauseComp) (CType resultCompTy)
          return accumulatedEffects
        _ -> throwError "Operation clause body must have a computation type"
    _ -> throwError $ "Operation " ++ op ++ " is not a function in signature."

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
