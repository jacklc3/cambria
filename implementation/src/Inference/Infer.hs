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
import qualified Inference.Unify as Unify
import Inference.Initialisation (initialCtx)

infer :: Computation -> Either String CompType
infer c = runInfer initialCtx (inferComp c)

extendSubst :: Subst -> Infer ()
extendSubst s' = modify (\st -> st { subst = s' `compose` subst st })

compose :: Subst -> Subst -> Subst
compose s2 s1 = Map.map (apply s2) s1 `Map.union` s2

applySubst :: Substitutable a => a -> Infer a
applySubst t = do
  s <- gets subst
  return (apply s t)

unify :: (Unify.Unifiable a, Substitutable a) => a -> a -> Infer ()
unify t1 t2 = do
  s <- gets subst
  u <- Unify.unify (apply s t1) (apply s t2)
  extendSubst u

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
  CDeclare op dtArg dtRet c -> do
    ctx <- ask
    let tArg = resolveBaseType dtArg
        tRet = resolveBaseType dtRet
        ar   = Arity tArg tRet
        -- Discover params from resolved types, merge with existing
        params = collectTParams tArg <> collectTParams tRet
        paramMap = Set.foldl' (\pm p ->
          if Map.member p pm then pm
          else Map.insert p p pm
          ) (parameters ctx) params
    -- Run continuation with extended context
    local (\ctx' -> ctx' {
      abilities  = Map.singleton op ar <> abilities ctx',
      parameters = paramMap
    }) (inferComp c)
  CHandle v c -> do
    tv <- inferValue v
    tInVal <- fresh
    tOutVal <- fresh
    unify tv (THandler (TComp tInVal mempty) (TComp tOutVal mempty))
    ~(THandler tIn tOut) <- applySubst tv
    tc <- extendAbilities (effects tIn) (inferComp c)
    unify tc tIn
    applySubst (addEffects (effects tc Map.\\ effects tIn) tOut)

checkComp :: CompType -> Computation -> Infer ()
checkComp expected c = do
  t <- inferComp c
  unify expected t
  t' <- applySubst t
  ctx <- asks abilities
  let allowed = effects expected <> ctx
      illegal = effects t' `Map.difference` allowed
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
  VName _   -> return TName
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
  VHandler (Handler (RetClause xr cr) opClauses finClause typeInsts) -> do
    -- Process type instantiations: build param substitution map
    ctx <- ask
    paramSubst <- foldM (\ps (paramName, baseType) -> do
      case Map.lookup paramName (parameters ctx) of
        Nothing -> throwError $ "Type parameter $" ++ paramName ++ " not in scope"
        Just paramId -> return (Map.insert paramId (resolveBaseType baseType) ps)
      ) Map.empty typeInsts
    -- Find all ops in abilities whose arities mention any instantiated param
    let instantiatedParams = Map.keysSet paramSubst
        arityMentions (Arity a r) = collectTParams a <> collectTParams r
        requiredOps = Map.keysSet $ Map.filter
          (\ar -> not $ Set.null $ arityMentions ar `Set.intersection` instantiatedParams)
          (abilities ctx)
    -- Verify handler handles all required ops
    let handledOps = Set.fromList (map fst opClauses)
    let missingOps = requiredOps Set.\\ handledOps
    unless (Set.null missingOps) $
      throwError $ "Handler instantiates type parameter(s) but does not handle operation(s): "
        ++ show (Set.toList missingOps)

    -- Resolve concrete arities for ops with instantiated params
    let concreteArity op = case Map.lookup op (abilities ctx) of
          Just (Arity a r) | not (Map.null paramSubst) ->
            Just (Arity (substParams paramSubst a) (substParams paramSubst r))
          _ -> Nothing

    let processOpClause (ops, hOut) (op, OpClause x k cOp) = do
          opArg <- fresh
          opRet <- fresh
          case concreteArity op of
            Just ar -> do
              unify opArg (arg ar)
              unify opRet (ret ar)
            Nothing -> return ()
          opArg' <- applySubst opArg
          opRet' <- applySubst opRet
          opOut <- extendVariable x (Forall mempty opArg')
                   $ extendVariable k (Forall mempty (TFun opRet' hOut))
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

resolveBaseType :: BaseType -> ValueType
resolveBaseType BTUnit         = TUnit
resolveBaseType BTInt          = TInt
resolveBaseType BTBool         = TBool
resolveBaseType BTDouble       = TDouble
resolveBaseType BTString       = TString
resolveBaseType BTName         = TName
resolveBaseType (BTPair a b)   = TPair (resolveBaseType a) (resolveBaseType b)
resolveBaseType (BTEither a b) = TEither (resolveBaseType a) (resolveBaseType b)
resolveBaseType (BTParam p)    = TParam p

collectTParams :: ValueType -> Set.Set Ident
collectTParams (TParam p)       = Set.singleton p
collectTParams (TPair t1 t2)    = collectTParams t1 <> collectTParams t2
collectTParams (TEither t1 t2)  = collectTParams t1 <> collectTParams t2
collectTParams (TFun t1 t2)     = collectTParams t1 <> collectTParams (value t2)
collectTParams (THandler t1 t2) = collectTParams (value t1) <> collectTParams (value t2)
collectTParams _                = Set.empty

substParams :: Map.Map Ident ValueType -> ValueType -> ValueType
substParams ps (TParam p)       = Map.findWithDefault (TParam p) p ps
substParams ps (TPair t1 t2)    = TPair (substParams ps t1) (substParams ps t2)
substParams ps (TEither t1 t2)  = TEither (substParams ps t1) (substParams ps t2)
substParams ps (TFun t1 t2)     = TFun (substParams ps t1) (substParamsComp ps t2)
substParams ps (THandler t1 t2) = THandler (substParamsComp ps t1) (substParamsComp ps t2)
substParams _  t                = t

substParamsComp :: Map.Map Ident ValueType -> CompType -> CompType
substParamsComp ps (TComp t es) = TComp (substParams ps t) (Map.map substAr es)
  where substAr (Arity a r) = Arity (substParams ps a) (substParams ps r)

