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
infer c = runInfer initialCtx $ do
  t <- inferComp c
  applySubst t

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
    tv <- inferValue v
    tRet <- fresh
    unify tf (TFun tv (TComp tRet mempty))
    applySubst tRet
  COp op v -> do
    ar <- lookupOp op
    tv <- inferValue v
    unify tv (arg ar)
    applySubst (TComp (ret ar) (Map.singleton op ar))
  CDo x c1 c2 -> do
    t1 <- inferComp c1
    t1' <- applySubst t1
    t2 <- extendVariable x (Forall mempty (value t1')) (inferComp c2)
    t1Final <- applySubst t1'
    return $ addEffects (effects t1Final) t2
  CIf v c1 c2 -> do
    tv <- inferValue v
    unify tv TBool
    t1 <- inferComp c1
    t2 <- inferComp c2
    unify t1 t2
    t1' <- applySubst t1
    t2' <- applySubst t2
    return $ addEffects (effects t2') t1'
  CCase v x1 c1 x2 c2 -> do
    tv <- inferValue v
    tl <- fresh
    tr <- fresh
    unify tv (TEither tl tr)
    tl' <- applySubst tl
    tr' <- applySubst tr
    t1 <- extendVariable x1 (Forall mempty tl') (inferComp c1)
    t2 <- extendVariable x2 (Forall mempty tr') (inferComp c2)
    unify t1 t2
    t1' <- applySubst t1
    t2' <- applySubst t2
    return $ addEffects (effects t2') t1'
  CHandle v c -> do
    tv <- inferValue v
    tIn <- fresh
    tOut <- fresh
    unify tv (THandler tIn tOut)
    tIn' <- applySubst tIn
    tc <- extendAbilities (effects tIn') (inferComp c)
    tIn'' <- applySubst tIn'
    unify tc tIn''
    tc' <- applySubst tc
    tOut' <- applySubst tOut
    tInFinal <- applySubst tIn''
    return $ addEffects (effects tc' Map.\\ effects tInFinal) tOut'

checkComp :: CompType -> Computation -> Infer ()
checkComp t1 c = do
  t2 <- inferComp c
  unify t1 t2
  t1' <- applySubst t1
  t2' <- applySubst t2
  checkEffectUsage t2' t1'

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
    unify tBody (TComp t2 mempty)
    applySubst tf
  VHandler (Handler (RetClause xr cr) opClauses finallyClause) -> do
    hInVal <- fresh
    hOutVal <- fresh
    retOut <- extendVariable xr (Forall mempty hInVal) (inferComp cr)
    unify retOut (TComp hOutVal mempty)
    
    let processOpClause (ops, hOut) (op, OpClause x k cOp) = do
          opArg <- fresh
          opRet <- fresh
          opOut <- extendVariable x (Forall mempty opArg) $
                   extendVariable k (Forall mempty (TFun opRet hOut)) $
                   inferComp cOp
          hOutCurrent <- applySubst hOut
          unify opOut hOutCurrent
          hOut' <- applySubst hOutCurrent
          opOut' <- applySubst opOut
          return (Map.insert op (Arity opArg opRet) ops, addEffects (effects opOut') hOutNew)

    -- Initial state for fold: empty ops, initial hOut is just the return value
    hOutStart <- applySubst (TComp hOutVal mempty)
    (ops, opsOut) <- foldM processOpClause (mempty, hOutStart) opClauses
    finalRes <- case finallyClause of
      Nothing -> return (TComp (value opsOut) ops)
      Just (FinClause xf cf) -> do
        opsOut' <- applySubst opsOut
        finOut <- extendVariable xf (Forall mempty (value opsOut')) (inferComp cf)
        unify (effects finOut) (effects opsOut')
        finOut' <- applySubst finOut
        return $ addEffects (effects opsOut') finOut'
    hInVal' <- applySubst hInVal
    return $ THandler (TComp hInVal ops) finalRes

  VPrimitive _ -> throwError "Cannot typecheck runtime primitive"
  VClosure _ _ _ -> throwError "Cannot typecheck runtime closure"

checkValue :: Value -> ValueType -> Infer ()
checkValue v t = do
  t' <- inferValue v
  unify t' t
