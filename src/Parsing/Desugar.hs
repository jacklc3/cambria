{-# LANGUAGE LambdaCase #-}

module Parsing.Desugar (
  desugar
) where

import Syntax
import Parsing.SugaredSyntax

import qualified Data.Map as Map
import Control.Monad (foldM, unless, when)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, modify)

type FixEnv = Map.Map Ident Fixity

builtinFixities :: FixEnv
builtinFixities = Map.fromList
  [ ("::", Fixity ARight 1)
  , ("||", Fixity ARight 2)
  , ("&&", Fixity ARight 3)
  , ("==", Fixity ANone  4)
  , ("/=", Fixity ANone  4)
  , ("<",  Fixity ANone  4)
  , (">",  Fixity ANone  4)
  , ("<=", Fixity ANone  4)
  , (">=", Fixity ANone  4)
  , ("++", Fixity ALeft  5)
  , ("+",  Fixity ALeft  6)
  , ("-",  Fixity ALeft  6)
  , ("*",  Fixity ALeft  7)
  , ("/",  Fixity ALeft  7)
  ]

type Desugar a = ReaderT FixEnv (StateT Integer (Except String)) a

fresh :: Desugar Ident
fresh = do
  n <- get
  modify succ
  return ("_" ++ show n)

lookupFixity :: Ident -> Desugar Fixity
lookupFixity bop = do
  fixities <- ask
  case Map.lookup bop fixities of
    Just fx -> return fx
    Nothing -> throwError $ "Infix function " ++ bop ++ " has no fixity in scope"

call :: Ident -> Value -> Computation
call = CApp . VVar

desugar :: SugaredComp -> Either String Computation
desugar c = runExcept (evalStateT (runReaderT (desugarComp c) builtinFixities) 0)

desugarComp :: SugaredComp -> Desugar Computation
desugarComp = \case
  SCReturn e -> do
    (k, v) <- desugarExpr e
    return $ k (CReturn v)
  SCOp op e  -> do
    (k, v) <- desugarExpr e
    return $ k (COp op v)
  SCDo p s1 s2 -> do
    c1 <- desugarComp s1
    c2 <- desugarComp s2
    (x, c2') <- desugarPattern p c2
    return $ CDo x c1 c2'
  SCIf e s1 s2 -> do
    (k, v) <- desugarExpr e
    c1 <- desugarComp s1
    c2 <- desugarComp s2
    return $ k (CIf v c1 c2)
  SCMatch e [(PEither L p1, s1), (PEither R p2, s2)]
    | irrefutable p1 && irrefutable p2 -> do
        (k, v) <- desugarExpr e
        c1 <- desugarComp s1
        (x1, c1') <- desugarPattern p1 c1
        c2 <- desugarComp s2
        (x2, c2') <- desugarPattern p2 c2
        return $ k (CCase v x1 c1' x2 c2')
  SCMatch e [(PEither R p2, s2), (PEither L p1, s1)]
    | irrefutable p1 && irrefutable p2 ->
        desugarComp (SCMatch e [(PEither L p1, s1), (PEither R p2, s2)])
  SCMatch e cases -> do
    (k,  v) <- desugarExpr e
    (k', x) <- case v of
      VVar y -> return (id, y)
      _      -> do
        tmp <- fresh
        return (CDo tmp (CReturn v), tmp)
    c <- desugarCases x cases
    return $ k (k' c)
  SCApp e1 e2 -> do
    (k1, v1) <- desugarExpr e1
    (k2, v2) <- desugarExpr e2
    return $ (k1 . k2) (CApp v1 v2)
  SCWith e sig s -> do
    (k, v) <- desugarExpr e
    sig' <- traverse (\(p, t) -> (,) p <$> desugarValueType t) sig
    c <- desugarComp s
    return $ k (CHandle v sig' c)
  SCEffect op ar s -> do
    c <- desugarComp s
    return (CEffect op ar c)
  SCAnnot s ct -> do
    c <- desugarComp s
    ct' <- desugarCompType ct
    return $ CAnnot c ct'
  SCLetRec defs c -> desugarLetRec defs c
  SCFixity fx@(Fixity _ prec) bop c -> do
    when (prec > 9) $
      throwError ("Infix function " ++ bop ++ " has a precedence outside 0 to 9: " ++ show prec)
    local (Map.insert bop fx) (desugarComp c)
  SCOpChain e chain -> do
    e' <- desugarOpChain e chain
    desugarComp (SCReturn e')

desugarExpr :: SugaredExpr -> Desugar (Computation -> Computation, Value)
desugarExpr = \case
  SEVar x    -> return (id, VVar x)
  SEUnit     -> return (id, VUnit)
  SEBool b   -> return (id, VBool b)
  SEInt i    -> return (id, VInt i)
  SEDouble n -> return (id, VDouble n)
  SEString s -> return (id, VString s)
  SEPair e1 e2 -> do
    (k1, v1) <- desugarExpr e1
    (k2, v2) <- desugarExpr e2
    return (k1 . k2, VPair v1 v2)
  SEEither s e -> do
    (k, v) <- desugarExpr e
    return (k, VEither s v)
  SEHandler cs -> do
    handler <- desugarHandler cs
    return (id, VHandler handler)
  SEFun (p:ps) s -> do
    c <- desugarComp s
    c1 <- desugarArguments ps c
    (x, c2) <- desugarPattern p c1
    return (id, VFun x c2)
  SERec g (p:ps) s -> do
    c <- desugarComp s
    c1 <- desugarArguments ps c
    (x, c2) <- desugarPattern p c1
    return (id, VRec g x c2)
  SEComp s -> do
    tmp  <- fresh
    c <- desugarComp s
    return (CDo tmp c, VVar tmp)
  SEAnnot e t -> do
    (k, v) <- desugarExpr e
    t' <- desugarValueType t
    return (k, VAnnot v t')

desugarOpChain :: SugaredExpr -> [(Ident, SugaredExpr)] -> Desugar SugaredExpr
desugarOpChain e chain = fst <$> climb 0 e chain
  where
    apply bop lhs rhs = SEComp (SCApp (SEVar bop) (SEPair lhs rhs))
    climb _ lhs [] = return (lhs, [])
    climb minPrec lhs rest@((bop, rhs) : rest') = do
      Fixity assoc prec <- lookupFixity bop
      if prec < minPrec then return (lhs, rest)
      else do
        (rhs', rest'') <- climb (if assoc == ARight then prec else succ prec) rhs rest'
        case rest'' of
          (bop', _) : _ | assoc == ANone -> do
            Fixity _ prec' <- lookupFixity bop'
            when (prec' == prec) $ throwError
              ("Non-associative operators cannot be chained: " ++ bop ++ ", " ++ bop')
          _ -> return ()
        climb minPrec (apply bop lhs rhs') rest''

desugarCases :: Ident -> [(Pattern, SugaredComp)] -> Desugar Computation
desugarCases x [] = return $ call "_matchfail" VUnit
desugarCases x ((p, s) : rest)
  | irrefutable p = do
      unless (null rest) $ throwError "Unreachable cases after an irrefutable pattern"
      c <- desugarComp s
      desugarCase x p c (call "_matchfail" VUnit)
  | otherwise = do
      onFail <- desugarCases x rest
      c <- desugarComp s
      tmp <- fresh
      c' <- desugarCase x p c (call tmp VUnit)
      return $ CDo tmp (CReturn (VFun "_" onFail)) c'

desugarCase :: Ident -> Pattern -> Computation -> Computation -> Desugar Computation
desugarCase x p onMatch onFail = case p of
  PVar y         -> return $ CDo y (CReturn (VVar x)) onMatch
  PWild          -> return onMatch
  PUnit          -> return $ CDo "_" (CReturn (VAnnot (VVar x) TUnit)) onMatch
  PInt i         -> eq (VInt i)
  PDouble d      -> eq (VDouble d)
  PBool b        -> eq (VBool b)
  PString t      -> eq (VString t)
  PPair p1 p2    -> do
    tmp1 <- fresh
    tmp2 <- fresh
    c2 <- desugarCase tmp2 p2 onMatch onFail
    c1 <- desugarCase tmp1 p1 c2 onFail
    return $ CDo tmp1 (call "_fst" (VVar x)) $
             CDo tmp2 (call "_snd" (VVar x)) c1
  PEither side q -> do
    tmp <- fresh
    c <- desugarCase tmp q onMatch onFail
    return $ case side of
      L -> CCase (VVar x) tmp c "_" onFail
      R -> CCase (VVar x) "_" onFail tmp c
  PNil           -> uncons (PEither L PWild)
  PCons p1 p2    -> uncons (PEither R (PPair p1 p2))
  where
    eq lit = do
      tmp <- fresh
      return $ CDo tmp (call "_eq" (VPair (VVar x) lit)) (CIf (VVar tmp) onMatch onFail)
    uncons q = do
      tmp <- fresh
      c <- desugarCase tmp q onMatch onFail
      return $ CDo tmp (call "_uncons" (VVar x)) c

-- mutual recursion as a single rec over the tuple of definitions (Bekic)
desugarLetRec :: [(Ident, [Pattern], SugaredComp)] -> SugaredComp -> Desugar Computation
desugarLetRec [(x, ps, s)] c = desugarComp (SCDo (PVar x) (SCReturn (SERec x ps s)) c)
desugarLetRec defs c = do
  g <- fresh
  v <- fresh
  let fetch s = SCDo (foldr1 PPair [PVar x | (x, _, _) <- defs]) (SCApp (SEVar g) SEUnit) s
      knot = SERec g [PWild] (SCReturn (foldr1 SEPair [SEFun ps (fetch s) | (_, ps, s) <- defs]))
      wrap (x,_,_) = SCDo (PVar x) (SCReturn (SEFun [PVar v] (fetch (SCApp (SEVar x) (SEVar v)))))
  desugarComp (SCDo (PVar g) (SCReturn knot) (foldr wrap c defs))

desugarHandler :: [HandlerClause] -> Desugar Handler
desugarHandler cs = do
  (mrc, ocs, fc) <- foldM f (Nothing, [], Nothing) cs
  rc' <- case mrc of
    Just rc -> return rc
    Nothing -> do
      tmp <- fresh
      return $ RetClause tmp (CReturn (VVar tmp))
  return $ Handler rc' ocs fc
    where
      f (_, ocs, fc) (RC p s) = do
        c <- desugarComp s
        (x, c') <- desugarPattern p c
        return (Just (RetClause x c'), ocs, fc)
      f (rc, ocs, fc) (OC op p k s) = do
        c <- desugarComp s
        (x, c') <- desugarPattern p c
        return (rc, (op, OpClause x k c') : ocs, fc)
      f (rc, ocs, _) (FC p s) = do
        c <- desugarComp s
        (x, c') <- desugarPattern p c
        return (rc, ocs, Just (FinClause x c'))

desugarPattern :: Pattern -> Computation -> Desugar (Ident, Computation)
desugarPattern PWild c = return ("_", c)
desugarPattern PUnit c = do
  tmp <- fresh
  return (tmp, CDo "_" (CReturn (VAnnot (VVar tmp) TUnit)) c)
desugarPattern (PVar x) c = return (x, c)
desugarPattern (PPair p1 p2) c = do
  tmp <- fresh
  (x2, c1) <- desugarPattern p2 c
  (x1, c2) <- desugarPattern p1 c1
  return (tmp, CDo x1 (call "_fst" (VVar tmp)) $
               CDo x2 (call "_snd" (VVar tmp)) c2)
desugarPattern _ _ = throwError "Refutable pattern in binding position"

desugarArguments :: [Pattern] -> Computation -> Desugar Computation
desugarArguments [] c = return c
desugarArguments (p:ps) c = do
  c1 <- desugarArguments ps c
  (x, c2) <- desugarPattern p c1
  return (CReturn (VFun x c2))

desugarValueType :: ValueType -> Desugar ValueType
desugarValueType = \case
  TPair t1 t2 -> do
    t1' <- desugarValueType t1
    t2' <- desugarValueType t2
    return (TPair t1' t2')
  TEither t1 t2 -> do
    t1' <- desugarValueType t1
    t2' <- desugarValueType t2
    return (TEither t1' t2')
  TFun t1 t2 -> do
    t1' <- desugarValueType t1
    t2' <- desugarCompType t2
    return (TFun t1' t2')
  TList t -> do
    t' <- desugarValueType t
    return (TList t')
  TMap k v -> do
    k' <- desugarValueType k
    v' <- desugarValueType v
    return (TMap k' v')
  TWeak t -> do
    t' <- desugarValueType t
    return (TWeak t')
  THandler i o -> do
    i' <- desugarCompType i
    o' <- desugarCompType o
    return (THandler i' o')
  t -> return t

desugarCompType :: CompType -> Desugar CompType
desugarCompType (TComp v effs) = do
  v'    <- desugarValueType v
  effs' <- desugarEffects effs
  return (TComp v' effs')

desugarEffects :: EffectsType -> Desugar EffectsType
desugarEffects (Closed m) = do
  m' <- traverse desugarArity m
  return (Closed m')
desugarEffects (Open m _) = do
  m' <- traverse desugarArity m
  r  <- fresh
  return (Open m' r)

desugarArity :: Arity -> Desugar Arity
desugarArity (Arity a r) = do
  a' <- desugarValueType a
  r' <- desugarValueType r
  return (Arity a' r')
