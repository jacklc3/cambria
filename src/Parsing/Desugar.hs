{-# LANGUAGE LambdaCase #-}

module Parsing.Desugar (
  desugar
) where

import Syntax
import Parsing.SugaredSyntax

import qualified Data.Map as Map
import Control.Monad (foldM, when)
import Control.Monad.State (StateT, evalStateT, get, gets, lift, modify)

type FixEnv = Map.Map String (Assoc, Int, OpTarget)

-- same relative precedences as before, loosest to tightest
builtinFixities :: FixEnv
builtinFixities = Map.fromList
  [ ("::", (ARight, 2, TargetVar "::"))
  , ("==", (ANone,  3, TargetVar "=="))
  , ("++", (ALeft,  4, TargetVar "++"))
  , ("+",  (ALeft,  5, TargetVar "+"))
  , ("-",  (ALeft,  5, TargetVar "-"))
  , ("*",  (ALeft,  6, TargetVar "*"))
  , ("/",  (ALeft,  6, TargetVar "/"))
  ]

data DState = DState { counter :: Integer, fixities :: FixEnv }

type Desugar a = StateT DState (Either String) a

fresh :: Desugar Ident
fresh = do
  n <- gets counter
  modify (\st -> st { counter = n + 1 })
  return ("_" ++ show n)

err :: String -> Desugar a
err = lift . Left

desugar :: SugaredComp -> Either String Computation
desugar c = evalStateT (desugarComp c) (DState 0 builtinFixities)

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
  -- a two-clause inl/inr match is just the kernel case construct
  SCMatch e [(PEither L p1, s1), (PEither R p2, s2)]
    | irrefutable p1 && irrefutable p2 -> do
    (k1, v) <- desugarExpr e
    c1 <- desugarComp s1
    (x1, c1') <- desugarPattern p1 c1
    c2 <- desugarComp s2
    (x2, c2') <- desugarPattern p2 c2
    return $ k1 (CCase v x1 c1' x2 c2')
  SCMatch e [(PEither R p2, s2), (PEither L p1, s1)]
    | irrefutable p1 && irrefutable p2 ->
    desugarComp (SCMatch e [(PEither L p1, s1), (PEither R p2, s2)])
  SCMatch e clauses -> do
    (k, v) <- desugarExpr e
    (s, wrap) <- case v of
      VVar x -> return (x, id)
      _      -> do
        t <- fresh
        return (t, CDo t (CReturn v))
    body <- compileClauses s clauses
    return $ k (wrap body)
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
  SCLetRec defs body -> desugarLetRec defs body
  SCFixity assoc prec name target rest -> do
    when (Map.member name builtinFixities) $
      err ("Cannot redeclare built-in operator " ++ name)
    when (prec < 1 || prec > 9) $
      err ("Operator precedence must be between 1 and 9: " ++ name)
    modify (\st -> st { fixities = Map.insert name (assoc, prec, target) (fixities st) })
    desugarComp rest
  SCOpChain e0 rest -> do
    (e, leftover) <- climb 0 e0 rest
    case leftover of
      [] -> desugarComp (exprToComp e)
      _  -> err "Internal error: unconsumed operator chain"

desugarExpr :: SugaredExpr -> Desugar (Computation -> Computation, Value)
desugarExpr = \case
  SEVar x     -> return (id, VVar x)
  SEUnit      -> return (id, VUnit)
  SEBool b    -> return (id, VBool b)
  SEInt i     -> return (id, VInt i)
  SEString s  -> return (id, VString s)
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

exprToComp :: SugaredExpr -> SugaredComp
exprToComp (SEComp c) = c
exprToComp e          = SCReturn e

-- precedence climbing over the flat chains from the parser
climb :: Int -> SugaredExpr -> [(String, SugaredExpr)]
      -> Desugar (SugaredExpr, [(String, SugaredExpr)])
climb _ lhs [] = return (lhs, [])
climb minPrec lhs rest@((opName, rhs0) : more) = do
  (assoc, prec, target) <- lookupFixity opName
  if prec < minPrec then return (lhs, rest)
  else do
    let nextMin = case assoc of
          ARight -> prec
          _      -> prec + 1
    (rhs, rest') <- climb nextMin rhs0 more
    case (assoc, rest') of
      (ANone, (op2, _) : _) -> do
        (_, prec2, _) <- lookupFixity op2
        when (prec2 == prec) $
          err ("Non-associative operators cannot be chained: " ++ opName ++ ", " ++ op2)
      _ -> return ()
    climb minPrec (SEComp (applyTarget target lhs rhs)) rest'

lookupFixity :: String -> Desugar (Assoc, Int, OpTarget)
lookupFixity opName = do
  fe <- gets fixities
  case Map.lookup opName fe of
    Just f  -> return f
    Nothing -> err ("Operator " ++ opName ++ " has no fixity declaration")

applyTarget :: OpTarget -> SugaredExpr -> SugaredExpr -> SugaredComp
applyTarget (TargetVar f) l r = SCApp (SEVar f) (SEPair l r)
applyTarget (TargetOp op) l r = SCOp op (SEPair l r)

irrefutable :: Pattern -> Bool
irrefutable (PVar _)      = True
irrefutable PWild         = True
irrefutable PUnit         = True
irrefutable (PPair p1 p2) = irrefutable p1 && irrefutable p2
irrefutable _             = False

matchFail :: Computation
matchFail = CApp (VVar "matchfail") VUnit

-- clauses tried top to bottom; each fall-through is a join point
compileClauses :: Ident -> [(Pattern, SugaredComp)] -> Desugar Computation
compileClauses _ [] = return matchFail
compileClauses s [(p, body)] = do
  bodyC <- desugarComp body
  matchPat s p bodyC matchFail
compileClauses s ((p, body) : rest) = do
  restC <- compileClauses s rest
  bodyC <- desugarComp body
  f <- fresh
  m <- matchPat s p bodyC (CApp (VVar f) VUnit)
  return $ CDo f (CReturn (VFun "_" restC)) m

matchPat :: Ident -> Pattern -> Computation -> Computation -> Desugar Computation
matchPat s (PVar x) succ_ _ = return $ CDo x (CReturn (VVar s)) succ_
matchPat _ PWild    succ_ _ = return succ_
matchPat s PUnit       succ_ failC = eqTest s VUnit succ_ failC
matchPat s (PInt i)    succ_ failC = eqTest s (VInt i) succ_ failC
matchPat s (PBool b)   succ_ failC = eqTest s (VBool b) succ_ failC
matchPat s (PString t) succ_ failC = eqTest s (VString t) succ_ failC
matchPat s (PPair p1 p2) succ_ failC = do
  a <- fresh
  b <- fresh
  inner2 <- matchPat b p2 succ_ failC
  inner1 <- matchPat a p1 inner2 failC
  return $ CDo a (CApp (VVar "fst") (VVar s)) $
           CDo b (CApp (VVar "snd") (VVar s)) inner1
matchPat s (PEither side p) succ_ failC = do
  x <- fresh
  inner <- matchPat x p succ_ failC
  return $ case side of
    L -> CCase (VVar s) x inner "_" failC
    R -> CCase (VVar s) "_" failC x inner
matchPat s PNil succ_ failC = do
  u <- fresh
  return $ CDo u (CApp (VVar "uncons") (VVar s)) $
           CCase (VVar u) "_" succ_ "_" failC
matchPat s (PCons p1 p2) succ_ failC = do
  u  <- fresh
  pr <- fresh
  h  <- fresh
  t  <- fresh
  inner2 <- matchPat t p2 succ_ failC
  inner1 <- matchPat h p1 inner2 failC
  return $ CDo u (CApp (VVar "uncons") (VVar s)) $
           CCase (VVar u) "_" failC pr
             (CDo h (CApp (VVar "fst") (VVar pr)) $
              CDo t (CApp (VVar "snd") (VVar pr)) inner1)

eqTest :: Ident -> Value -> Computation -> Computation -> Desugar Computation
eqTest s lit succ_ failC = do
  b <- fresh
  return $ CDo b (CApp (VVar "==") (VPair (VVar s) lit)) (CIf (VVar b) succ_ failC)

-- mutual recursion as a single rec over a tagged sum (Bekic)
desugarLetRec :: [(Ident, [Pattern], SugaredComp)] -> SugaredComp -> Desugar Computation
desugarLetRec defs body = do
  h   <- fresh
  arg <- fresh
  let n = length defs
  wrappers <- mapM (wrapper h n) (zip [0..] defs)
  let clauses  = [ (injPat i n (tuplePat ps), c) | (i, (_, ps, c)) <- zip [0..] defs ]
      hBody    = bindAll wrappers (SCMatch (SEVar arg) clauses)
      hDef     = SERec h [PVar arg] hBody
      whole    = SCDo (PVar h) (SCReturn hDef) (bindAll wrappers body)
  desugarComp whole
  where
    bindAll ws c = foldr (\(f, w) acc -> SCDo (PVar f) (SCReturn w) acc) c ws

    wrapper h n (i, (f, ps, _)) = do
      xs <- mapM (const fresh) ps
      let tup = tupleExpr (map SEVar xs)
      return (f, SEFun (map PVar xs) (SCApp (SEVar h) (injExpr i n tup)))

    tupleExpr [e]    = e
    tupleExpr (e:es) = SEPair e (tupleExpr es)
    tupleExpr []     = SEUnit

    tuplePat [p]    = p
    tuplePat (p:ps) = PPair p (tuplePat ps)
    tuplePat []     = PUnit

    injExpr i n e
      | i == n - 1 = iterate (SEEither R) e !! i
      | otherwise  = iterate (SEEither R) (SEEither L e) !! i

    injPat i n p
      | i == n - 1 = iterate (PEither R) p !! i
      | otherwise  = iterate (PEither R) (PEither L p) !! i

desugarHandler :: [HandlerClause] -> Desugar Handler
desugarHandler cs = do
  (rc, ocs, fc) <- foldM f (Nothing, [], Nothing) cs
  rc' <- case rc of
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

-- binding positions are irrefutable; the parser enforces this
desugarPattern :: Pattern -> Computation -> Desugar (Ident, Computation)
desugarPattern PWild c = return ("_", c)
desugarPattern PUnit c = return ("_", c)
desugarPattern (PVar x) c = return (x, c)
desugarPattern (PPair p1 p2) c = do
  tmp <- fresh
  (x2, c1) <- desugarPattern p2 c
  (x1, c2) <- desugarPattern p1 c1
  return (tmp, CDo x1 (CApp (VVar "fst") (VVar tmp)) $
    CDo x2 (CApp (VVar "snd") (VVar tmp)) c2)
desugarPattern _ _ = err "Refutable pattern in binding position"

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
