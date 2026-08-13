{-# LANGUAGE LambdaCase #-}

module GC (collect) where

import Data.Monoid (Any(..))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax

type Live  = Set.Set Name
type Sweep a = (Any, a)

collect :: Value -> Value -> Value
collect v k = snd (sweepValue (saturate Set.empty) k)
  where
    saturate marked
      | new == marked = new
      | otherwise     = saturate new
      where mark = markValue marked Set.empty (Env Map.empty)
            new  = mark v <> mark k

markValue :: Live -> Set.Set Ident -> Env -> Value -> Live
markValue marked bound env@(Env m) = \case
  VName a             -> Set.singleton a
  VVar x
    | x `Set.member` bound -> mempty
    | otherwise            -> foldMap (markValue marked mempty env) (Map.lookup x m)
  VPair a b           -> markValue marked bound env a <> markValue marked bound env b
  VEither _ a         -> markValue marked bound env a
  VAnnot a _          -> markValue marked bound env a
  VFun x c            -> markComp marked (Set.insert x bound) env c
  VRec g x c          -> markComp marked (Set.insert g (Set.insert x bound)) env c
  VHandler h          -> markHandler marked bound env h
  VMap es             -> foldMap (\(k, x) -> markValue marked bound env k <> markValue marked bound env x) es
  VList xs            -> foldMap (markValue marked bound env) xs
  VWeak w             -> foldMap (markValue marked bound env) (Map.restrictKeys w marked)
  VFunClosure x c e      -> markComp marked (Set.singleton x) e c
  VRecClosure g x c e -> markComp marked (Set.fromList [g, x]) e c
  VHandlerClosure h e -> markHandler marked Set.empty e h
  _                   -> mempty

markComp :: Live -> Set.Set Ident -> Env -> Computation -> Live
markComp marked bound env = \case
  CReturn a           -> mv bound a
  COp _ a             -> mv bound a
  CDo x c1 c2         -> mc bound c1 <> mc (Set.insert x bound) c2
  CIf a c1 c2         -> mv bound a <> mc bound c1 <> mc bound c2
  CCase a x1 c1 x2 c2 -> mv bound a <> mc (Set.insert x1 bound) c1 <> mc (Set.insert x2 bound) c2
  CApp a b            -> mv bound a <> mv bound b
  CHandle a _ c       -> mv bound a <> mc bound c
  CEffect _ _ c       -> mc bound c
  CAnnot c _          -> mc bound c
  where
    mv b = markValue marked b env
    mc b = markComp marked b env

markHandler :: Live -> Set.Set Ident -> Env -> Handler -> Live
markHandler marked bound env (Handler (RetClause xr cr) ocs fc) =
  markComp marked (Set.insert xr bound) env cr
    <> foldMap (\(_, OpClause x k c) -> markComp marked (Set.insert x (Set.insert k bound)) env c) ocs
    <> foldMap (\(FinClause x c) -> markComp marked (Set.insert x bound) env c) fc

share :: (a -> Sweep a) -> a -> Sweep a
share rebuild x = case rebuild x of
  swept@(Any True, _) -> swept
  _                   -> (Any False, x)

sweepValue :: Live -> Value -> Sweep Value
sweepValue lv = share $ \case
  VPair a b             -> VPair <$> sweepValue lv a <*> sweepValue lv b
  VEither s a           -> VEither s <$> sweepValue lv a
  VHandler h            -> VHandler <$> sweepHandler lv h
  VMap es               -> VMap <$> traverse (\(k, x) -> (,) <$> sweepValue lv k <*> sweepValue lv x) es
  VList xs              -> VList <$> traverse (sweepValue lv) xs
  VAnnot a t            -> flip VAnnot t <$> sweepValue lv a
  VFun x c              -> VFun x <$> sweepComp lv c
  VRec g x c            -> VRec g x <$> sweepComp lv c
  VFunClosure x c e        -> VFunClosure x <$> sweepComp lv c <*> sweepEnv lv e
  VRecClosure g x c e   -> VRecClosure g x <$> sweepComp lv c <*> sweepEnv lv e
  VHandlerClosure h e   -> VHandlerClosure <$> sweepHandler lv h <*> sweepEnv lv e
  VWeak m               -> (Any (Map.size survivors < Map.size m), VWeak)
                             <*> traverse (sweepValue lv) survivors
                             where survivors = Map.restrictKeys m lv
  v                     -> pure v

sweepComp :: Live -> Computation -> Sweep Computation
sweepComp lv = share $ \case
  CReturn a           -> CReturn <$> sweepValue lv a
  COp op a            -> COp op <$> sweepValue lv a
  CDo x c1 c2         -> CDo x <$> sweepComp lv c1 <*> sweepComp lv c2
  CIf a c1 c2         -> CIf <$> sweepValue lv a <*> sweepComp lv c1 <*> sweepComp lv c2
  CCase a x1 c1 x2 c2 -> (\a' c1' c2' -> CCase a' x1 c1' x2 c2')
                           <$> sweepValue lv a <*> sweepComp lv c1 <*> sweepComp lv c2
  CApp a b            -> CApp <$> sweepValue lv a <*> sweepValue lv b
  CHandle a s c       -> (\a' c' -> CHandle a' s c') <$> sweepValue lv a <*> sweepComp lv c
  CEffect op ar c     -> CEffect op ar <$> sweepComp lv c
  CAnnot c t          -> flip CAnnot t <$> sweepComp lv c

sweepHandler :: Live -> Handler -> Sweep Handler
sweepHandler lv = share $ \(Handler (RetClause xr cr) ocs fc) ->
  Handler
    <$> (RetClause xr <$> sweepComp lv cr)
    <*> traverse (\(op, OpClause x k c) -> (,) op . OpClause x k <$> sweepComp lv c) ocs
    <*> traverse (\(FinClause x c) -> FinClause x <$> sweepComp lv c) fc

sweepEnv :: Live -> Env -> Sweep Env
sweepEnv lv = share $ \(Env e) -> Env <$> traverse (sweepValue lv) e
