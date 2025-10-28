module Inference.Initialisation where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Syntax (Ident, Op)
import Inference.Types

initialOpSigs :: Map.Map Op Scheme
initialOpSigs = Map.fromList [
    ("print", Forall [] [] (VType $ TFun TString (TComp TUnit (EffectSet (Set.singleton "print") Nothing)))),
    ("read",  Forall [] [] (VType $ TFun TUnit (TComp TString (EffectSet (Set.singleton "read") Nothing)))),
    ("decide",Forall [] [] (VType $ TFun TUnit (TComp TBool (EffectSet (Set.singleton "decide") Nothing)))),
    ("fail",  Forall [0] [] (VType $ TFun TUnit (TComp (TVar 0) (EffectSet (Set.singleton "fail") Nothing)))),
    ("get",   Forall [] [] (VType $ TFun TUnit (TComp TInt (EffectSet (Set.singleton "get") Nothing)))),
    ("set",   Forall [] [] (VType $ TFun TInt (TComp TUnit (EffectSet (Set.singleton "set") Nothing))))
  ]

initialCtx :: Map.Map Ident Scheme
initialCtx = Map.fromList [
    ("join", Forall [] [] (VType $ TFun TString (TComp (TFun TString (TComp TString (EffectSet Set.empty Nothing))) (EffectSet Set.empty Nothing))))
  ]
