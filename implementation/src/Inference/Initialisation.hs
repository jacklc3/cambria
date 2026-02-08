module Inference.Initialisation where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Inference.Types

primitives :: [(String, Scheme)]
primitives =
  [ ("+",    Forall Set.empty (TFun (TPair TInt TInt) (TComp TInt mempty)))
  , ("-",    Forall Set.empty (TFun (TPair TInt TInt) (TComp TInt mempty)))
  , ("*",    Forall Set.empty (TFun (TPair TInt TInt) (TComp TInt mempty)))
  , ("max",  Forall Set.empty (TFun (TPair TInt TInt) (TComp TInt mempty)))
  , ("/",    Forall Set.empty (TFun (TPair TInt TInt) (TComp TDouble mempty)))
  , ("++",   Forall Set.empty (TFun (TPair TString TString) (TComp TString mempty)))
  , ("hash", Forall Set.empty (TFun TName (TComp TString mempty)))
  , ("==",   Forall (Set.fromList ["a"]) (TFun (TPair (TVar "a") (TVar "a")) (TComp TBool mempty)))
  , ("fst",  Forall (Set.fromList ["a","b"]) (TFun (TPair (TVar "a") (TVar "b")) (TComp (TVar "a") mempty)))
  , ("snd",  Forall (Set.fromList ["a","b"]) (TFun (TPair (TVar "a") (TVar "b")) (TComp (TVar "b") mempty)))
  ]

primitiveOps :: [(String, Arity)]
primitiveOps =
  [ ("new",       Arity TUnit TName)
  , ("print",     Arity TString TUnit)
  , ("read",      Arity TUnit TString)
  , ("flip",      Arity TUnit TBool)
  , ("bernoulli", Arity TDouble TBool)
  , ("uniform",   Arity TUnit TDouble)
  ]

initialCtx :: Context
initialCtx = Context
  (Map.fromList primitives)
  (Map.fromList primitiveOps)
  Map.empty
