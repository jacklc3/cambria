module Inference.Initialisation where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Inference.Types

-- | Primitive function types
primitives :: [(String, Scheme)]
primitives =
  [ ("+",    Forall Set.empty (TFun (TPair TInt TInt) TInt Map.empty))
  , ("-",    Forall Set.empty (TFun (TPair TInt TInt) TInt Map.empty))
  , ("*",    Forall Set.empty (TFun (TPair TInt TInt) TInt Map.empty))
  , ("max",  Forall Set.empty (TFun (TPair TInt TInt) TInt Map.empty))
  , ("/",    Forall Set.empty (TFun (TPair TInt TInt) TDouble Map.empty))
  , ("++",   Forall Set.empty (TFun (TPair TString TString) TString Map.empty))
  , ("hash", Forall Set.empty (TFun TName TString Map.empty))
  , ("==",   Forall (Set.fromList ["a"]) (TFun (TPair (TVar "a") (TVar "a")) TBool Map.empty))
  , ("fst",  Forall (Set.fromList ["a","b"]) (TFun (TPair (TVar "a") (TVar "b")) (TVar "a") Map.empty))
  , ("snd",  Forall (Set.fromList ["a","b"]) (TFun (TPair (TVar "a") (TVar "b")) (TVar "b") Map.empty))
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
