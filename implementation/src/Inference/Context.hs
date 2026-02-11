module Inference.Context where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Types

data Scheme = Forall (Set.Set Ident) ValueType
  deriving (Eq, Show)

data Context = Context {
  variables :: Map.Map Ident Scheme,
  abilities :: Effects
} deriving (Show)

primitives :: [(String, Scheme)]
primitives =
  [ ("+",    Forall Set.empty (TFun (TPair TInt TInt) (TComp TInt mempty)))
  , ("-",    Forall Set.empty (TFun (TPair TInt TInt) (TComp TInt mempty)))
  , ("*",    Forall Set.empty (TFun (TPair TInt TInt) (TComp TInt mempty)))
  , ("max",  Forall Set.empty (TFun (TPair TInt TInt) (TComp TInt mempty)))
  , ("/",    Forall Set.empty (TFun (TPair TInt TInt) (TComp TDouble mempty)))
  , ("++",   Forall Set.empty (TFun (TPair TString TString) (TComp TString mempty)))
  , ("hash", Forall Set.empty (TFun TUnique (TComp TString mempty)))
  , ("==",   Forall (Set.fromList ["a"]) (TFun (TPair (TVar "a") (TVar "a")) (TComp TBool mempty)))
  , ("fst",  Forall (Set.fromList ["a","b"]) (TFun (TPair (TVar "a") (TVar "b")) (TComp (TVar "a") mempty)))
  , ("snd",  Forall (Set.fromList ["a","b"]) (TFun (TPair (TVar "a") (TVar "b")) (TComp (TVar "b") mempty)))
  , ("empty",  Forall (Set.fromList ["k","v"]) (TFun TUnit (TComp (TMap (TVar "k") (TVar "v")) mempty)))
  , ("insert", Forall (Set.fromList ["k","v"]) (TFun (TPair (TPair (TVar "k") (TVar "v")) (TMap (TVar "k") (TVar "v"))) (TComp (TMap (TVar "k") (TVar "v")) mempty)))
  , ("remove", Forall (Set.fromList ["k","v"]) (TFun (TPair (TVar "k") (TMap (TVar "k") (TVar "v"))) (TComp (TMap (TVar "k") (TVar "v")) mempty)))
  , ("lookup", Forall (Set.fromList ["k","v"]) (TFun (TPair (TVar "k") (TMap (TVar "k") (TVar "v"))) (TComp (TVar "v") mempty)))
  , ("member", Forall (Set.fromList ["k","v"]) (TFun (TPair (TVar "k") (TMap (TVar "k") (TVar "v"))) (TComp TBool mempty)))
  , ("nil",    Forall (Set.fromList ["a"]) (TFun TUnit (TComp (TList (TVar "a")) mempty)))
  , ("cons",   Forall (Set.fromList ["a"]) (TFun (TPair (TVar "a") (TList (TVar "a"))) (TComp (TList (TVar "a")) mempty)))
  , ("head",   Forall (Set.fromList ["a"]) (TFun (TList (TVar "a")) (TComp (TVar "a") mempty)))
  , ("tail",   Forall (Set.fromList ["a"]) (TFun (TList (TVar "a")) (TComp (TList (TVar "a")) mempty)))
  , ("isnil",  Forall (Set.fromList ["a"]) (TFun (TList (TVar "a")) (TComp TBool mempty)))
  ]

primitiveOps :: [(String, Arity)]
primitiveOps =
  [ ("unique",    Arity TUnit TUnique)
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
