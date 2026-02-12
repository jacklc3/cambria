module Inference.Context where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Types

data Scheme = Forall (Set.Set Ident) ValueType
  deriving (Eq, Show)

type Context = Map.Map Ident Scheme

closed :: EffectsType
closed = Closed mempty

primitives :: [(String, Scheme)]
primitives =
  [ ("+",      Forall Set.empty (TFun (TPair TInt TInt) (TComp TInt closed)))
  , ("-",      Forall Set.empty (TFun (TPair TInt TInt) (TComp TInt closed)))
  , ("*",      Forall Set.empty (TFun (TPair TInt TInt) (TComp TInt closed)))
  , ("max",    Forall Set.empty (TFun (TPair TInt TInt) (TComp TInt closed)))
  , ("/",      Forall Set.empty (TFun (TPair TInt TInt) (TComp TDouble closed)))
  , ("++",     Forall Set.empty (TFun (TPair TString TString) (TComp TString closed)))
  , ("hash",   Forall Set.empty (TFun TUnique (TComp TString closed)))
  , ("==",     Forall (Set.fromList ["a"]) (TFun (TPair (TVar "a") (TVar "a")) (TComp TBool closed)))
  , ("fst",    Forall (Set.fromList ["a","b"]) (TFun (TPair (TVar "a") (TVar "b")) (TComp (TVar "a") closed)))
  , ("snd",    Forall (Set.fromList ["a","b"]) (TFun (TPair (TVar "a") (TVar "b")) (TComp (TVar "b") closed)))
  , ("empty",  Forall (Set.fromList ["k","v"]) (TFun TUnit (TComp (TMap (TVar "k") (TVar "v")) closed)))
  , ("insert", Forall (Set.fromList ["k","v"]) (TFun (TPair (TPair (TVar "k") (TVar "v")) (TMap (TVar "k") (TVar "v"))) (TComp (TMap (TVar "k") (TVar "v")) closed)))
  , ("remove", Forall (Set.fromList ["k","v"]) (TFun (TPair (TVar "k") (TMap (TVar "k") (TVar "v"))) (TComp (TMap (TVar "k") (TVar "v")) closed)))
  , ("lookup", Forall (Set.fromList ["k","v"]) (TFun (TPair (TVar "k") (TMap (TVar "k") (TVar "v"))) (TComp (TVar "v") closed)))
  , ("member", Forall (Set.fromList ["k","v"]) (TFun (TPair (TVar "k") (TMap (TVar "k") (TVar "v"))) (TComp TBool closed)))
  , ("nil",    Forall (Set.fromList ["a"]) (TFun TUnit (TComp (TList (TVar "a")) closed)))
  , ("cons",   Forall (Set.fromList ["a"]) (TFun (TPair (TVar "a") (TList (TVar "a"))) (TComp (TList (TVar "a")) closed)))
  , ("head",   Forall (Set.fromList ["a"]) (TFun (TList (TVar "a")) (TComp (TVar "a") closed)))
  , ("tail",   Forall (Set.fromList ["a"]) (TFun (TList (TVar "a")) (TComp (TList (TVar "a")) closed)))
  , ("isnil",  Forall (Set.fromList ["a"]) (TFun (TList (TVar "a")) (TComp TBool closed)))
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
initialCtx = Map.fromList primitives
