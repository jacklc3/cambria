module Inference.Context where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Types

data Scheme = Forall (Set.Set Ident) ValueType
  deriving (Eq, Show)

type Context = Map.Map Ident Scheme

open :: EffectsType
open = Open mempty "e"

primitives :: [(String, Scheme)]
primitives =
  [ ("+",      Forall (Set.fromList ["e"]) (TFun (TPair TInt TInt) (TComp TInt open)))
  , ("-",      Forall (Set.fromList ["e"]) (TFun (TPair TInt TInt) (TComp TInt open)))
  , ("*",      Forall (Set.fromList ["e"]) (TFun (TPair TInt TInt) (TComp TInt open)))
  , ("max",    Forall (Set.fromList ["e"]) (TFun (TPair TInt TInt) (TComp TInt open)))
  , ("/",      Forall (Set.fromList ["e"]) (TFun (TPair TInt TInt) (TComp TDouble open)))
  , ("++",     Forall (Set.fromList ["e"]) (TFun (TPair TString TString) (TComp TString open)))
  , ("hash",   Forall (Set.fromList ["e"]) (TFun TUnique (TComp TString open)))
  , ("==",     Forall (Set.fromList ["a","e"]) (TFun (TPair (TVar "a") (TVar "a")) (TComp TBool open)))
  , ("fst",    Forall (Set.fromList ["a","b","e"]) (TFun (TPair (TVar "a") (TVar "b")) (TComp (TVar "a") open)))
  , ("snd",    Forall (Set.fromList ["a","b","e"]) (TFun (TPair (TVar "a") (TVar "b")) (TComp (TVar "b") open)))
  , ("empty",  Forall (Set.fromList ["k","v","e"]) (TFun TUnit (TComp (TMap (TVar "k") (TVar "v")) open)))
  , ("insert", Forall (Set.fromList ["k","v","e"]) (TFun (TPair (TPair (TVar "k") (TVar "v")) (TMap (TVar "k") (TVar "v"))) (TComp (TMap (TVar "k") (TVar "v")) open)))
  , ("remove", Forall (Set.fromList ["k","v","e"]) (TFun (TPair (TVar "k") (TMap (TVar "k") (TVar "v"))) (TComp (TMap (TVar "k") (TVar "v")) open)))
  , ("lookup", Forall (Set.fromList ["k","v","e"]) (TFun (TPair (TVar "k") (TMap (TVar "k") (TVar "v"))) (TComp (TVar "v") open)))
  , ("member", Forall (Set.fromList ["k","v","e"]) (TFun (TPair (TVar "k") (TMap (TVar "k") (TVar "v"))) (TComp TBool open)))
  , ("nil",    Forall (Set.fromList ["a","e"]) (TFun TUnit (TComp (TList (TVar "a")) open)))
  , ("cons",   Forall (Set.fromList ["a","e"]) (TFun (TPair (TVar "a") (TList (TVar "a"))) (TComp (TList (TVar "a")) open)))
  , ("head",   Forall (Set.fromList ["a","e"]) (TFun (TList (TVar "a")) (TComp (TVar "a") open)))
  , ("tail",   Forall (Set.fromList ["a","e"]) (TFun (TList (TVar "a")) (TComp (TList (TVar "a")) open)))
  , ("isnil",  Forall (Set.fromList ["a","e"]) (TFun (TList (TVar "a")) (TComp TBool open)))
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
