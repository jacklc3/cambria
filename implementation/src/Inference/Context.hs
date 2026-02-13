module Inference.Context where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Types

data Scheme = Forall (Set.Set Ident) (Maybe Ident) ValueType
  deriving (Eq, Show)

type Context = Map.Map Ident Scheme

mkPrimScheme :: [Ident] -> ValueType -> ValueType -> Scheme
mkPrimScheme vs t1 t2 = Forall (Set.fromList vs) (Just "e") (TFun t1 (TComp t2 (Open mempty "e")))

primitives :: [(String, Scheme)]
primitives =
  [ ("+",      mkPrimScheme [] (TPair TInt TInt) TInt)
  , ("-",      mkPrimScheme [] (TPair TInt TInt) TInt)
  , ("*",      mkPrimScheme [] (TPair TInt TInt) TInt)
  , ("max",    mkPrimScheme [] (TPair TInt TInt) TInt)
  , ("/",      mkPrimScheme [] (TPair TInt TInt) TDouble)
  , ("++",     mkPrimScheme [] (TPair TString TString) TString)
  , ("hash",   mkPrimScheme [] TUnique TString)
  , ("==",     mkPrimScheme ["a"] (TPair (TVar "a") (TVar "a")) TBool)
  , ("fst",    mkPrimScheme ["a","b"] (TPair (TVar "a") (TVar "b")) (TVar "a"))
  , ("snd",    mkPrimScheme ["a","b"] (TPair (TVar "a") (TVar "b")) (TVar "b"))
  , ("empty",  mkPrimScheme ["k","v"] TUnit (TMap (TVar "k") (TVar "v")))
  , ("insert", mkPrimScheme ["k","v"] (TPair (TPair (TVar "k") (TVar "v")) (TMap (TVar "k") (TVar "v"))) (TMap (TVar "k") (TVar "v")))
  , ("remove", mkPrimScheme ["k","v"] (TPair (TVar "k") (TMap (TVar "k") (TVar "v"))) (TMap (TVar "k") (TVar "v")))
  , ("lookup", mkPrimScheme ["k","v"] (TPair (TVar "k") (TMap (TVar "k") (TVar "v"))) (TVar "v"))
  , ("member", mkPrimScheme ["k","v"] (TPair (TVar "k") (TMap (TVar "k") (TVar "v"))) TBool)
  , ("nil",    mkPrimScheme ["a"] TUnit (TList (TVar "a")))
  , ("cons",   mkPrimScheme ["a"] (TPair (TVar "a") (TList (TVar "a"))) (TList (TVar "a")))
  , ("head",   mkPrimScheme ["a"] (TList (TVar "a")) (TVar "a"))
  , ("tail",   mkPrimScheme ["a"] (TList (TVar "a")) (TList (TVar "a")))
  , ("isnil",  mkPrimScheme ["a"] (TList (TVar "a")) TBool)
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
