module Primitives where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Unique (hashUnique)

import Syntax

data Primitive = Primitive {
  primScheme :: Scheme,
  primValue  :: Value
}

primitives :: Map.Map Ident Primitive
primitives = Map.fromList
  [ prim "+"      []        (TPair TInt TInt)   TInt    $ \(VPair (VInt x) (VInt y)) -> VInt (x + y)
  , prim "-"      []        (TPair TInt TInt)   TInt    $ \(VPair (VInt x) (VInt y)) -> VInt (x - y)
  , prim "*"      []        (TPair TInt TInt)   TInt    $ \(VPair (VInt x) (VInt y)) -> VInt (x * y)
  , prim "max"    []        (TPair TInt TInt)   TInt    $ \(VPair (VInt x) (VInt y)) -> VInt (max x y)
  , prim "/"      []        (TPair TInt TInt)   TDouble
      $ \(VPair (VInt x) (VInt y)) -> VDouble (fromIntegral x / fromIntegral y)
  , prim "++"     []        (TPair TString TString) TString
      $ \(VPair (VString x) (VString y)) -> VString (x ++ y)
  , prim "<"      []        (TPair TInt TInt)   TBool   $ \(VPair (VInt x) (VInt y)) -> VBool (x < y)
  , prim ">"      []        (TPair TInt TInt)   TBool   $ \(VPair (VInt x) (VInt y)) -> VBool (x > y)
  , prim "<="     []        (TPair TInt TInt)   TBool   $ \(VPair (VInt x) (VInt y)) -> VBool (x <= y)
  , prim ">="     []        (TPair TInt TInt)   TBool   $ \(VPair (VInt x) (VInt y)) -> VBool (x >= y)
  , prim "&&"     []        (TPair TBool TBool) TBool   $ \(VPair (VBool x) (VBool y)) -> VBool (x && y)
  , prim "||"     []        (TPair TBool TBool) TBool   $ \(VPair (VBool x) (VBool y)) -> VBool (x || y)
  , prim "=="     ["a"]     (TPair ta ta)       TBool   $ \(VPair x y) -> VBool (x == y)
  , prim "/="     ["a"]     (TPair ta ta)       TBool   $ \(VPair x y) -> VBool (x /= y)
  , prim "hash"   []        TName               TString $ \(VName a) -> VString $ show $ hashUnique a
  , prim "fst"    ["a","b"] (TPair ta tb)       ta      $ \(VPair x _) -> x
  , prim "snd"    ["a","b"] (TPair ta tb)       tb      $ \(VPair _ x) -> x
  , prim "insert" ["k","v"] (TPair (TPair tk tv) (TMap tk tv)) (TMap tk tv)
      $ \(VPair (VPair k v) (VMap m)) -> VMap ((k, v) : filter (\(k', _) -> k' /= k) m)
  , prim "remove" ["k","v"] (TPair tk (TMap tk tv)) (TMap tk tv)
      $ \(VPair k (VMap m)) -> VMap (filter (\(k', _) -> k' /= k) m)
  , prim "lookup" ["k","v"] (TPair tk (TMap tk tv)) tv
      $ \(VPair k (VMap m)) -> case Prelude.lookup k m of
          Just v  -> v
          Nothing -> error $ "Key not found in map: " ++ show k
  , prim "member" ["k","v"] (TPair tk (TMap tk tv)) TBool
      $ \(VPair k (VMap m)) -> VBool (any (\(k', _) -> k' == k) m)
  , prim "::"     ["a"]     (TPair ta (TList ta)) (TList ta)
      $ \(VPair x (VList xs)) -> VList (x : xs)
  , prim "null"   ["a"]     (TList ta)          TBool   $ \(VList xs) -> VBool (null xs)
  , prim "_uncons" ["a"]    (TList ta) (TEither TUnit (TPair ta (TList ta)))
      $ \(VList xs') -> case xs' of
          []     -> VEither L VUnit
          (x:xs) -> VEither R (VPair x (VList xs))
  , prim "absurd" ["a"]     TVoid               ta
      $ \_ -> error "absurd: applied to a value of type Void"
  , prim "_matchfail" ["a"] TUnit               ta
      $ \_ -> error "Pattern match failure: no case matched"
  , ("[]",    Primitive (Forall (Set.singleton "a") mempty (TList ta))        (VList []))
  , ("empty", Primitive (Forall (Set.fromList ["k","v"]) mempty (TMap tk tv)) (VMap []))
  ]
  where
    ta = TVar "a"
    tb = TVar "b"
    tk = TVar "k"
    tv = TVar "v"
    prim name vs t1 t2 f =
      (name, Primitive (mkPrimScheme vs t1 t2) (VPrimitive (\v -> CReturn $! f v)))

mkPrimScheme :: [Ident] -> ValueType -> ValueType -> Scheme
mkPrimScheme vs t1 t2 =
  Forall (Set.fromList vs) (Set.singleton "e") (TFun t1 (TComp t2 (Open mempty "e")))

-- the desugarer emits these; a leading underscore is unbindable in the surface
aliases :: Map.Map Ident Primitive
aliases = Map.fromList
  [ ("_fst",    primitives Map.! "fst")
  , ("_snd",    primitives Map.! "snd")
  , ("_eq",     primitives Map.! "==")
  ]

inbuiltOps :: [(Op, Arity)]
inbuiltOps =
  [ ("fresh",     Arity TUnit TName)
  , ("print",     Arity TString TUnit)
  , ("read",      Arity TUnit TString)
  , ("flip",      Arity TUnit TBool)
  , ("bernoulli", Arity TDouble TBool)
  , ("uniform",   Arity TUnit TDouble)
  ]

initialEnv :: Env
initialEnv = Env (Map.map primValue (primitives <> aliases))

initialCtx :: Context
initialCtx = Map.map primScheme (primitives <> aliases)
