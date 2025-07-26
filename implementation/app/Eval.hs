module Eval where

import Ast
import Gensym
import Data.Map (Map)
import Data.List (foldl', find)
import qualified Data.Map as Map

type M a = SymbolGen Parameter a

data EvalResult
    = Pure Value
    | Impure OpName Value (Value -> Computation) Env
    | RuntimeError String

instance Show EvalResult where
    show (Pure v)          = "Pure " ++ show v
    show (Impure op v _ _) = "Impure " ++ op ++ " " ++ show v
    show (RuntimeError s)  = "Error: " ++ s

eval :: Env -> Computation -> M EvalResult
eval env (CReturn val) = return $ Pure (evalValue env val)

eval env (CApp funVal argVal) =
  case evalValue env funVal of
    VClosure var body cEnv ->
      let newEnv = Map.insert var (evalValue env argVal) cEnv
      in eval newEnv body
    {-
    VRecFun recEnv fName xName body ->
      let evaluatedArg = evalValue env argVal
          recClosure = VRecFun recEnv fName xName body
          newEnv = Map.insert xName evaluatedArg (Map.insert fName recClosure recEnv)
      in eval newEnv body
    -}
    VContinuation k kEnv -> eval kEnv (k (evalValue env argVal))
    _ -> return $ RuntimeError $ "Cannot apply closure or continutation: " ++ show funVal

eval env (CIf cond c1 c2) =
  case evalValue env cond of
    VBool True -> eval env c1
    VBool False -> eval env c2
    _ -> return $ RuntimeError $ "If condition must be a boolean, but got: " ++ show cond

eval env (CSeq var c1 c2) =
  eval env c1 >>= (\r -> case r of
    Pure val -> eval (Map.insert var val env) c2
    Impure op v k opEnv -> return $ Impure op v (\res -> CSeq var (k res) c2) opEnv
    err@(RuntimeError _) -> return $ err
  )

eval env (COp op v) =
    return $ Impure op (evalValue env v) CReturn env

eval env (CHandle h c) = evalHandle env h c

evalHandle :: Env -> Handler -> Computation -> M EvalResult
evalHandle env h c =
  eval env c >>= (\r ->
    case r of
      Pure val -> let (var, retBody) = hReturnClause h
                  in eval (Map.insert var val env) retBody
      Impure op v cont opEnv ->
        case findOpClause op (hOpClauses h) of
          Just (x, k, b) ->
            let hCont = VContinuation (\v' -> CHandle h (cont v')) opEnv
                hEnv = foldl' (flip (uncurry Map.insert)) env [(x, v), (k, hCont)]
            in  eval hEnv b
          Nothing -> return $ Impure op v (\v' -> CHandle h (cont v')) opEnv
      err@(RuntimeError _) -> return err)
  where
    findOpClause op cs = case find (\(op', _, _, _) -> op == op') cs of
                           Nothing           -> Nothing
                           Just (_, x, k, c) -> Just (x, k, c)

evalValue :: Env -> Value -> Value
evalValue env (VVar name) =
  case Map.lookup name env of
    Just val -> evalValue env val
    Nothing -> error $ "Unbound variable: " ++ name
evalValue env (VPair v1 v2) = VPair (evalValue env v1) (evalValue env v2)
evalValue env (VFun var c) = VClosure var c env
evalValue _ val = val

initialEnv :: Env
initialEnv = Map.fromList [
    ("+", primBinOpInt (\x y -> VInt (x + y))),
    ("-", primBinOpInt (\x y -> VInt (x - y))),
    ("*", primBinOpInt (\x y -> VInt (x * y))),
    ("++", primBinOpStr (\x y -> VString (x ++ y))),
    ("max", primBinOpInt (\x y -> VInt (max x y))),
    ("fst", VContinuation (\(VPair x _) -> CReturn x) Map.empty),
    ("snd", VContinuation (\(VPair _ x) -> CReturn x) Map.empty) ]

primBinOpInt :: (Integer -> Integer -> Value) -> Value
primBinOpInt op = VContinuation handle_x Map.empty
  where
    handle_x (VInt x) = CReturn (VContinuation (handle_y x) Map.empty)
    handle_x _        = error "Type error: first argument to primitive was not an integer."
    handle_y x (VInt y) = CReturn (op x y)
    handle_y _ _        = error "Type error: second argument to primitive was not an integer."

primBinOpStr :: (String -> String -> Value) -> Value
primBinOpStr op = VContinuation handle_x Map.empty
  where
    handle_x (VString x)   = CReturn (VContinuation (handle_y x) Map.empty)
    handle_x _             = error "Type error: first argument was not a string."
    handle_y x (VString y) = CReturn (op x y)
    handle_y v v'          = error ("Type error: second argument was not a string: " ++ show v ++ show v')
