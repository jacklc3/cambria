-- Wasm entry point for the Cambria playground.
--
-- Built with the GHC wasm backend in reactor mode; the browser calls
-- runCambria with the program text and gets back the combined output
-- (print lines, then the result and its type), the same format as the CLI.
--
-- Differences from the CLI driver (src/Main.hs):
--   * !print output is accumulated and returned rather than written to stdout
--   * !read is not available (there is no stdin in the playground)

module Main where

import GHC.Wasm.Prim

import Environment (initialEnv, primitiveOps)
import Eval (Result(..), eval)
import Syntax

import Parsing.Parser (parse)
import Parsing.Desugar (desugar)

import Inference.Infer (infer)

foreign export javascript "runCambria"
  runCambria :: JSString -> IO JSString

main :: IO ()
main = return ()   -- unused: linked with -no-hs-main, driven from JS

runCambria :: JSString -> IO JSString
runCambria src = toJSString <$> runSource (fromJSString src)

runSource :: String -> IO String
runSource src =
  case parse src of
    Left err -> return err
    Right sugaredAst ->
      let ast = desugar sugaredAst in
      case infer ast of
        Left err -> return err
        Right t -> do
          (out, printed) <- run initialEnv ast []
          return $ concatMap (++ "\n") (reverse printed)
                   ++ out ++ " : " ++ show t

-- The CLI's evalIO loop, with print captured and read refused.
run :: Env -> Computation -> [String] -> IO (String, [String])
run env c printed =
  case eval env c of
    Pure v -> return (show (Pure v), printed)
    Impure "print" (VString s) k -> run env (CApp k VUnit) (s : printed)
    Impure "read" _ _ ->
      return ("!read is not supported in the playground", printed)
    Impure op v k ->
      case lookup op primitiveOps of
        Just prim -> do
          c' <- prim v k
          run env c' printed
        Nothing -> return (show (Impure op v k), printed)
