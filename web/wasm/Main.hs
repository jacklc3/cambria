-- Wasm entry point for the playground: the browser calls runCambria with the
-- program text and gets back a {ok, output} object.

module Main where

import Control.Exception (ErrorCall(..), SomeException, throwIO, try)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import GHC.Wasm.Prim

import Runtime (OpTable, runIO, inbuiltOps)
import Syntax

foreign export javascript "runCambria"
  runCambria :: JSString -> IO JSVal

-- Builds the reply object. (!!) to align the two backend types.
foreign import javascript unsafe "({ ok: !!$1, output: $2 })"
  mkResult :: Bool -> JSString -> IO JSVal

main :: IO ()
main = return ()   -- unused: linked with -no-hs-main, driven from JS

runCambria :: JSString -> IO JSVal
runCambria src = do
  ref     <- newIORef []
  outcome <- try (runIO (playgroundOps ref) (fromJSString src))
  prefix  <- unlines . reverse <$> readIORef ref
  case outcome of
    Left e           -> reply False (prefix ++ message e)
    Right (Left err) -> reply False (prefix ++ err)
    Right (Right s)  -> reply True  (prefix ++ s)
  where reply ok = mkResult ok . toJSString

-- Exception text without the call stack GHC appends to error calls.
message :: SomeException -> String
message = takeWhile (/= '\n') . show

-- Shadows print and read: print collects into the buffer, and read has no console.
playgroundOps :: IORef [String] -> OpTable
playgroundOps ref =
  [ ("print", \(VString s) k -> modifyIORef' ref (s :) >> return (CApp k VUnit))
  , ("read",  \_           _ -> throwIO (ErrorCall "!read is not supported in the playground"))
  ] ++ inbuiltOps
