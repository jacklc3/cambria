module Main where

import System.Exit (exitFailure, exitSuccess)
import Parsing.Parser (parse)
import Parsing.Desugar (desugar)
import Inference.Infer (infer)

-- | Parse and type-check a source string, returning the type as a string
-- or an error message.
inferSource :: String -> Either String String
inferSource src =
  case parse src of
    Left err  -> Left err
    Right ast -> case infer (desugar ast) of
      Left err -> Left err
      Right t  -> Right (show t)

-- | A test case: file description, source code, and expected result.
-- Right s means we expect successful inference with type string s.
-- Left s means we expect a type error containing substring s.
data TestCase = TestCase
  { testName     :: String
  , testSource   :: String
  , testExpected :: Either String String
  }

-- ============================================================
-- Core examples (from examples/ directory)
-- ============================================================

coreTests :: [TestCase]
coreTests =
  [ TestCase "unique: fresh resource identifiers"
      "do x <- !unique () in\ndo y <- !unique () in\ndo z <- !unique () in\nreturn ((x,y),z)"
      (Right "((Unique & Unique) & Unique)!{ unique : Unit ~> Unique }")

  , TestCase "print: string output effect"
      "!print \"test\""
      (Right "Unit!{ print : Str ~> Unit }")

  , TestCase "random: coin flip effect"
      "!flip ()"
      (Right "Bool!{ flip : Unit ~> Bool }")

  , TestCase "pickMax: handler eliminating decide effect"
      ( "with handler {\n"
     ++ "  decide v k -> max (k true, k false)\n"
     ++ "} handle (\n"
     ++ "  do x1 <- if !decide () then return 15 else return 30 in\n"
     ++ "  do x2 <- if !decide () then return 5  else return 10 in\n"
     ++ "  return (x1 - x2)\n"
     ++ ")" )
      (Right "Int!{}")

  , TestCase "collect: handler collecting print output"
      ( "with handler {\n"
     ++ "  return x -> return (x, \"\"),\n"
     ++ "  print s k -> do x <- k () in\n"
     ++ "    return (fst x, s ++ (snd x))\n"
     ++ "} handle (\n"
     ++ "  !print \"A\"; !print \"B\"; !print \"C\"\n"
     ++ ")" )
      (Right "(Unit & Str)!{}")
  ]

-- ============================================================
-- Bidirectional type checking tests
-- ============================================================

bidiTests :: [TestCase]
bidiTests =
  [ TestCase "checkValue: function application checks argument"
      "(fun x -> return (x + 1)) 3"
      (Right "Int!{}")

  , TestCase "checkValue: pair checked structurally"
      "return ((1, true), \"hello\")"
      (Right "((Int & Bool) & Str)!{}")

  , TestCase "checkValue: either left branch"
      "case inl 42 of {\n  inl x -> return (x + 1),\n  inr y -> return (y - 1)\n}"
      (Right "Int!{}")

  , TestCase "checkValue: if checks condition against Bool"
      "if true then return 1 else return 2"
      (Right "Int!{}")

  , TestCase "checkValue: polymorphic identity function"
      "return (fun x -> return x)"
      (Right "(t0 -> t0!{})!{}")

  , TestCase "inferValue: recursive function preserves effects"
      ( "do f <- return (rec f n -> if n == 0 then return 0 else do _ <- !print \"tick\" in f (n - 1)) in\n"
     ++ "f 3" )
      (Right "Int!{ print : Str ~> Unit }")
  ]

-- ============================================================
-- Handler tests
-- ============================================================

handlerTests :: [TestCase]
handlerTests =
  [ TestCase "handler: passthrough (print handled, effect removed)"
      ( "with handler {\n"
     ++ "  return x -> return x,\n"
     ++ "  print s k -> k ()\n"
     ++ "} handle (\n"
     ++ "  !print \"hello\"\n"
     ++ ")" )
      (Right "Unit!{}")

  , TestCase "handler: effect leak from op clause body"
      ( "with handler {\n"
     ++ "  return x -> return x,\n"
     ++ "  decide v k -> do _ <- !print \"deciding\" in k true\n"
     ++ "} handle (\n"
     ++ "  if !decide () then return 1 else return 2\n"
     ++ ")" )
      (Right "Int!{ print : Str ~> Unit }")

  , TestCase "handler: unifies effect arities between handler and body"
      ( "with handler {\n"
     ++ "  return x -> return (x + 1),\n"
     ++ "  ask v k -> k \"hello\"\n"
     ++ "} handle (\n"
     ++ "  do s <- !ask () in\n"
     ++ "  return s\n"
     ++ ")" )
      (Left "Type mismatch")

  , TestCase "handler: op return type consistent with body usage"
      ( "with handler {\n"
     ++ "  return x -> return x,\n"
     ++ "  choose v k -> k true\n"
     ++ "} handle (\n"
     ++ "  do b <- !choose () in\n"
     ++ "  if b then return 1 else return 2\n"
     ++ ")" )
      (Right "Int!{}")

  , TestCase "handler: unhandled effects pass through"
      ( "with handler {\n"
     ++ "  return x -> return x,\n"
     ++ "  decide v k -> k true\n"
     ++ "} handle (\n"
     ++ "  do b <- !decide () in\n"
     ++ "  if b then !print \"yes\" else !print \"no\"\n"
     ++ ")" )
      (Right "Unit!{ print : Str ~> Unit }")

  , TestCase "handler: finally clause (local state)"
      ( "with handler {\n"
     ++ "  return x  -> return (fun _ -> return x),\n"
     ++ "  get a k -> return (fun s -> k (s a) s),\n"
     ++ "  set x k -> return (fun s -> k () (fun a -> if a == fst x then snd x else s a)),\n"
     ++ "  ref x k -> do a <- !unique () in return (fun s ->\n"
     ++ "    k a (fun b -> if b == a then return x else s b)),\n"
     ++ "  finally s -> s (fun _ -> return 0)\n"
     ++ "} handle (\n"
     ++ "  do a <- !ref 2 in\n"
     ++ "  do b <- !ref 3 in\n"
     ++ "  (!set (a, !get b + !get a); !get a)\n"
     ++ ")" )
      (Right "Int!{ unique : Unit ~> Unique }")

  , TestCase "handler: type param inst missing op rejected (inline)"
      ( "with handler {\n"
     ++ "  $p -> Unique,\n"
     ++ "  return x -> return x,\n"
     ++ "  get a k -> k 42,\n"
     ++ "  ref x k -> k ()\n"
     ++ "} handle (\n"
     ++ "  declare !get : $p ~> Int.\n"
     ++ "  declare !set : $p & Int ~> Unit.\n"
     ++ "  declare !ref : Int ~> $p.\n"
     ++ "  do a <- !ref 0 in\n"
     ++ "  !set (a, !get a)\n"
     ++ ")" )
      (Left "does not handle operation")

  , TestCase "handler: type param inst missing op rejected (variable)"
      ( "do h <- return handler {\n"
     ++ "  $p -> Unique,\n"
     ++ "  return x -> return x,\n"
     ++ "  get a k -> k 42,\n"
     ++ "  ref x k -> k ()\n"
     ++ "} in\n"
     ++ "with h handle (\n"
     ++ "  declare !get : $p ~> Int.\n"
     ++ "  declare !set : $p & Int ~> Unit.\n"
     ++ "  declare !ref : Int ~> $p.\n"
     ++ "  do a <- !ref 0 in\n"
     ++ "  !set (a, !get a)\n"
     ++ ")" )
      (Left "does not handle operation")
  ]

-- ============================================================
-- Computation inference tests
-- ============================================================

compTests :: [TestCase]
compTests =
  [ TestCase "do: sequential binding"
      "do x <- return 1 in\ndo y <- return 2 in\nreturn (x + y, ())"
      (Right "(Int & Unit)!{}")

  , TestCase "do: effects accumulate across bindings"
      "do x <- !flip () in\ndo _ <- !print \"hello\" in\nreturn x"
      (Right "Bool!{ flip : Unit ~> Bool , print : Str ~> Unit }")

  , TestCase "function: closure captures variable"
      "do f <- (do x <- return 2 in return fun y -> return x) in f 3"
      (Right "Int!{}")

  , TestCase "substitution example"
      ( "with handler {\n"
     ++ "  return x  -> return (inl x),\n"
     ++ "  var a k -> return (inr a),\n"
     ++ "  sub _ k -> do c <- !unique () in\n"
     ++ "    case k (inl c) of {\n"
     ++ "      inl x -> return (inl x),\n"
     ++ "      inr d -> if d == c then k (inr ()) else return (inr d)\n"
     ++ "    }\n"
     ++ "} handle (\n"
     ++ "  case !sub () of {\n"
     ++ "    inl a -> case !sub () of {\n"
     ++ "      inl b -> !var a,\n"
     ++ "      inr _ -> return 2\n"
     ++ "    },\n"
     ++ "    inr _ -> return 3\n"
     ++ "  }\n"
     ++ ")" )
      (Right "(Int + Unique)!{ unique : Unit ~> Unique }")
  ]

-- ============================================================
-- Error tests (programs that should fail type checking)
-- ============================================================

errorTests :: [TestCase]
errorTests =
  [ TestCase "error: unbound variable"
      "return x"
      (Left "Unbound variable: x")

  , TestCase "error: if condition not Bool"
      "if 42 then return 1 else return 2"
      (Left "Type mismatch")

  , TestCase "error: unknown operation"
      "!nonexistent ()"
      (Left "Unknown operation: nonexistent")

  , TestCase "error: branch type mismatch"
      "if true then return 1 else return \"hello\""
      (Left "Type mismatch")

  , TestCase "error: function applied to wrong type"
      "(fun x -> return (x + 1)) true"
      (Left "Type mismatch")
  ]

-- ============================================================
-- Test runner
-- ============================================================

runTest :: TestCase -> (Bool, String)
runTest tc =
  let result = inferSource (testSource tc)
  in case (testExpected tc, result) of
    -- Expected success, got success: check exact match
    (Right expected, Right actual)
      | expected == actual -> (True,  "PASS: " ++ testName tc)
      | otherwise          -> (False, "FAIL: " ++ testName tc
                                   ++ "\n  Expected: " ++ expected
                                   ++ "\n  Actual:   " ++ actual)
    -- Expected error, got error: check substring match
    (Left expectedErr, Left actualErr)
      | expectedErr `isInfixOf` actualErr -> (True,  "PASS: " ++ testName tc)
      | otherwise                          -> (False, "FAIL: " ++ testName tc
                                                   ++ "\n  Expected error containing: " ++ expectedErr
                                                   ++ "\n  Actual error: " ++ actualErr)
    -- Expected success, got error
    (Right expected, Left err) -> (False, "FAIL: " ++ testName tc
                                       ++ "\n  Expected type: " ++ expected
                                       ++ "\n  Got error: " ++ err)
    -- Expected error, got success
    (Left expectedErr, Right actual) -> (False, "FAIL: " ++ testName tc
                                             ++ "\n  Expected error containing: " ++ expectedErr
                                             ++ "\n  Got type: " ++ actual)

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _          = True
isPrefixOf _ []          = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

tails :: [a] -> [[a]]
tails []     = [[]]
tails xs@(_:xs') = xs : tails xs'

main :: IO ()
main = do
  let allTests = concat
        [ coreTests
        , bidiTests
        , handlerTests
        , compTests
        , errorTests
        ]
  let results = map runTest allTests
  mapM_ (putStrLn . snd) results
  let passed = length (filter fst results)
  let total  = length results
  putStrLn ""
  putStrLn $ show passed ++ "/" ++ show total ++ " tests passed."
  if passed == total
    then exitSuccess
    else exitFailure
