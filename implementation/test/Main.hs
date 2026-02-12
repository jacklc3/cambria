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
      (Right "(t0 -> t0!{e1})!{}")

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
-- Let-polymorphism tests (Kammar-Pretnar style: no value restriction)
-- ============================================================

polyTests :: [TestCase]
polyTests =
  [ TestCase "poly: identity function used at two types"
      ( "do id <- return (fun x -> return x) in\n"
     ++ "do a <- id 1 in\n"
     ++ "do b <- id true in\n"
     ++ "return (a, b)" )
      (Right "(Int & Bool)!{}")

  , TestCase "poly: const function used at two types"
      ( "do const <- return (fun x -> return (fun y -> return x)) in\n"
     ++ "do a <- const 42 true in\n"
     ++ "do b <- const \"hello\" 99 in\n"
     ++ "return (a, b)" )
      (Right "(Int & Str)!{}")

  , TestCase "poly: swap function used at two pair types"
      ( "do swap <- return (fun p -> return (snd p, fst p)) in\n"
     ++ "do a <- swap (1, true) in\n"
     ++ "do b <- swap (\"hello\", 42) in\n"
     ++ "return (fst a, fst b)" )
      (Right "(Bool & Int)!{}")

  , TestCase "poly: higher-order polymorphic apply"
      ( "do apply <- return (fun f -> f ()) in\n"
     ++ "do a <- apply (fun x -> return 42) in\n"
     ++ "do b <- apply (fun x -> return true) in\n"
     ++ "return (a, b)" )
      (Right "(Int & Bool)!{}")

  , TestCase "poly: generalization works alongside effects"
      ( "do _ <- !flip () in\n"
     ++ "do id <- return (fun x -> return x) in\n"
     ++ "do a <- id 42 in\n"
     ++ "do b <- id true in\n"
     ++ "return (a, b)" )
      (Right "(Int & Bool)!{ flip : Unit ~> Bool }")

  , TestCase "poly: polymorphic function used across effectful sequencing"
      ( "do id <- return (fun x -> return x) in\n"
     ++ "do a <- id 1 in\n"
     ++ "do _ <- !print \"between\" in\n"
     ++ "do b <- id true in\n"
     ++ "return (a, b)" )
      (Right "(Int & Bool)!{ print : Str ~> Unit }")

  , TestCase "poly: nested polymorphic bindings"
      ( "do id <- return (fun x -> return x) in\n"
     ++ "do f <- id (fun y -> return (y, y)) in\n"
     ++ "do a <- f 1 in\n"
     ++ "do b <- f true in\n"
     ++ "return (a, b)" )
      (Right "((Int & Int) & (Bool & Bool))!{}")

  , TestCase "poly: effect return type var NOT generalized (soundness)"
      ( "do r <- !myeffect () in\n"
     ++ "do a <- return r in\n"
     ++ "do b <- return r in\n"
     ++ "return (a, b)" )
      (Right "(t0 & t0)!{ myeffect : Unit ~> t0 }")

  , TestCase "poly: polymorphic function with handler"
      ( "do id <- return (fun x -> return x) in\n"
     ++ "with handler {\n"
     ++ "  return x -> return x,\n"
     ++ "  ask v k -> k 42\n"
     ++ "} handle (\n"
     ++ "  do a <- id (!ask ()) in\n"
     ++ "  return a\n"
     ++ ")" )
      (Right "Int!{}")

  , TestCase "poly: polymorphic list operations"
      ( "do empty1 <- nil () in\n"
     ++ "do empty2 <- nil () in\n"
     ++ "do ints <- cons (1, empty1) in\n"
     ++ "do bools <- cons (true, empty2) in\n"
     ++ "return (head ints, head bools)" )
      (Right "(Int & Bool)!{}")
  ]

-- ============================================================
-- Polymorphism + Parameter interaction tests
-- ============================================================

polyParamTests :: [TestCase]
polyParamTests =
  [ TestCase "poly+param: swap refs — poly function at abstract $p type"
      ( "with handler {\n"
     ++ "  $p -> Unique,\n"
     ++ "  return x  -> return (fun _ -> return x),\n"
     ++ "  get a k -> return (fun s -> k (lookup (a, s)) s),\n"
     ++ "  set x k -> return (fun s -> k () (insert (x, s))),\n"
     ++ "  ref x k -> do a <- !unique () in return (fun s ->\n"
     ++ "    k a (insert (((a, x), s)))),\n"
     ++ "  finally s -> s (empty ())\n"
     ++ "} handle (\n"
     ++ "  declare !get : $p ~> Int.\n"
     ++ "  declare !set : $p & Int ~> Unit.\n"
     ++ "  declare !ref : Int ~> $p.\n"
     ++ "  do swap <- return (fun pair -> return (snd pair, fst pair)) in\n"
     ++ "  do swapped_ints <- swap (10, 20) in\n"
     ++ "  do a <- !ref 100 in\n"
     ++ "  do b <- !ref 200 in\n"
     ++ "  do swapped_refs <- swap (a, b) in\n"
     ++ "  do v1 <- !get (fst swapped_refs) in\n"
     ++ "  do v2 <- !get (snd swapped_refs) in\n"
     ++ "  return (swapped_ints, (v1, v2))\n"
     ++ ")" )
      (Right "((Int & Int) & (Int & Int))!{ unique : Unit ~> Unique }")

  , TestCase "poly+param: with_ref combinator — poly callback return type"
      ( "with handler {\n"
     ++ "  $p -> Unique,\n"
     ++ "  return x  -> return (fun _ -> return x),\n"
     ++ "  get a k -> return (fun s -> k (lookup (a, s)) s),\n"
     ++ "  set x k -> return (fun s -> k () (insert (x, s))),\n"
     ++ "  ref x k -> do a <- !unique () in return (fun s ->\n"
     ++ "    k a (insert (((a, x), s)))),\n"
     ++ "  finally s -> s (empty ())\n"
     ++ "} handle (\n"
     ++ "  declare !get : $p ~> Int.\n"
     ++ "  declare !set : $p & Int ~> Unit.\n"
     ++ "  declare !ref : Int ~> $p.\n"
     ++ "  do with_ref <- return (fun init -> return (fun body ->\n"
     ++ "    do r <- !ref init in body r\n"
     ++ "  )) in\n"
     ++ "  do mk1 <- with_ref 42 in\n"
     ++ "  do r1 <- mk1 (fun r -> do v <- !get r in return v) in\n"
     ++ "  do mk2 <- with_ref 0 in\n"
     ++ "  do r2 <- mk2 (fun r -> do v <- !get r in return (v == 0)) in\n"
     ++ "  do mk3 <- with_ref 5 in\n"
     ++ "  do r3 <- mk3 (fun r ->\n"
     ++ "    do before <- !get r in\n"
     ++ "    do _ <- !set (r, before * 2) in\n"
     ++ "    do after <- !get r in\n"
     ++ "    return (before, after)\n"
     ++ "  ) in\n"
     ++ "  return (r1, (r2, r3))\n"
     ++ ")" )
      (Right "(Int & (Bool & (Int & Int)))!{ unique : Unit ~> Unique }")

  , TestCase "poly+param: map_pair at Int, $p->Int, and Int->$p"
      ( "with handler {\n"
     ++ "  $p -> Unique,\n"
     ++ "  return x  -> return (fun _ -> return x),\n"
     ++ "  get a k -> return (fun s -> k (lookup (a, s)) s),\n"
     ++ "  set x k -> return (fun s -> k () (insert (x, s))),\n"
     ++ "  ref x k -> do a <- !unique () in return (fun s ->\n"
     ++ "    k a (insert (((a, x), s)))),\n"
     ++ "  finally s -> s (empty ())\n"
     ++ "} handle (\n"
     ++ "  declare !get : $p ~> Int.\n"
     ++ "  declare !set : $p & Int ~> Unit.\n"
     ++ "  declare !ref : Int ~> $p.\n"
     ++ "  do map_pair <- return (fun f -> return (fun pair ->\n"
     ++ "    do x <- f (fst pair) in\n"
     ++ "    do y <- f (snd pair) in\n"
     ++ "    return (x, y)\n"
     ++ "  )) in\n"
     ++ "  do double_both <- map_pair (fun n -> return (n * 2)) in\n"
     ++ "  do doubled <- double_both (3, 7) in\n"
     ++ "  do a <- !ref 100 in\n"
     ++ "  do b <- !ref 200 in\n"
     ++ "  do read_both <- map_pair (fun r -> !get r) in\n"
     ++ "  do vals <- read_both (a, b) in\n"
     ++ "  do sum <- return (fst vals + snd vals) in\n"
     ++ "  do alloc_both <- map_pair (fun n -> !ref n) in\n"
     ++ "  do new_refs <- alloc_both (sum, fst doubled + snd doubled) in\n"
     ++ "  return (doubled, (sum, (!get (fst new_refs), !get (snd new_refs))))\n"
     ++ ")" )
      (Right "((Int & Int) & (Int & (Int & Int)))!{ unique : Unit ~> Unique }")

  , TestCase "poly+param: polymorphic incr on abstract refs"
      ( "with handler {\n"
     ++ "  $p -> Unique,\n"
     ++ "  return x  -> return (fun _ -> return x),\n"
     ++ "  get a k -> return (fun s -> k (lookup (a, s)) s),\n"
     ++ "  set x k -> return (fun s -> k () (insert (x, s))),\n"
     ++ "  ref x k -> do a <- !unique () in return (fun s ->\n"
     ++ "    k a (insert (((a, x), s)))),\n"
     ++ "  finally s -> s (empty ())\n"
     ++ "} handle (\n"
     ++ "  declare !get : $p ~> Int.\n"
     ++ "  declare !set : $p & Int ~> Unit.\n"
     ++ "  declare !ref : Int ~> $p.\n"
     ++ "  do incr <- return (fun r ->\n"
     ++ "    do v <- !get r in\n"
     ++ "    !set (r, v + 1)\n"
     ++ "  ) in\n"
     ++ "  do a <- !ref 10 in\n"
     ++ "  do b <- !ref 20 in\n"
     ++ "  do _ <- incr a in\n"
     ++ "  do _ <- incr b in\n"
     ++ "  do _ <- incr a in\n"
     ++ "  return (!get a + !get b)\n"
     ++ ")" )
      (Right "Int!{ unique : Unit ~> Unique }")

  , TestCase "poly+param: map at (Int->$p), ($p->Int), (Int->Bool)"
      ( "with handler {\n"
     ++ "  $p -> Unique,\n"
     ++ "  return x  -> return (fun _ -> return x),\n"
     ++ "  get a k -> return (fun s -> k (lookup (a, s)) s),\n"
     ++ "  set x k -> return (fun s -> k () (insert (x, s))),\n"
     ++ "  ref x k -> do a <- !unique () in return (fun s ->\n"
     ++ "    k a (insert (((a, x), s)))),\n"
     ++ "  finally s -> s (empty ())\n"
     ++ "} handle (\n"
     ++ "  declare !get : $p ~> Int.\n"
     ++ "  declare !set : $p & Int ~> Unit.\n"
     ++ "  declare !ref : Int ~> $p.\n"
     ++ "  do map <- return (rec map f -> return (fun xs ->\n"
     ++ "    if isnil xs then nil ()\n"
     ++ "    else do hd <- f (head xs) in\n"
     ++ "         do mapper <- map f in\n"
     ++ "         do tl <- mapper (tail xs) in\n"
     ++ "         cons (hd, tl)\n"
     ++ "  )) in\n"
     ++ "  do allocator <- map (fun n -> !ref n) in\n"
     ++ "  do refs <- allocator (cons (10, cons (20, cons (30, nil ())))) in\n"
     ++ "  do reader <- map (fun r -> !get r) in\n"
     ++ "  do vals <- reader refs in\n"
     ++ "  do first <- return (head vals) in\n"
     ++ "  do tester <- map (fun n -> return (n == 20)) in\n"
     ++ "  do checks <- tester vals in\n"
     ++ "  return (first, checks)\n"
     ++ ")" )
      (Right "(Int & List Bool)!{ unique : Unit ~> Unique }")
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

  , TestCase "unknown operation inferred bottom-up"
      "!nonexistent ()"
      (Right "t0!{ nonexistent : Unit ~> t0 }")

  , TestCase "error: duplicate declare with conflicting use"
      ( "declare !ask : Unit ~> Int.\n"
     ++ "do x <- !ask () in\n"
     ++ "declare !ask : Unit ~> Bool.\n"
     ++ "do y <- !ask () in\n"
     ++ "return (x, y)" )
      (Left "Type mismatch: Int vs Bool")

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
        , polyTests
        , polyParamTests
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
