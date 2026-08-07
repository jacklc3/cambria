# Cambria: A Language for Parameterized Handlers

Cambria is a functional programming language combining **algebraic effects and handlers** and **parameterized algebraic theories (PATs)**.
It is a tool for programming with computational effects that involve dynamically allocated resources, such as memory cells, code pointers, or threads.

## Key Features

* **Effects and Handlers:** Cambria is built on a fine-grained call-by-value effect calculus.
* **Type and Effect System:** We introduce a new parameter type that may be instantiated based on the effect implementation, while maintaining type safety.
* **Haskell Implementation:** The language is implemented with a parser, desugarer, type inference, and evaluator.
* **Browser Playground:** A playground runs the interpreter compiled to WebAssembly at [cambria-lang.org](https://cambria-lang.org).

## Examples

### Local State

A handler for local state, where memory cells can be dynamically created and manipulated.
The state is represented as a function from parameters (memory locations) to their stored values, and the location names are supplied by a separate fresh-name handler.
From [examples/local_state.cba](examples/local_state.cba), which returns `3`:

```
-- Local state: a name allocator and a store over the names
-- cf. Completeness for algebraic theories of local state, FoSSaCS 2010
with [$name -> Int] handler {
  return x   -> return (fun _ -> return x),
  fresh _ k  -> return (fun n -> k n (n+1)),
  eq (a,b) k -> k (a == b),
  finally f  -> f 0
} handle

effect !fresh : Unit ~> $name.
with [$loc -> $name] handler {
  return x    -> return (fun _ -> return x),
  get a k     -> return (fun s -> k (s a) s),
  set (a,x) k -> return (fun s ->
                   k () (fun b -> if !eq (a, b) then return x else s b)),
  ref x k     -> do a <- !fresh () in return (fun s ->
                   k a (fun b -> if !eq (a, b) then return x else s b)),
  finally s   -> s (fun _ -> return 0)
} handle

effect !ref : Int ~> $loc.
effect !get : $loc ~> Int.
effect !set : $loc * Int ~> Unit.
do a <- !ref 2 in
do b <- !ref 3 in
do _ <- !set (a, !get b) in
!get a
```

### Polymorphic Map over Abstract References

Polymorphic functions may be instantiated at parameter types.
The same `map` function works at types `Int -> $loc` and `$loc -> Bool`.
From [examples/poly_map.cba](examples/poly_map.cba), which returns `[False, True, False]`:

```
-- Polymorphic map over abstract references
-- State handlers as in local_state.cba
with [$name -> Int] handler {
  return x   -> return (fun _ -> return x),
  fresh _ k  -> return (fun n -> k n (n+1)),
  eq (a,b) k -> k (a == b),
  finally f  -> f 0
} handle

effect !fresh : Unit ~> $name.
with [$loc -> $name] handler {
  return x    -> return (fun _ -> return x),
  get a k     -> return (fun s -> k (s a) s),
  set (a,x) k -> return (fun s ->
                   k () (fun b -> if !eq (a, b) then return x else s b)),
  ref x k     -> do a <- !fresh () in return (fun s ->
                   k a (fun b -> if !eq (a, b) then return x else s b)),
  finally s   -> s (fun _ -> return 0)
} handle

effect !ref : Int ~> $loc.
effect !get : $loc ~> Int.
effect !set : $loc * Int ~> Unit.
do map <- return (rec map f xs ->
  case uncons xs of {
    inl _        -> return [],
    inr (x, xs') -> f x :: map f xs'
  })
in
do vals <- return (10 :: 20 :: 30 :: []) in
do refs <- map (fun n -> !ref n) vals in
do checks <- map (fun n -> !get n == 20) refs in
return checks
```

### Code Jumps

A handler for a theory of code jumps, with labels and goto commands.
From [examples/jumps.cba](examples/jumps.cba), which returns `inl 3`:

```
-- First-class labels and goto via multi-shot continuations
-- cf. Substitution, jumps, and algebraic effects, LICS 2014
with [$name -> Int] handler {
  return x  -> return (fun _ -> return x),
  fresh _ k -> return (fun n -> k n (n+1)),
  eq (a,b) k -> k (a == b),
  finally f -> f 0
} handle

effect !fresh : Unit ~> $name.
with [$p -> $name] handler {
  return x  -> return (inl x),
  goto a _  -> return (inr a),
  label _ k -> do c <- !fresh () in
    case k (inl c) of {
      inl x -> return (inl x),
      inr d -> if !eq (d, c) then k (inr ()) else return (inr d)
    }
} handle

effect !label : Unit ~> $p + Unit.
effect !goto  : $p   ~> Void.
case !label () of {
  inl a -> case !label () of {
    inl b -> !goto a; return 1,
    inr _ -> return 2
  },
  inr _ -> return 3
}
```

## Getting Started

### Prerequisites

* [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
* [Cabal](https://www.haskell.org/cabal/)

### Building

To build the Cambria interpreter run:

```bash
cabal build
```

### Running

Run a Cambria program using the following command:

```bash
cabal run cambria -- <path_to_file>
```
For example:

```bash
cabal run cambria -- examples/local_state.cba
```

### Testing

Run the test cases in [test/cases/](test/cases/) and examples in
[examples/](examples/) with the following command:

```bash
cabal test
```
