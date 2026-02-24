# Cambria: A Language for Parameterized Handlers

Cambria is a functional programming language combining **algebraic effects and handlers** and **parameterized algebraic theories (PATs)**.
It aims to provide a practical and expressive tool for programming with computational effects that involve multiple, dynamically created instances of a resource, such as memory cells, code pointers, or threads.

## The Problem of Instances

The standard framework of algebraic effects and handlers provides a modular approach to defining and composing computational effects.
However, it is best suited for effects that are global in nature. It doesn't offer a direct solution for creating multiple, independent instances of an effect at runtime.

This is the "problem of instances" that Cambria aims to solve.

## The Solution: Parameterized Handlers

Cambria's solution is to integrate effect handlers with the denotational semantics of PATs.
PATs extend algebraic theories by allowing operations to be parameterized by resource identifiers, formally distinguishing between operations that *use* existing resources and those that *bind* new ones.

## Key Features

* **Effects and Handlers:** Cambria is built on a standard effect calculus with a fine-grained call-by-value strategy, separating inert *values* from potentially effectful *computations*.
* **Type and Effect System:** A type and effect system ensures that programs are well-behaved and do not get stuck due to type errors. We introduce a new parameter type that may be instantiated by handlers based on the effect implementation.
* **Haskell Implementation:** The language is implemented in Haskell, with a parser, desugarer, type inference, and evaluator.

## Examples

Here are a few examples that demonstrate the expressive power of Cambria.

### Local State

This example implements a handler for local state, where memory cells can be dynamically created and manipulated. The state is managed using a map from parameters (memory locations) to their stored values.

```
-- from examples/local_state.cba
with handler {
  return x  -> return (fun _ -> return x),
  get a k -> return (fun s -> k (lookup (a, s)) s),
  set x k -> return (fun s -> k () (insert (x, s))),
  ref x k -> do a <- !unique () in return (fun s ->
    k a (insert (((a, x), s)))),
  finally s -> s (empty ())
} handle (
  do a <- !ref 2 in
  do b <- !ref 3 in
  (!set (a, !get b + !get a); !get a)
)
-- returns 5
```

### Polymorphic Map over Abstract References

This example demonstrates polymorphic functions operating over lists of abstract references. The same `map` function works at types `Int -> $p` (allocation), `$p -> Int` (reading), and `Int -> Bool` (testing).

```
-- from implementation/examples/poly_fold_refs.cba (excerpt)
do map <- return (rec map f xs ->
  case uncons xs of {
    inl _        -> return [],
    inr (x, xs') -> f x :: map f xs'
  }
) in
do refs <- map (fun n -> !ref n) (10 :: 20 :: 30 :: []) in
do checks <- map (fun r -> !get r == 20) refs in
return vals
-- returns [False, True, False]
```

### Substitution and Jumps

This example implements a handler for a theory of substitution and jumps, which models a form of non-local control flow.

```
-- from examples/substitution.cba
with handler {
  return x  -> return (inl x),
  var a k -> return (inr a),
  sub _ k -> do c <- !unique () in
    case k (inl c) of {
      inl x -> return (inl x),
      inr d -> if d == c then k (inr ()) else return (inr d)
    }
} handle (
  case !sub () of {
    inl a -> case !sub () of {
      inl b -> !var a,
      inr _ -> return 2
    },
    inr _ -> return 3
  }
)
-- returns 3
```

## Getting Started

### Prerequisites

* [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
* [Cabal](https://www.haskell.org/cabal/)
* Alex
* Happy

### Building

To build the Cambria interpreter run:

```bash
cabal build
```

### Running
You can run a Cambria program using the following command:

```bash
cabal run cambria -- <path_to_file>
```
For example:

```bash
cabal run cambria -- examples/local_state.cba
```

### Testing

```bash
cabal test
```
