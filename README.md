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
* **Haskell Implementation:** The language is implemented in Haskell, with a parser, desugarer, and evaluator.

## Examples

Here are a few examples that demonstrate the expressive power of Cambria.

### Local State

This example implements a handler for local binary state, where memory cells can be dynamically created and manipulated. The state is managed by passing a function from parameters (memory locations) to their stored values.

```
-- from implementation/examples/local_state.cam
with handler {
  return x  -> return (fun _ -> return x),
  get(a; k) -> return (fun s -> k (s a) s),
  set(x; k) -> return (fun s -> k () (fun a -> if a == fst x then snd x else s a)),
  ref(x; k) -> do a <- !new () in return (fun s ->
    k a (fun b -> if b == a then return x else s b)),
  finally s -> s (fun _ -> return 0)
} handle (
  do a <- !ref 2 in
  do b <- !ref 3 in
  (!set (a, !get b + !get a); !get a)
)
-- returns 5
```

### Substitution and Jumps

This example implements a handler for a theory of substitution and jumps, which models a form of non-local control flow.

```
-- from implementation/examples/substitution.cam
with handler {
  return x  -> return (inl x),
  var(a; k) -> return (inr a),
  sub(v; k) -> do c <- !new () in
    case k (inl c) of {
      inl x -> return (inl x),
      inr d -> if d == c then k (inr ()) else return (inr d)
    }
} handle (
  case !sub () of {
    inl a -> case !sub () of {
      inl b -> !var a,
      inr x -> return 2
    },
    inr x -> return 3
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

To build the Cambria interpreter, navigate to the `implementation` directory and run:

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
cabal run cambria -- examples/local_state.cam
```

## Future Work

 - Extending type inference of current effect systems (i.e. HM type inference, bidirectional type inference, row polymorphism) to handle parameter types.
 - Understand the inbuilt `!new` operator from a categorical perspective.
