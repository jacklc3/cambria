# Cambria

Functional programming language with parameterized algebraic effects and handlers, implemented in Haskell. Research project exploring the "problem of instances" — how to create multiple independent instances of an effect at runtime using Parameterized Algebraic Theories (PATs).

## Build & Run

```bash
cd implementation
cabal build              # Build the interpreter
cabal run cambria -- <file.cba>   # Run a .cba program
cabal run cambria -- examples/local_state.cba  # Example
```

**Prerequisites:** GHC, Cabal, Alex (lexer generator), Happy (parser generator)

## Project Structure

```
implementation/
  cambria.cabal              # Project config (Haskell2010, Cabal)
  src/
    Main.hs                  # Entry point, built-in IO effects (new, print, read, flip, bernoulli, uniform)
    Syntax.hs                # Core AST (desugared)
    Environment.hs           # Runtime environment
    Eval.hs                  # Evaluator (call-by-value, effect handlers)
    Parsing/
      Lexer.x                # Alex lexer specification
      Parser.y               # Happy parser specification
      Token.hs               # Token types
      SugaredSyntax.hs       # Sugared AST (pre-desugaring)
      Desugar.hs             # Desugaring pass
    Inference/
      Types.hs               # Type system types (types, effects, parameter types)
      Infer.hs               # Type & effect inference (extended Hindley-Milner)
      Initialisation.hs      # Inference state initialisation
      Substitutable.hs       # Type substitution class
      Unify.hs               # Unification algorithm
  examples/                  # Example .cba programs
main.tex                     # LaTeX thesis/paper
library.bib                  # Bibliography
```

## Pipeline

Source `.cba` file → **Lexer** (Alex) → **Parser** (Happy) → Sugared AST → **Desugar** → Core AST → **Type Inference** → **Eval**

## Key Concepts

- **Values vs Computations:** Fine-grained call-by-value separates inert values from effectful computations
- **Effects:** Operations like `!get`, `!set`, `!ref` are effect invocations; `!new` is a built-in for creating fresh resource identifiers
- **Handlers:** `with handler { return x -> ..., op(p; k) -> ..., finally f -> ... } handle (...)` pattern
- **Parameters:** Operations are parameterized by resource identifiers, distinguishing operations that *use* resources from those that *bind* new ones
- **Built-in IO effects:** `new`, `print`, `read`, `flip`, `bernoulli`, `uniform` (handled in Main.hs)

## Dependencies

base, containers, text, random, mtl, array
