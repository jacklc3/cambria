# Cambria playground

A browser code editor with Cambria highlighting, an examples
menu populated from `examples/*.cba`, and the interpreter compiled to
WebAssembly so programs run in the browser. The deployed site is
static, so nothing executes server-side.

## Running locally

```
python3 web/serve.py        # http://127.0.0.1:8642
```

Without a wasm build present, the dev server runs programs with the locally
built native interpreter instead (run `cabal build` first).

## Building the wasm interpreter

Requires the [GHC wasm toolchain](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta) in `~/.ghc-wasm`:

```
./web/wasm/build.sh                    # web/cambria.wasm + JS glue
node --no-warnings web/wasm/test.mjs   # smoke test
```

`web/wasm/Main.hs` is the entry point (cabal target `cambria-wasm`, buildable only on wasm32).
`web/vendor/` holds a vendored copy of `@bjorn3/browser_wasi_shim` 0.3.0, committed rather than fetched at runtime.
