# Cambria playground

A browser playground: a code editor with Cambria highlighting, an examples
menu populated from `examples/*.cba`, and the interpreter compiled to
WebAssembly so programs run in the visitor's browser. The deployed site is
static; nothing executes server-side.

## Try it locally

```
python3 web/serve.py        # http://127.0.0.1:8642
```

Without a wasm build present, the dev server runs programs with the locally
built native interpreter instead (`cabal build` first). The output pane says
which backend ran.

## Building the wasm interpreter

Requires the [GHC wasm toolchain](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)
in `~/.ghc-wasm` (their `bootstrap.sh` sets it up; ~2.4 GB):

```
./web/wasm/build.sh                    # web/cambria.wasm (~1.3 MB) + JS glue
node --no-warnings web/wasm/test.mjs   # smoke test
```

`web/wasm/MainWasm.hs` is the entry point (cabal target `cambria-wasm`,
buildable only on wasm32). Two WASI details: the instance needs an `argv[0]`,
and its environment must not contain `PWD` (the RTS tries to `chdir` there,
which fails without a filesystem).

## Layout

- `index.html`  — the front end, no build step
- `worker.js`   — Web Worker hosting the wasm interpreter, killed on timeout
- `serve.py`    — local dev server
- `wasm/`       — wasm entry point, build script, node smoke test

Deployment: static hosting with `web/` as the site root and `examples/`
alongside; a CI job runs `web/wasm/build.sh` and publishes the result.
