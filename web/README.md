# Cambria playground

A browser playground for Cambria: a Monaco editor with Cambria syntax
highlighting, an examples menu populated from `examples/*.cba`, and the
interpreter itself compiled to WebAssembly so programs run entirely in the
visitor's browser. No server-side execution; the deployed site is static.

## Try it locally

```
python3 web/serve.py        # http://127.0.0.1:8642
```

Without a wasm build present, the dev server falls back to running programs
with the locally built native interpreter (`cabal build` first). The output
pane shows which backend ran (`wasm` or `server`).

## Building the wasm interpreter

Requires the [GHC wasm toolchain](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)
in `~/.ghc-wasm` (their `bootstrap.sh` sets it up; ~2.4 GB):

```
./web/wasm/build.sh          # produces web/cambria.wasm (~1.4 MB) + JS glue
node --no-warnings web/wasm/test.mjs   # smoke test
```

`web/wasm/MainWasm.hs` is the wasm entry point (cabal target `cambria-wasm`,
buildable only on wasm32). It reuses the pipeline unchanged, except that
`!print` output is accumulated and returned and `!read` is unavailable.

Two WASI initialisation details, learned the hard way: the instance must be
given an `argv[0]`, and its environment must not contain `PWD` (the RTS
tries to `chdir` there, which fails without a filesystem).

## Layout

- `index.html`  — the whole front end (Monaco from CDN, no build step)
- `worker.js`   — Web Worker hosting the wasm interpreter, killed on timeout
- `serve.py`    — local dev server (static files + native-run fallback)
- `wasm/`       — wasm entry point, build script, node smoke test

Deployment target: static hosting (e.g. GitHub Pages) with `web/` as the
site root and `examples/` alongside; a CI job runs `web/wasm/build.sh` and
publishes the result.
