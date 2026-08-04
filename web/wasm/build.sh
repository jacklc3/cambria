#!/bin/bash
# Build the Cambria interpreter to WebAssembly for the playground.
#
# Prerequisites: the GHC wasm toolchain (https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)
# installed to ~/.ghc-wasm.  Native alex/happy are installed on first run.
#
# Produces web/cambria.wasm, web/ghc_wasm_jsffi.js, and web/wasm/ghc_wasm_jsffi.mjs.

set -euo pipefail
cd "$(dirname "$0")/../.."   # repo root

# 1. The cross build runs alex/happy natively, installing them if missing.
export PATH="$HOME/.local/bin:$PATH"
command -v happy >/dev/null || cabal install happy --installdir="$HOME/.local/bin" --overwrite-policy=always
command -v alex  >/dev/null || cabal install alex  --installdir="$HOME/.local/bin" --overwrite-policy=always

# 2. Cross-compile with the wasm toolchain.
if [ ! -f "$HOME/.ghc-wasm/env" ]; then
  echo "GHC wasm toolchain not found at ~/.ghc-wasm/env" >&2
  echo "Bootstrap it first: https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta" >&2
  exit 1
fi
source "$HOME/.ghc-wasm/env"
wasm32-wasi-cabal build exe:cambria-wasm

WASM=$(wasm32-wasi-cabal list-bin exe:cambria-wasm)
[ -f "$WASM" ] || { echo "cambria-wasm artifact not found at $WASM" >&2; exit 1; }

# 3. Generate the JS FFI glue module.
"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs -i "$WASM" -o web/ghc_wasm_jsffi.js
cp web/ghc_wasm_jsffi.js web/wasm/ghc_wasm_jsffi.mjs

# 4. Shrink with wasm-opt, which the toolchain supplies via binaryen.
wasm-opt -Oz "$WASM" -o web/cambria.wasm
ls -lh web/cambria.wasm web/ghc_wasm_jsffi.js
