#!/bin/bash
# Build the Cambria interpreter to WebAssembly for the playground.
#
# Prerequisites: the GHC wasm toolchain (https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)
# installed to ~/.ghc-wasm.  Native alex/happy are installed on first run.
#
# Produces web/cambria.wasm and web/ghc_wasm_jsffi.js.

set -euo pipefail
cd "$(dirname "$0")/../.."   # repo root

# 1. The cross build runs alex/happy natively; install them if missing.
export PATH="$HOME/.local/bin:$PATH"
command -v happy >/dev/null || cabal install happy --installdir="$HOME/.local/bin" --overwrite-policy=always
command -v alex  >/dev/null || cabal install alex  --installdir="$HOME/.local/bin" --overwrite-policy=always

# 2. Cross-compile with the wasm toolchain.
source ~/.ghc-wasm/env
wasm32-wasi-cabal build exe:cambria-wasm

WASM=$(find dist-newstyle -type f \( -name 'cambria-wasm.wasm' -o -name 'cambria-wasm' \) | grep wasm32 | head -1)
[ -n "$WASM" ] || { echo "cambria-wasm artifact not found"; exit 1; }

# 3. Generate the JS FFI glue module (plus an .mjs copy for the node test).
"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs -i "$WASM" -o web/ghc_wasm_jsffi.js
cp web/ghc_wasm_jsffi.js web/wasm/ghc_wasm_jsffi.mjs

# 4. Optionally shrink with wasm-opt, then install into web/.
if command -v wasm-opt >/dev/null; then
  wasm-opt -Oz "$WASM" -o web/cambria.wasm
else
  cp "$WASM" web/cambria.wasm
fi
ls -lh web/cambria.wasm web/ghc_wasm_jsffi.js
