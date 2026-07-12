// Web Worker that runs the Cambria interpreter compiled to WebAssembly.
// Receives {code}, replies {output} or {error}.  The main page terminates
// and respawns this worker if a program runs past the timeout.

import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";
import { WASI, OpenFile, File, ConsoleStdout } from "https://esm.sh/@bjorn3/browser_wasi_shim@0.3.0";

const instancePromise = (async () => {
  // args[0] must be present (the RTS reads argv), and env must not contain
  // PWD (hs_init_ghc tries to chdir into it, which fails without a filesystem).
  const wasi = new WASI(["cambria-wasm"], [], [
    new OpenFile(new File([])),                      // stdin
    ConsoleStdout.lineBuffered(() => {}),            // stdout (unused; prints are captured in Haskell)
    ConsoleStdout.lineBuffered(l => console.warn("[cambria]", l)), // stderr
  ]);
  const exports = {};
  const response = await fetch("./cambria.wasm");
  const bytes = await response.arrayBuffer();
  const { instance } = await WebAssembly.instantiate(bytes, {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghc_wasm_jsffi(exports),
  });
  Object.assign(exports, instance.exports);
  wasi.initialize(instance);
  instance.exports.hs_init(0, 0);
  return instance;
})();

self.onmessage = async (e) => {
  try {
    const instance = await instancePromise;
    const output = await instance.exports.runCambria(e.data.code);
    self.postMessage({ output });
  } catch (err) {
    self.postMessage({ error: String(err) });
  }
};

// Signal readiness so the page can decide between wasm and the dev server.
instancePromise.then(
  () => self.postMessage({ ready: true }),
  (err) => self.postMessage({ ready: false, error: String(err) }),
);
