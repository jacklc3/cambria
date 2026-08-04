// Web Worker that runs the Cambria interpreter compiled to WebAssembly.
// Receives {code, id}, replies {id, result} with the {ok, output} object
// runCambria returns, or {id, error}.

import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";
import { WASI, OpenFile, File, ConsoleStdout } from "./vendor/browser_wasi_shim.js";

const modulePromise = (async () => {
  const response = await fetch("./cambria.wasm");
  if (!response.ok) throw new Error(`cambria.wasm: ${response.status} ${response.statusText}`);
  return WebAssembly.compile(await response.arrayBuffer());
})();

// Fresh instance per run to refresh global counters (i.e. fresh names).
async function newInstance() {
  // args[0] must be present (the RTS reads argv), and env must not contain
  // PWD (hs_init_ghc tries to chdir into it, which fails without a filesystem).
  const wasi = new WASI(["cambria-wasm"], [], [
    new OpenFile(new File([])),                      // stdin
    ConsoleStdout.lineBuffered(() => {}),            // stdout (unused; prints are captured in Haskell)
    ConsoleStdout.lineBuffered(l => console.warn("[cambria]", l)), // stderr
  ]);
  const exports = {};
  const instance = await WebAssembly.instantiate(await modulePromise, {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghc_wasm_jsffi(exports),
  });
  Object.assign(exports, instance.exports);
  wasi.initialize(instance);
  instance.exports.hs_init(0, 0);
  return instance;
}

self.onmessage = async (e) => {
  const { code, id } = e.data;
  try {
    const instance = await newInstance();
    self.postMessage({ id, result: await instance.exports.runCambria(code) });
  } catch (err) {
    self.postMessage({ id, error: String(err) });
  }
};

// Signal readiness so the page can decide between wasm and the dev server.
modulePromise.then(
  () => self.postMessage({ ready: true }),
  (err) => self.postMessage({ ready: false, error: String(err) }),
);
