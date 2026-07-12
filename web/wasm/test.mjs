// Node smoke test for the wasm interpreter.
// Run web/wasm/build.sh first, then:  node --no-warnings web/wasm/test.mjs
import { WASI } from "node:wasi";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.mjs";

const at = (p) => fileURLToPath(new URL(p, import.meta.url));

// args[0] must be present; env must not contain PWD (the RTS chdirs into it).
const wasi = new WASI({ version: "preview1", args: ["cambria-wasm"], env: {} });
const exports = {};
const { instance } = await WebAssembly.instantiate(await readFile(at("../cambria.wasm")), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(exports),
});
Object.assign(exports, instance.exports);
wasi.initialize(instance);
instance.exports.hs_init(0, 0);

const example = await readFile(at("../../examples/pattern_matching.cba"), "utf8");
const tests = [
  ["arith", "return (1 + 1)", "Pure: 2 : Int!{}"],
  ["print", `!print "hi" ; return 0`, "hi\nPure: 0 : Int!{ print : Str ~> Unit }"],
  ["fresh", "do n <- !fresh () in do m <- !fresh () in return (n == m)",
   "Pure: False : Bool!{ fresh : Unit ~> Name }"],
  ["example", example, null],
];

let failed = 0;
for (const [name, code, expected] of tests) {
  const out = await instance.exports.runCambria(code);
  const ok = expected === null ? out.startsWith("Pure:") : out === expected;
  console.log(ok ? "PASS" : "FAIL", name, ok ? "" : JSON.stringify(out));
  if (!ok) failed++;
}
process.exit(failed === 0 ? 0 : 1);
