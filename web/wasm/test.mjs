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

// runCambria replies with an {ok, output} object
const example = await readFile(at("../../examples/pattern_matching.cba"), "utf8");
const tests = [
  ["arith", "return (1 + 1)",
   (r) => r.ok && r.output === "Pure: 2 : Int!{}"],
  ["capture", `!print "a" ; !print "b" ; return 42`,
   (r) => r.ok && r.output === "a\nb\nPure: 42 : Int!{ print : Str ~> Unit }"],
  ["fresh", "do n <- !fresh () in do m <- !fresh () in return (n == m)",
   (r) => r.ok && r.output === "Pure: False : Bool!{ fresh : Unit ~> Name }"],
  ["type error", "return (1 + true)",
   (r) => !r.ok && r.output.startsWith("Type mismatch:")],
  ["example", example,
   (r) => r.ok && r.output.startsWith("Pure:")],
];

let failed = 0;
for (const [name, code, check] of tests) {
  const out = await instance.exports.runCambria(code);
  const ok = check(out);
  console.log(ok ? "PASS" : "FAIL", name, ok ? "" : JSON.stringify(out));
  if (!ok) failed++;
}
process.exit(failed === 0 ? 0 : 1);
