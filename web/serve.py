#!/usr/bin/env python3
"""Local dev server for the Cambria playground.

Serves the static playground from web/, the repo's examples/*.cba, and a
POST /run endpoint that executes programs with the locally built interpreter
under a timeout.  For deployment the /run endpoint is replaced by the
interpreter compiled to WebAssembly; the front end calls the same interface.
"""

import glob
import json
import os
import subprocess
import tempfile
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer

ROOT = os.path.dirname(os.path.abspath(__file__))   # web/
REPO = os.path.dirname(ROOT)
PORT = int(os.environ.get("PORT", "8642"))
TIMEOUT = float(os.environ.get("CAMBRIA_TIMEOUT", "5"))

BIN = os.environ.get("CAMBRIA_BIN") or next(
    iter(glob.glob(os.path.join(REPO, "dist-newstyle", "**", "x", "cambria",
                                "build", "cambria", "cambria"),
                   recursive=True)), None)


class Handler(SimpleHTTPRequestHandler):
    extensions_map = {**SimpleHTTPRequestHandler.extensions_map,
                      ".wasm": "application/wasm",
                      ".mjs": "text/javascript"}

    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=ROOT, **kwargs)

    def log_message(self, fmt, *args):
        pass

    def do_GET(self):
        if self.path == "/examples":
            names = sorted(os.path.basename(p) for p in
                           glob.glob(os.path.join(REPO, "examples", "*.cba")))
            return self.send_json({"examples": names})
        if self.path.startswith("/examples/"):
            name = os.path.basename(self.path)
            path = os.path.join(REPO, "examples", name)
            if os.path.isfile(path) and name.endswith(".cba"):
                data = open(path, "rb").read()
                self.send_response(200)
                self.send_header("Content-Type", "text/plain; charset=utf-8")
                self.send_header("Content-Length", str(len(data)))
                self.end_headers()
                self.wfile.write(data)
                return
        return super().do_GET()

    def do_POST(self):
        if self.path != "/run":
            return self.send_error(404)
        if BIN is None:
            return self.send_json({"output": "No cambria binary found; run `cabal build` first.",
                                   "timedOut": False})
        n = int(self.headers.get("Content-Length", "0"))
        code = json.loads(self.rfile.read(n)).get("code", "")
        with tempfile.NamedTemporaryFile("w", suffix=".cba", delete=False) as f:
            f.write(code)
            path = f.name
        try:
            r = subprocess.run([BIN, path], capture_output=True, text=True,
                               timeout=TIMEOUT)
            out = (r.stdout + r.stderr).strip()
            self.send_json({"output": out, "timedOut": False})
        except subprocess.TimeoutExpired:
            self.send_json({"output": f"Timed out after {TIMEOUT:g} seconds.",
                            "timedOut": True})
        finally:
            os.unlink(path)

    def send_json(self, obj):
        data = json.dumps(obj).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)


if __name__ == "__main__":
    print(f"Cambria playground: http://127.0.0.1:{PORT}  (binary: {BIN})")
    ThreadingHTTPServer(("127.0.0.1", PORT), Handler).serve_forever()
