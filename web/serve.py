#!/usr/bin/env python3
"""Local dev server for the Cambria playground.

Serves the static playground from web/, staging the repo's examples into
web/examples/ and the syntax reference into web/reference.pdf at startup, plus
a post /run endpoint that executes programs with the locally built interpreter
under a timeout.  The front end prefers the interpreter compiled to
WebAssembly and only falls back to /run when the wasm build is unavailable.
"""

import glob
import json
import os
import shutil
import subprocess
import tempfile
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer

import examples

ROOT = os.path.dirname(os.path.abspath(__file__))   # web/
REPO = os.path.dirname(ROOT)
PORT = int(os.environ.get("PORT", "8642"))
TIMEOUT = float(os.environ.get("CAMBRIA_TIMEOUT", "5"))
LATEX_TIMEOUT = 120
MAX_BODY = 1 << 20

BIN = os.environ.get("CAMBRIA_BIN") or next(
    iter(glob.glob(os.path.join(REPO, "dist-newstyle", "**", "x", "cambria",
                                "build", "cambria", "cambria"),
                   recursive=True)), None)


def reference():
    """Build reference/cambria.tex, stage it as web/reference.pdf, and return it."""
    src = os.path.join(REPO, "reference", "cambria.tex")
    pdf = os.path.join(REPO, "reference", "cambria.pdf")
    try:
        r = subprocess.run(["latexmk", "-pdf", "-cd", "-interaction=nonstopmode",
                            "-halt-on-error", src],
                           capture_output=True, timeout=LATEX_TIMEOUT)
        if r.returncode:
            print("latexmk failed; see reference/cambria.log")
    except FileNotFoundError:
        print("No latexmk found; not building the syntax reference")
    except subprocess.TimeoutExpired:
        print(f"latexmk timed out after {LATEX_TIMEOUT:g} seconds")
    if not os.path.exists(pdf):
        return None
    dest = os.path.join(ROOT, "reference.pdf")
    shutil.copyfile(pdf, dest)
    return dest


class Handler(SimpleHTTPRequestHandler):
    extensions_map = {**SimpleHTTPRequestHandler.extensions_map,
                      ".wasm": "application/wasm",
                      ".cba": "text/plain"}

    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=ROOT, **kwargs)

    def log_message(self, fmt, *args):
        pass

    def do_POST(self):
        if self.path != "/run":
            return self.send_error(404)
        if BIN is None:
            return self.send_run("No cambria binary found; run `cabal build` first.")
        code = self.read_code()
        if code is None:
            return self.send_run('Expected a JSON body with a string "code" field.')
        path = None
        try:
            with tempfile.NamedTemporaryFile("w", suffix=".cba", delete=False) as f:
                path = f.name
                f.write(code)
            r = subprocess.run([BIN, path], capture_output=True, text=True,
                               timeout=TIMEOUT)
            self.send_run((r.stdout + r.stderr).strip(), r.returncode == 0)
        except subprocess.TimeoutExpired:
            self.send_run(f"Timed out after {TIMEOUT:g} seconds.")
        except OSError as e:
            self.send_run(f"Could not run the program: {e}")
        finally:
            if path is not None and os.path.exists(path):
                os.unlink(path)

    def read_code(self):
        """The program text from the request body, or None if it is malformed."""
        try:
            n = int(self.headers.get("Content-Length", ""))
            if not 0 <= n <= MAX_BODY:
                return None
            code = json.loads(self.rfile.read(n))["code"]
        except (KeyError, TypeError, ValueError):
            return None
        return code if isinstance(code, str) else None

    def send_run(self, output, ok=False):
        data = json.dumps({"ok": ok, "output": output}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)


if __name__ == "__main__":
    staged = examples.sync()
    ref = reference()
    print(f"Cambria playground: http://127.0.0.1:{PORT}  "
          f"(binary: {BIN}, {len(staged)} examples, "
          f"reference: {'yes' if ref else 'no'})")
    ThreadingHTTPServer(("127.0.0.1", PORT), Handler).serve_forever()
