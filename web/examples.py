#!/usr/bin/env python3
"""Stage the repo's examples/*.cba into web/examples/ for the playground."""

import glob
import json
import os
import shutil

ROOT = os.path.dirname(os.path.abspath(__file__))   # web/
REPO = os.path.dirname(ROOT)
DEST = os.path.join(ROOT, "examples")


def sync():
    """Copy the examples into web/examples/, write index.json, return the names."""
    sources = sorted(glob.glob(os.path.join(REPO, "examples", "*.cba")))
    names = [os.path.basename(p) for p in sources]
    os.makedirs(DEST, exist_ok=True)
    for stale in glob.glob(os.path.join(DEST, "*.cba")):
        if os.path.basename(stale) not in names:
            os.remove(stale)
    for src in sources:
        shutil.copyfile(src, os.path.join(DEST, os.path.basename(src)))
    with open(os.path.join(DEST, "index.json"), "w") as f:
        json.dump({"examples": names}, f, indent=2)
        f.write("\n")
    return names


if __name__ == "__main__":
    staged = sync()
    print(f"Staged {len(staged)} examples in {DEST}")
