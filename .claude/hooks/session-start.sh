#!/bin/bash
set -euo pipefail

# Only run inside Claude Code on the web. Locally, do nothing.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

SUDO=""
if [ "$(id -u)" -ne 0 ]; then
  SUDO="sudo"
fi

if ! command -v cabal >/dev/null 2>&1 \
   || ! command -v ghc   >/dev/null 2>&1 \
   || ! command -v alex  >/dev/null 2>&1 \
   || ! command -v happy >/dev/null 2>&1; then
  export DEBIAN_FRONTEND=noninteractive
  # Tolerate failures from unrelated third-party PPAs; required packages
  # live in Ubuntu's main archive.
  $SUDO apt-get update -qq || true
  $SUDO apt-get install -y -qq ghc cabal-install happy alex
fi

cd "${CLAUDE_PROJECT_DIR:-$(dirname "$0")/../..}"

cabal update
cabal build --only-dependencies all
