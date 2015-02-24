#!/bin/sh

# Execute 'b9', if necessary compile it.

set -e

PATH="$(pwd)/.cabal-sandbox/bin:$PATH"

if [[ ! -x .cabal-sandbox/bin/b9c ]]; then
    cabal install
fi

.cabal-sandbox/bin/b9c  $@
