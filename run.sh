#!/bin/sh

# Execute 'b9', if necessary compile it.

set -e

if [[ ! -x .cabal-sandbox/bin/b9c ]]; then
    cabal install
fi

.cabal-sandbox/bin/b9c  $@
