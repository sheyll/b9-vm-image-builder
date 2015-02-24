#!/bin/sh

# Execute 'b9', if necessary compile it.

set -e

PATH="$(pwd)/.cabal-sandbox/bin:$PATH"

cabal install
./run.sh $@
