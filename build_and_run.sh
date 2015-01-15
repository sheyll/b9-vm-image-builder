#!/bin/sh

# Execute 'b9', if necessary compile it.

set -e

cabal install
./run.sh $@
