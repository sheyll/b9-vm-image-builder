#!/bin/sh

# Execute 'b9' directly from the sources without using cabal or the need to
# compile it.

set -e

runhaskell -isrc/lib:src/cli src/cli/Main.hs $@
