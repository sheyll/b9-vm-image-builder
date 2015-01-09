#!/bin/bash

# Install the latest stackage lts snapshot and run cabal clean/update

set -e

cabal clean -v
rm -f cabal.config
wget http://www.stackage.org/lts/cabal.config
cabal update -v
cabal install --only-dependencies -j
