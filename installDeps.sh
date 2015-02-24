#!/bin/bash
#
# Install the latest stackage lts snapshot, run cabal clean/update and
# initialize the cabal sandbox.
#
set -e

PATH="$(pwd)/.cabal-sandbox/bin:$PATH"

cabal clean -v
cabal sandbox delete -v || echo "IGNORING"

rm -f cabal.config
wget http://www.stackage.org/lts/cabal.config

cabal update -v
cabal sandbox -v init

cabal install happy alex haddock

cabal install -v --only-dependencies --enable-tests -j
