#!/bin/bash
#
# Install dependencies and enable test compilation, then rebuild the sources and
# run all tests.
#
set -exu

cabal install -j --enable-tests --dependencies-only
cabal configure --enable-tests --disable-optimization
cabal build
./dist/build/spec/spec
