#!/bin/bash
#
# Prepare B9 for release:
# * clean the workspace
# * install the dependencies
# * check that the project compiles
# * run all tests
# * build documentation
#
set -eu

cabal clean
./build_and_test.sh
./build_haddock.sh
cabal check
cabal sdist

