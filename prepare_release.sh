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

PATH="$(pwd)/.cabal-sandbox/bin:$PATH"

./installDeps.sh
./build_and_test.sh
./build_haddock.sh
cabal check
