#!/usr/bin/env bash
#
# Prepare B9 for release:
# * clean the workspace
# * install the dependencies
# * check that the project compiles
# * run all tests
# * build documentation
#
set -eu

stack clean
stack build
stack test
stack haddock
stack sdist

