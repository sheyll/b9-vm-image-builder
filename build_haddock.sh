#!/bin/bash
#
# Create a HTML version of B9 code documentation using haddock.
#
set -xeu

cabal haddock
