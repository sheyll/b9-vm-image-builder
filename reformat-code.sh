#!/usr/bin/env nix-shell
#!nix-shell --pure
#!nix-shell -p haskellPackages.ormolu
#!nix-shell -i bash


find ./src -name '*.hs' -exec ormolu -o '-XBangPatterns' -m inplace '{}' \;


