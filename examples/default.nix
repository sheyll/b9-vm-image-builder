let 
  b9c = (import ../default.nix {}).b9c;
  pkgs = import ../nix/pkgs.nix {};
  exampleRunner = ./runExamples.sh;
in
  pkgs.writeScriptBin "exampleRunner" ''
    #!/usr/bin/env bash
    set -ex

    ${exampleRunner} ${b9c}/bin/b9c
'' 
