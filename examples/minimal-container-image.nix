{ pkgs ? import <nixpkgs> {} }:
with pkgs;
buildEnv {
  name = "b9-example-env";
  paths = [
        bash
        coreutils
      ];
}
