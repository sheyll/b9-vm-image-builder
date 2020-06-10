{pkgs ? (import ./nix/pkgs.nix {}) }:
(import ./default.nix {inherit pkgs;}).shell
