{ pkgs ? (import ./nix/pkgs.nix {}) }:
let 
  pkgsWithB9 = pkgs.extend (import ./overlay.nix);

  shell = pkgsWithB9.haskellPackages.shellFor {
    packages = p: [
      p."b9"
    ];
    buildInputs = with pkgsWithB9.haskellPackages; [
      cabal-install
      hlint
      brittany
      ghcid
    ];
    withHoogle = true;
  };

in
  {
    inherit shell;
    inherit (pkgsWithB9) b9c;
  }
