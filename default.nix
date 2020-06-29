{ pkgs ? (import ./nix/pkgs.nix {}) }:
let 
  pkgsWithB9 = pkgs.extend (import ./overlay.nix);

  shell = pkgsWithB9.myHaskellPackages.shellFor {
    packages = p: [
      p.b9
    ];
    buildInputs = with pkgsWithB9.myHaskellPackages; [
      cabal-install
      hlint
      ghcid
      pkgsWithB9.docker
      niv
      ormolu
      ghcide 
    ];
    withHoogle = true;
  };

in
  {
    inherit shell;
    inherit (pkgsWithB9) b9c;
  }
