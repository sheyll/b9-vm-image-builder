with (import <nixos> {});

mkShell {
  buildInputs = [(callPackage ../b9c.nix {})];

}

