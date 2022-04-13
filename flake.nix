{
  description = "b9-vm-image-builder";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;
  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [ haskellNix.overlay (import ./overlay.nix) ];
        pkgs = import nixpkgs { inherit system overlays; };
        b9flake = pkgs.b9-haskell-project.flake { };
      in
      b9flake // {
        lib = {
          inherit (pkgs) b9cOsRuntimeDeps b9cRuntimeDeps;
        };
        inherit overlays;
        packages = b9flake.packages // rec {
          inherit (pkgs) b9c b9c-unwrapped;
          materializationUpdater = pkgs.runCommand "materializationUpdater"
            {
              nativeBuildInputs = [ pkgs.makeWrapper ];
            }
            ''
              mkdir -p $out/bin
              cp ${"${pkgs.b9-haskell-project.plan-nix.passthru.generateMaterialized}"} $out/bin/materializationUpdater
              wrapProgram $out/bin/materializationUpdater --add-flags nix/materialization/b9
            '';
        };
        defaultPackage = self.packages."${system}".b9c;
      });
}
