{
  description = "b9-vm-image-builder";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            b9-vm-image-builder =
              final.haskell-nix.cabalProject' {
                name = "b9-vm-image-builder";
                src = ./.;
                compiler-nix-name = "ghc8107";
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                  cabal = { };
                  hlint = { };
                  haskell-language-server = { };
                };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        flake = pkgs.b9-vm-image-builder.flake { };
        b9cRuntimeDeps = { nixpkgs }: with nixpkgs;
          [
            cdrkit
            libvirt
            openssh
            qemu
            rsync
            docker
            podman
            systemd
            e2fsprogs
            xorriso
            bash
            curl
            coreutils
            dosfstools
            mtools
          ];
      in
      flake // {
        lib = { inherit b9cRuntimeDeps; };
        packages = flake.packages // rec {
          b9c-unwrapped = flake.packages."b9:exe:b9c";
          b9c = pkgs.stdenvNoCC.mkDerivation {
            name = "b9c";
            buildInputs = [ pkgs.makeWrapper ];
            depsHostHost = b9cRuntimeDeps { nixpkgs = pkgs; };
            phases = [ "buildPhase" "installPhase" ];
            buildPhase = ''
              mkdir -p $out/bin
              cp ${b9c-unwrapped}/bin/b9c $out/bin
            '';
            installPhase = ''
              wrapProgram \
                $out/bin/b9c \
                --prefix PATH : "${pkgs.lib.makeBinPath (b9cRuntimeDeps {nixpkgs=pkgs;})}:${pkgs.libvirt}/libexec" \
                --set B9_LIBVIRT_LXC "${pkgs.libvirt}/libexec/libvirt_lxc"
            '';
            meta = {
              homepage = b9c-unwrapped.meta.homepage;
              description = b9c-unwrapped.meta.description + " CLI-only version";
              license = b9c-unwrapped.meta.license;
              platforms = b9c-unwrapped.meta.platforms;
            };

          };
        };
        defaultPackage = self.packages."${system}".b9c;
      });
}
