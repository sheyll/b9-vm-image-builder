final: prev:
let
  b9cOsRuntimeDeps = with final;
    [
      libvirt
      systemd
      rsync
      docker
      podman
    ];
  b9cRuntimeDeps = with final;
    [
      cdrkit
      openssh
      qemu
      e2fsprogs
      xorriso
      bash
      curl
      coreutils
      dosfstools
      mtools
    ];
  b9-flake = final.b9-haskell-project.flake { };
  b9c-unwrapped = b9-flake.packages."b9:exe:b9c";
  b9-haskell-project =
    final.haskell-nix.cabalProject' {
      name = "b9-haskell-project";
      src = ./.;
      compiler-nix-name = "ghc8107";
      # This is used by `nix develop .` to open a shell for use with
      # `cabal`, `hlint` and `haskell-language-server`
      shell = {
        tools = {
          cabal = { };
          hlint = { };
          haskell-language-server = { };
        };
        exactDeps = true;
        NIX_SHELL_TAG = "b9";
        shellHook = ''
          export LC_ALL=en_US.UTF-8
          export LANG=en_US.UTF-8
          export LANGUAGE=en_US.UTF-8
          echo
          echo "___________________________________________________"
          echo "                                                   "
          echo "     B9 Development Environment                    "
          echo "___________________________________________________"
          echo "                                                   "
          echo "      * use 'cabal' to build this software         "
          echo "                                                   "
          echo "==================================================="
          echo
        '';
      };
      modules = [
        {
          packages.b9.components.tests.spec.build-tools = [
            b9-haskell-project.hsPkgs.hspec-discover
          ];
        }
      ];
      index-state = "2021-09-03T00:00:00Z";
      checkMaterialization = false;
      materialized = ./nix/materialization/b9;
    };

in {
  inherit b9-haskell-project b9c-unwrapped b9cOsRuntimeDeps b9cRuntimeDeps;
  b9c =
    prev.stdenvNoCC.mkDerivation {
      name = "b9c";
      buildInputs = [ prev.makeWrapper ];
      depsHostHost = b9cRuntimeDeps ++ b9cOsRuntimeDeps;
      phases = [ "buildPhase" "installPhase" ];
      buildPhase = ''
        mkdir -p $out/bin
        cp ${b9c-unwrapped}/bin/b9c $out/bin
      '';
      installPhase = ''
        wrapProgram \
          $out/bin/b9c \
          --prefix PATH : "${prev.lib.makeBinPath b9cRuntimeDeps}:" \
          --suffix PATH : "${prev.lib.makeBinPath b9cOsRuntimeDeps}:" \
          --suffix PATH : "${prev.libvirt}/libexec" \
          --set-default B9_LIBVIRT_LXC "${prev.libvirt}/libexec/libvirt_lxc"
      '';
      meta = {
        homepage = final.b9c-unwrapped.meta.homepage;
        description = final.b9c-unwrapped.meta.description + " CLI-only version";
        license = final.b9c-unwrapped.meta.license;
        platforms = final.b9c-unwrapped.meta.platforms;
      };
    };
}
