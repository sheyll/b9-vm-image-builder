{ q ? import <nixos> {  }
, compiler ? "ghc843" }:
let
  nonHaskellBuildInputs =
    with q.pkgs;
    [cdrkit libvirt sudo openssh qemu rsync e2fsprogs xorriso];


  cleanSrc = q.pkgs.lib.cleanSourceWith {
    filter = (path: type:
      let base = baseNameOf (toString path);
      in !(q.pkgs.lib.hasPrefix ".ghc.environment." base) &&
         !(q.pkgs.lib.hasSuffix ".nix" base)
    );
    src = q.pkgs.lib.cleanSource ./.;
  };

  pkg = q.pkgs.haskell.packages.${compiler}.callCabal2nix
            "b9" cleanSrc {};
in
  pkg.overrideAttrs(attrs: {
      propagatedBuildInputs =
           attrs.propagatedBuildInputs
        ++ nonHaskellBuildInputs;
  })
