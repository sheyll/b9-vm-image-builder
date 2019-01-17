{ q ? import <nixpkgs> {  } }:
let
  nonHaskellBuildInputs =
    with q.pkgs;
    [cdrkit libvirt sudo openssh qemu rsync e2fsprogs xorriso];


  cleanSrc = q.pkgs.lib.cleanSourceWith {
    filter = (path: type:
      let base = baseNameOf (toString path);
      in !(q.pkgs.lib.hasPrefix ".ghc.environment." base) &&
         !(q.pkgs.lib.hasSuffix ".nix" base) &&
         !(q.pkgs.lib.hasPrefix "BUILD_" base)
    );
    src = q.pkgs.lib.cleanSource ./.;
  };
  hpkgs = q.pkgs.haskellPackages;
  pkg = hpkgs.callCabal2nix "b9" cleanSrc {};
in
  pkg.overrideAttrs(attrs: {
      buildInputs =
           attrs.buildInputs
        ++ nonHaskellBuildInputs;
  })
