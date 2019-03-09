{lib, haskellPackages, stdenv,
 makeWrapper,
 cdrkit, libvirt, sudo,
 openssh, qemu, rsync,
 e2fsprogs, xorriso, bash, curl,
 coreutils, dosfstools, mtools}:

let
  q = import <nixpkgs> {};
  cleanSrc = lib.cleanSourceWith {
    filter = (path: type:
      let base = baseNameOf (toString path);
      in !(lib.hasPrefix ".ghc.environment." base) &&
         !(lib.hasSuffix ".nix" base) &&
         !(lib.hasPrefix "BUILD_" base)
    );
    src = lib.cleanSource ./.;
  };

  b9Unwrapped = haskellPackages.callCabal2nix "b9" cleanSrc {};

  nonHaskellBuildInputs =
    [cdrkit libvirt sudo
     openssh qemu rsync
     e2fsprogs xorriso bash curl
     coreutils dosfstools mtools];

in
  stdenv.mkDerivation {
    name = "b9c";
    buildInputs = [b9Unwrapped q.pkgs.makeWrapper];
    depsHostHost = nonHaskellBuildInputs;
    phases = ["installPhase" "postFixup"];
    installPhase = ''
      mkdir -p $out/bin
      cp ${b9Unwrapped}/bin/b9c $out/bin/b9c
      '';
    # NOTE: the env var name "B9_LIBVIRT_LXC" is also specified in the Haskell code!
    postFixup = ''
      mv  $out/bin/b9c $out/bin/b9c.unwrapped
      makeWrapper \
        $out/bin/b9c.unwrapped \
        $out/bin/b9c \
        --prefix PATH : "${q.stdenv.lib.makeBinPath nonHaskellBuildInputs}:${libvirt}/libexec" \
        --set B9_LIBVIRT_LXC "${libvirt}/libexec/libvirt_lxc"
      '';
    meta = {
      homepage = b9Unwrapped.meta.homepage;
      description = b9Unwrapped.meta.description + " CLI-only version";
      license = b9Unwrapped.meta.license;
      platforms = b9Unwrapped.meta.platforms;
    };
  }
