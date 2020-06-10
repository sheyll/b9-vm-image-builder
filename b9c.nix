{lib, haskellPackages, stdenv,
 makeWrapper,
 cdrkit, docker, libvirt,
 openssh, qemu, rsync,
 nix-gitignore,
 haskell,
 e2fsprogs, xorriso, bash, curl,
 coreutils, dosfstools, mtools}:

let
  b9Unwrapped = import ./b9.nix {inherit haskellPackages nix-gitignore;}; 

  b9Static = haskell.lib.justStaticExecutables b9Unwrapped;

  nonHaskellBuildInputs =
    [cdrkit libvirt
     openssh qemu rsync docker
     e2fsprogs xorriso bash curl
     coreutils dosfstools mtools];

in
  stdenv.mkDerivation {
    name = "b9c";
    buildInputs = [b9Unwrapped makeWrapper];
    depsHostHost = nonHaskellBuildInputs;
    phases = ["installPhase" "postFixup"];
    installPhase = ''
      mkdir -p $out/bin
      cp ${b9Static}/bin/b9c $out/bin/b9c
      '';
    # NOTE: the env var name "B9_LIBVIRT_LXC" is also specified in the Haskell code!
    postFixup = ''
      mv  $out/bin/b9c $out/bin/b9c.unwrapped
      makeWrapper \
        $out/bin/b9c.unwrapped \
        $out/bin/b9c \
        --prefix PATH : "${stdenv.lib.makeBinPath nonHaskellBuildInputs}:${libvirt}/libexec" \
        --set B9_LIBVIRT_LXC "${libvirt}/libexec/libvirt_lxc"
      '';
    meta = {
      homepage = b9Unwrapped.meta.homepage;
      description = b9Unwrapped.meta.description + " CLI-only version";
      license = b9Unwrapped.meta.license;
      platforms = b9Unwrapped.meta.platforms;
    };
  }
