{haskellPackages,
 cdrkit, docker, libvirt,
 openssh, qemu, rsync,
 nix-gitignore,
 systemd, 
 podman,
 e2fsprogs, xorriso, curl,
 dosfstools, mtools,
haskell
 }:

let
  cleanSrcGitIgnore = nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  cleanSrc = cleanSrcGitIgnore ./.;

in 
  haskell.lib.addBuildTools (haskellPackages.callCabal2nix "b9" cleanSrc {}) [
    qemu cdrkit docker libvirt
    openssh qemu rsync
    systemd 
    podman
    e2fsprogs xorriso curl
    dosfstools mtools
  ]

