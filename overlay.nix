self: super:
{
  b9c = self.pkgs.callPackage ./b9c.nix { };
  myHaskellPackages = super.myHaskellPackages.override {
    overrides = hself: hsuper: 
    {
      b9 = import ./b9.nix  # TODO try pkgs.callPackage
        { 
          inherit (super) nix-gitignore qemu haskell
                          cdrkit docker libvirt
                          openssh rsync
                          systemd 
                          podman
                          e2fsprogs xorriso curl
                          dosfstools mtools;
          haskellPackages = hsuper; 
        };
    };
  };
}
