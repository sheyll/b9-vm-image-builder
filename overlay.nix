self: super:
{
  b9c = self.pkgs.callPackage ./b9c.nix { };
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: 
    {
      with-utf8 = hsuper.callCabal2nix "with-utf8" ((import ./nix/sources.nix).with-utf8) {};
      b9 = import ./b9.nix  # TODO try pkgs.callPackage
        { 
          inherit (super) nix-gitignore qemu haskell
                          cdrkit docker libvirt
                          openssh rsync
                          systemd 
                          podman
                          e2fsprogs xorriso curl
                          dosfstools mtools;
          haskellPackages = hself;
        };
    };
  };
}
