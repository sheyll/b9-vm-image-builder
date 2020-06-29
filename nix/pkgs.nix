{ sources ? import ./sources.nix }:
with
{ 
  overlay = self: super:
  { 
    niv = import ./sources.nix {};
    myHaskellPackages = super.haskellPackages.override (old: {
      overrides = self.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: 
        {
          # ADD overrides here ...
        });
    });
  };
};
import sources.nixpkgs 
{ 
  overlays = [ overlay ] ; 
  config = {}; 
}

