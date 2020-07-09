{ sources ? import ./sources.nix }:
with
{ 
  overlay = self: super:
  { 
    niv = import ./sources.nix {};
    haskellPackages = super.haskellPackages.override (old: {
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

