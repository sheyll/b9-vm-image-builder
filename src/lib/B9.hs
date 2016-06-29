{-| B9 is a library and build tool with primitive operations to run a
    build script inside a virtual machine and to create and convert
    virtual machine image files as well as related ISO and VFAT disk images
    for e.g. cloud-init configuration sources.

    This module re-exports the modules needed to build a tool around the
    library, e.g. see @src\/cli\/Main.hs@ as an example.

    "B9.ArtifactGenerator" is the module containing the basic data structure
    used to describe a B9 build.

-}

module B9 (b9_version, module X) where

import B9.Core.Prelude as X
import Paths_b9 (version) 

-- | Return the cabal package version of the B9 library.
b9_version :: Version
b9_version = version
