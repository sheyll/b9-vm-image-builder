-- | Programmatic Interface to b9 artifact generation.
--
-- An extensible approach to vm deployment configuration management.
--
-- TODO: do it.
--
-- @since 1.0.0
module B9.Artifact () where


---- | Build Environment
--disks = let
--  rootImg = loadSharedImage "prod-19.2" (Resize (GB 8))
--
--  dataImgWithContent =
--    let foo = "http://test.localdomain/data-foo.zip"
--        bar = "http://test.localdomain/data-bar.zip"
--        emptyImg = emptyExt4FileSystem "data" (GB 4)
--    in onFileSystem emptyImg $
--         directory "foo" $ do
--          unZipped (remoteBinary foo)
--          fileAttributes (\_f -> UnixFilePermissions 0 7 5 5 "root" "root")
--


