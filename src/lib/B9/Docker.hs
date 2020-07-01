-- | Implementation of an execution environment that uses /docker/.
module B9.Docker
  ( Docker (..),
  )
where

import B9.B9Config
  (
    dockerConfigs,
    getB9Config,
  )
import B9.B9Config.Docker as X
import B9.Container
import B9.DiskImages
import B9.ShellScript
import Control.Lens (view)

newtype Docker = Docker DockerConfig

instance Backend Docker where
  getBackendConfig _ =
    fmap Docker . view dockerConfigs <$> getB9Config

  -- supportedImageTypes :: proxy config -> [ImageType]
  supportedImageTypes _ = [Raw]

  -- runInEnvironment ::
  --   forall e.
  --   (Member BuildInfoReader e, CommandIO e) =>
  --   config ->
  --   ExecEnv ->
  --   Script ->
  --   Eff e Bool
  runInEnvironment (Docker _dcfg) _env scriptIn = do
    if emptyScript scriptIn
      then return True
      else do error "TODO"
-- where
--       setUp = do
--         buildId <- getBuildId
--         buildBaseDir <- getBuildDir
--         uuid <- randomUUID
--         let scriptDirHost = buildDir </> "init-script"
--             scriptDirGuest = "/" ++ buildId
--             domainFile = buildBaseDir </> uuid' <.> domainConfig
--             mkDomain =
--               createDomain cfgIn env buildId uuid' scriptDirHost scriptDirGuest
--             uuid' = printf "%U" uuid
--             setupEnv =
--               Begin
--                 [ Run "export" ["HOME=/root"],
--                   Run "export" ["USER=root"],
--                   Run "source" ["/etc/profile"]
--                 ]
--             script = Begin [setupEnv, scriptIn, successMarkerCmd scriptDirGuest]
--             buildDir = buildBaseDir </> uuid'
--         liftIO $ do
--           createDirectoryIfMissing True scriptDirHost
--           writeSh (scriptDirHost </> initScript) script
--           domain <- mkDomain
--           writeFile domainFile domain
--         return $ Context scriptDirHost uuid domainFile cfgIn
