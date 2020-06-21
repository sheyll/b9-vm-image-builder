-- | Implementation of an execution environment that uses /systemdNspawn/.
module B9.SystemdNspawn
  ( SystemdNspawn (..),
  )
where

import B9.B9Config
  ( ContainerCapability,
    getB9Config,
    systemdNspawnConfigs,
  )
import B9.B9Config.SystemdNspawn as X
import B9.B9Exec
import B9.BuildInfo
import B9.Container
import B9.DiskImages
import B9.ExecEnv
import B9.ShellScript
import Control.Lens (view)
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Char (toLower)
import System.Directory
import System.FilePath
import System.IO.B9Extras
  ( UUID (),
    randomUUID,
  )
import Text.Printf (printf)

newtype SystemdNspawn = SystemdNspawn SystemdNspawnConfig

instance Backend SystemdNspawn where
  getBackendConfig _ =
    fmap SystemdNspawn . view systemdNspawnConfigs <$> getB9Config

  -- supportedImageTypes :: proxy config -> [ImageType]
  supportedImageTypes _ = [Raw]

  -- runInEnvironment ::
  --   forall e.
  --   (Member BuildInfoReader e, CommandIO e) =>
  --   config ->
  --   ExecEnv ->
  --   Script ->
  --   Eff e Bool
  runInEnvironment (SystemdNspawn dcfg) env scriptIn = do
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
