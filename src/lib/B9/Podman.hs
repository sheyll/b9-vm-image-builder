-- | Implementation of an execution environment that uses /podman/.
module B9.Podman
  ( Podman (..),
  )
where

import B9.B9Config
  ( ContainerCapability,
    getB9Config,
    podmanConfigs,
  )
import B9.B9Config.Podman as X
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

newtype Podman = Podman PodmanConfig

instance Backend Podman where
  getBackendConfig _ =
    fmap Podman . view podmanConfigs <$> getB9Config

  -- supportedImageTypes :: proxy config -> [ImageType]
  supportedImageTypes _ = [Raw]

  -- runInEnvironment ::
  --   forall e.
  --   (Member BuildInfoReader e, CommandIO e) =>
  --   config ->
  --   ExecEnv ->
  --   Script ->
  --   Eff e Bool
  runInEnvironment (Podman dcfg) env scriptIn = do
    if emptyScript scriptIn
      then return True
      else do error "TODO"
