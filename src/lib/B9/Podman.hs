-- | Implementation of an execution environment that uses /podman/.
module B9.Podman
  ( Podman (..),
  )
where

import B9.B9Config
  ( getB9Config,
    podmanConfigs,
  )
import B9.B9Config.Podman as X
import B9.Container
import B9.DiskImages
import B9.ShellScript
import Control.Lens (view)

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
  runInEnvironment (Podman _dcfg) _env scriptIn = do
    if emptyScript scriptIn
      then return True
      else do error "TODO"
