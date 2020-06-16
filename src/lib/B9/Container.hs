-- | An interface for container backends such as libvirt-lxc or docker 
module B9.Container ( Backend (..) ) where 

import B9.B9Logging
import B9.BuildInfo
import B9.DiskImages
import B9.ExecEnv
import B9.ShellScript
import Control.Eff

-- | Class of backends that run a 'Script' in an 'ExecEnv' in an OS-level 
-- container like docker or lxc.
class Backend config where 
  -- | Return 'Nothing' if the configuration **disables** this container backend,
  -- and return 'Just ...' if the configuration **enables** this container backend.
  getBackendConfig :: 
    forall proxy e. (Member BuildInfoReader e, CommandIO e) => 
    proxy config -> Eff e (Maybe config)
  -- | The input images, that a given container accepts 
  supportedImageTypes :: proxy config -> [ImageType]
  supportedImageTypes _ = [Raw]
  -- | Run a 'Script' in an 'ExecEnv', and return 'True' if the script 
  -- completed successfully.
  runInEnvironment :: 
    forall e. (Member BuildInfoReader e, CommandIO e) => 
    config -> ExecEnv -> Script -> Eff e Bool


