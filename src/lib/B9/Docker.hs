-- | Implementation of an execution environment that uses /docker/.
module B9.Docker 
  ( Docker(..) ) where


import B9.B9Config
  (ContainerCapability,  getB9Config,
    dockerConfigs,
  )
import B9.B9Config.Docker as X
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

newtype Docker = Docker DockerConfig

instance Backend Docker where 

  getBackendConfig _ =  
      fmap Docker . view dockerConfigs <$> getB9Config
    
