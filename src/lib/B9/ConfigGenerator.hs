module B9.ConfigGenerator
  (ConfigGenerator(..)
  ,TemplateFiles(..)
  ,InstanceId(..)
  ,ConfigTarget(..)
  ,CloudInitType(..)
  ) where


import Data.Data
import Data.List
import Data.Monoid
import Control.Applicative
import Control.Monad.IO.Class
import B9.DiskImages
import B9.ConfigUtils hiding (tell)
import Control.Monad.Reader
import Control.Monad.Writer
import System.FilePath
import System.Directory

-- | Given a subdirectory and a config generator, run the config generator to
-- produce the configuration artifacts inside that subdirectory and return a list of ready-to-use 'Config'
assemble :: FilePath -> ConfigGenerator -> IO [AssembledConfig]
assemble outDir =
  flip runReaderT (ConfigEnv [] [] [] outDir) . execWriterT . runCEM . assembleCfg

  -- | A single config generator specifies howto generate multiple output
-- files/directories. It consists of a netsted set of variable bindings that are replaced inside the text files
data ConfigGenerator = FromDirectory SystemPath TemplateFiles [ConfigGenerator]
                     | Let [(String, String)] [ConfigGenerator]
                     | ConfigInstance InstanceId  [ConfigAssembly]
                     deriving (Read, Show, Typeable, Data)

instance Monoid ConfigGenerator where
  mempty = Let [] []
  (Let [] []) `mappend` x = x
  x `mappend` (Let [] []) = x
  x `mappend` y = Let [] [x, y]

newtype TemplateFiles = TemplateFiles [FilePath]
  deriving (Read, Show, Typeable, Data)

newtype InstanceId = IID String
  deriving (Read, Show, Typeable, Data, Eq)

data ConfigTarget = ConfigImage CloudInitType FilePath
                  | ConfigMount FilePath MountPoint
  deriving (Read, Show, Typeable, Data, Eq)

data ConfigAssembly = CloudInit CloudInitType FilePath
                    | MountDuringBuild
  deriving (Read, Show, Typeable, Data, Eq)

data CloudInitType = CI_ISO | CI_VFAT
  deriving (Read, Show, Typeable, Data, Eq)


data ConfigEnv = ConfigEnv { ceTemplateFiles :: [FilePath]
                           , ceNonTemplateFiles :: [FilePath]
                           , ceEnv :: [(String, String)]
                           , ceOutDir :: FilePath }
  deriving (Read, Show, Typeable, Data, Eq)

data AssembledConfig = AssembledConfig { acDirectory :: FilePath
                                       , acIID :: InstanceId
                                       , acTargets :: [ConfigTarget]  }
  deriving (Read, Show, Typeable, Data, Eq)

newtype CEM a = CEM { runCEM :: WriterT [AssembledConfig] (ReaderT ConfigEnv IO) a }
  deriving ( Functor, Applicative, Monad, MonadReader ConfigEnv
           , MonadWriter [AssembledConfig], MonadIO )

assembleCfg :: ConfigGenerator -> CEM ()
assembleCfg g =
  case g of
    FromDirectory dir ts gs -> addDirectory dir ts gs
    Let bindings gs -> local (addBindings bindings) (mapM_ assembleCfg gs)
    ConfigInstance iid targets -> mapM_ (createTarget iid) targets
  where
    addBindings :: [(String, String)] -> ConfigEnv -> ConfigEnv
    addBindings newEnv ce = ce { ceEnv = ceEnv ce <> newEnv}

createTarget :: InstanceId -> ConfigAssembly -> CEM ()
createTarget iid@(IID iidStr) ca = do
  ce <- ask
  let instanceDir = outDir </> iidStr
      ConfigEnv teFiles nonTeFiles env outDir = ce
  tell [AssembledConfig instanceDir iid []]

addDirectory :: SystemPath -> TemplateFiles -> [ConfigGenerator] -> CEM ()
addDirectory sysPath (TemplateFiles tes) gs = do
  dir <- resolve sysPath
  files <- liftIO (getDirectoryContents dir)
  let (filesTe, filesNonTe) = partition (`elem` tes) files
  filesTeAbs <- liftIO (mapM (canonicalizePath . (dir </>)) filesTe)
  filesNonTeAbs <- liftIO (mapM (canonicalizePath . (dir </>)) filesNonTe)
  local (addTeFiles filesTeAbs . addNonTeFiles filesNonTeAbs)
        (mapM_ assembleCfg gs)
  where
    addTeFiles fs ce =
      ce { ceTemplateFiles = nub (ceTemplateFiles ce <> fs) }
    addNonTeFiles fs ce =
      ce { ceNonTemplateFiles = nub (ceNonTemplateFiles ce <> fs) }


test1 =
  FromDirectory
    (Path "/home/sven/TELECONF_AND_OCC/vm-deployment/mrfp/lb-mp1.meetyoo.de/")
    (TemplateFiles ["meta-data", "user-data"])
    [ Let [("domain", "te.st"),
          ("network", "192.168.178."),
          ("ntp-server", "192.168.178.92")]
          [ Let [("host", "test-server-1")]
                [(ConfigInstance (IID "test-server-1")
                                 [CloudInit CI_ISO "EXPORT/"])]
          , Let [("host", "test-server-1")]
                [(ConfigInstance (IID "test-server-1")
                                 [CloudInit CI_ISO "EXPORT/"])]
          ]
     ]
