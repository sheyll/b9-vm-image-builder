module B9.ConfigGeneratorImpl (assemble) where

import B9.ConfigGenerator
import B9.DiskImages
import B9.B9Monad
import B9.B9Config
import B9.ConfigUtils hiding (tell)

import Data.Data
import Data.List
import Data.Function
import Control.Arrow
import Control.Exception
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import System.FilePath
import System.Directory
import Text.Printf
import Text.Show.Pretty (ppShow)

-- | Given a subdirectory and a config generator, run the config generator to
-- produce the configuration artifacts inside that subdirectory and return a list of ready-to-use 'Config'
assemble :: ConfigGenerator -> B9 [AssembledConfig]
assemble cfgGen = do
  buildDir <- getBuildDir
  b9cfgEnvVars <- envVars <$> getConfig
  let outDir = buildDir </> "config"
  ensureDir (outDir ++ "/")
  (flip runReaderT (ConfigEnv [] [] b9cfgEnvVars outDir)
   . execWriterT
   . runCEM
   . assembleCfg) cfgGen

-- | Internal CEM action to generate configuration artifacts.
assembleCfg :: ConfigGenerator -> CEM ()
assembleCfg g =
  case g of
    FromDirectory dir ts gs -> addDirectory dir ts gs
    Let bindings gs -> local (addBindings bindings) (mapM_ assembleCfg gs)
    Each vars valueSets gs ->
      let bindingSets = generateEachBinding vars valueSets
          in mapM_ assembleCfg (flip Let gs <$> bindingSets)
    ConfigInstance (IID iidStrTemplate) assemblies -> do
      env <- asks ceEnv
      let iid@(IID iidStr) = IID (subst env iidStrTemplate)
      uniqueIID@(IID uniqueIIDStr) <- generateUniqueIID iid
      local (addBindings [(uniqueInstanceIdKey, uniqueIIDStr)
                         ,(instanceIdKey, iidStr)])
            (createTargets uniqueIID iid assemblies)
  where
    addBindings :: [(String, String)] -> ConfigEnv -> ConfigEnv
    addBindings newEnv ce =
      let newEnvSubst = map resolveBinding newEnv
          resolveBinding (k,v) = (k, subst oldEnv v)
          oldEnv = ceEnv ce
      in ce { ceEnv = nubBy ((==) `on` fst) (newEnvSubst ++ oldEnv)}

    generateEachBinding :: [String] -> [[String]] -> [[(String,String)]]
    generateEachBinding vars valueSets =
      if all ((== length vars) . length) valueSets
         then zip vars <$> valueSets
         else error (printf "Error in 'Each' binding during configuration \
                            \generation in:\n '%s'.\n\nThe variable list\n\
                            \%s\n has %i entries, but this binding set\n%s\n\n\
                            \has a different number of entries!\n"
                            (ppShow g)
                            (ppShow vars)
                            (length vars)
                            (ppShow (head (dropWhile ((== length vars) . length)
                                                     valueSets))))

    createTargets :: InstanceId -> InstanceId -> [ConfigAssembly] -> CEM ()
    createTargets uniqueIID iid assemblies = do
      instanceDir <- materializeConfiguration uniqueIID
      targets <- mapM (createTarget instanceDir) assemblies
      tell [AssembledConfig iid (join targets)]

generateUniqueIID :: InstanceId -> CEM InstanceId
generateUniqueIID (IID iid) = do
  buildId <- liftB9 getBuildId
  return (IID (printf "%s-%s" iid buildId))

addDirectory :: SystemPath -> TemplateFiles -> [ConfigGenerator] -> CEM ()
addDirectory sysPath (TemplateFiles teTemplates) gs = do
  env <- asks ceEnv
  let tes = subst env <$> teTemplates
  dir <- resolve (substPath env sysPath)
  entries <- liftIO (getDirectoryContents dir)
  fileEntries <- mapM (liftIO . doesFileExist . (dir </>)) entries
  let files = snd <$> filter fst (fileEntries `zip` entries)
  let (filesTe, filesNonTe) = partition (`elem` tes) files
  when (length tes /= length filesTe)
    (error (printf "Error in configuration generator.\n\
                   \Not all listed 'TemplateFiles' could be found.\n\
                   \Directory: '%s'\n\
                   \Requested Files: %s\n\
                   \Available Files: %s\n"
                   dir
                   (show tes)
                   (show files)))
  liftB9 (traceL (printf "Adding template files from '%s': %s"
                         dir (show filesTe)))
  liftB9 (traceL (printf "Adding non-template files from '%s': %s"
                         dir (show filesNonTe)))
  local (addTemplates (dir, filesTe) . addFiles (dir, filesNonTe))
        (mapM_ assembleCfg gs)
  where
    addTemplates ts ce = ce { ceTemplateFiles = ts : ceTemplateFiles ce }
    addFiles fs ce = ce { ceNonTemplateFiles = fs : ceNonTemplateFiles ce }

-- | Create a new directory in the build directory, copy all files to that
-- directory and substitute template variables in /template files/ (aka teFiles).
materializeConfiguration :: InstanceId -> CEM FilePath
materializeConfiguration (IID iid) = do
  ce <- ask
  let instanceDir = outDir </> iid
      ConfigEnv teFiles nonTeFiles env outDir = ce
  liftB9 (dbgL (printf "materializing configuration '%s'" iid))
  liftB9 (traceL (printf "configuration environment: %s" (show env)))
  ensureDir (instanceDir ++ "/")
  mapM_ (copyTo instanceDir) [(d,f) | (d, fs) <- nonTeFiles, f <- fs]
  mapM_ (renderTemplateTo env instanceDir) [(d,f) | (d, fs) <- teFiles, f <- fs]
  liftB9 (traceL (printf "adding files: '%s'" (show (teFiles ++ nonTeFiles))))
  return instanceDir
  where
    copyTo = convertFileWith ((.) liftIO . copyFile)

    renderTemplateTo env = convertFileWith (substFile env)

    convertFileWith converter destRootDir (srcDir, srcFile) = do
      let destDir = destRootDir </> takeDirectory srcFile
      liftIO (createDirectoryIfMissing True destDir)
      converter (srcDir </> srcFile) (destDir </> takeFileName srcFile)

-- | Create the actual configuration target, either just a mountpoint, or an ISO
-- or VFAT image.
createTarget :: FilePath -> ConfigAssembly -> CEM [ConfigTarget]
createTarget configDir (MountDuringBuild mountPointTemplate) = do
  env <- asks ceEnv
  let mountPoint = subst env mountPointTemplate
  liftB9 (dbgL (printf "add config mount point '%s' -> '%s'"
                       configDir mountPointTemplate))
  liftB9 (infoL (printf "MOUNTED CI_DIR '%s' TO '%s'"
                        (takeFileName configDir)
                        mountPoint))
  return [ConfigMount configDir (MountPoint mountPoint)]
createTarget configDir (CloudInit types pathTemplate) = do
  mapM (create_ configDir pathTemplate) types
  where
    create_ instanceDir ciDirTemplate CI_DIR = do
      env <- asks ceEnv
      let ciDir = subst env ciDirTemplate
      ensureDir (ciDir ++ "/")
      liftB9 $ dbgL (printf "creating cloud init directory '%s'" ciDir)
      files <- getDirectoryFiles instanceDir
      liftB9 $ traceL (printf "copying files: %s" (show files))
      liftIO (mapM_
                (uncurry copyFile)
                (((instanceDir </>) &&& (ciDir </>)) <$> files))
      liftB9 (infoL (printf "CREATED CI_DIR: '%s'" (takeFileName ciDir)))
      return (CloudInitTarget CI_DIR ciDir)

    create_ instanceDir isoFileNameTemplate CI_ISO = do
      outDir <- asks ceOutDir
      env <- asks ceEnv
      let isoFile = subst env isoFileNameTemplate <.> "iso"
          tmpFile = outDir </> takeFileName isoFile
      ensureDir tmpFile
      liftB9 $ do
        dbgL (printf "creating cloud init iso temp image '%s',\
                     \ destination file: '%s" tmpFile isoFile)
        cmd (printf "genisoimage\
                    \ -output '%s'\
                    \ -volid cidata\
                    \ -rock\
                    \ -d '%s' 2>&1"
                    tmpFile
                    instanceDir)
        dbgL (printf "moving cloud init iso image '%s' to '%s'"
                     tmpFile
                     isoFile)
      ensureDir isoFile
      liftIO (copyFile tmpFile isoFile)
      liftB9 (infoL (printf "CREATED CI_ISO IMAGE: '%s'" (takeFileName isoFile)))
      return (CloudInitTarget CI_ISO isoFile)

    create_ instanceDir vfatFileTemplate CI_VFAT = do
      outDir <- asks ceOutDir
      env <- asks ceEnv
      let vfatFile = subst env vfatFileTemplate <.> "vfat"
          tmpFile = outDir </> takeFileName vfatFile
      ensureDir tmpFile
      files <- (map (instanceDir </>)) <$> getDirectoryFiles instanceDir
      liftB9 $ do
        dbgL (printf "creating cloud init vfat image '%s'" tmpFile)
        traceL (printf "adding '%s'" (show files))
        cmd (printf "truncate --size 2M '%s'" tmpFile)
        cmd (printf "mkfs.vfat -n cidata '%s' 2>&1" tmpFile)
        cmd (intercalate " " ((printf "mcopy -oi '%s' " tmpFile)
                              : (printf "'%s'" <$> files))
             ++ " ::")
        dbgL (printf "moving cloud init vfat image '%s' to '%s'" tmpFile vfatFile)
      ensureDir vfatFile
      liftIO (copyFile tmpFile vfatFile)
      liftB9 (infoL (printf "CREATED CI_VFAT IMAGE: '%s'" (takeFileName vfatFile)))
      return (CloudInitTarget CI_ISO vfatFile)

newtype CEM a = CEM { runCEM :: WriterT [AssembledConfig] (ReaderT ConfigEnv B9) a }
  deriving ( Functor, Applicative, Monad, MonadReader ConfigEnv
           , MonadWriter [AssembledConfig], MonadIO
           )

data ConfigEnv = ConfigEnv { ceTemplateFiles :: [(FilePath, [FilePath])]
                           , ceNonTemplateFiles :: [(FilePath, [FilePath])]
                           , ceEnv :: [(String, String)]
                           , ceOutDir :: FilePath }
  deriving (Read, Show, Typeable, Data, Eq)

liftB9 :: B9 a -> CEM a
liftB9 = CEM . lift . lift

-- * tests

test_configEntriesAreOverwritten = do
  let t = Let [("x", "1")
              ,("y", "$x")]
              [Let [("x", expected)]
                   [ConfigInstance (IID "test") [MountDuringBuild "${y}"]]]
      expected = "2"
  [AssembledConfig _ [ConfigMount _ (MountPoint x)]] <- assembleTest t
  when (x /= expected) (error (printf "Expected '%s' got '%s'" expected x))

test_Each = do
  let t = Each ["x", "y"]
               expected
               [ConfigInstance (IID "$x$y") [MountDuringBuild "egal"]]
      expected = [["1", "a"], ["2", "b"]]
  [ AssembledConfig (IID v1) [ConfigMount _ _]
    , AssembledConfig (IID v2) [ConfigMount _ _]] <- assembleTest t
  let actual = [v1, v2]
      expected' = map join expected
  when (actual /= expected') (error (printf "Expected '%s' got '%s'"
                                           (show expected')
                                           (show actual)))

test_EachToManyVars = do
  let t = Each ["x"] expected []
      expected = [["1", "a"], ["2", "b"]]
  Left (SomeException _) <- try (assembleTest t)
  return ()

test_EachEmpty = do
  let t = Each [] expected []
      expected = [[], []]
  [] <- assembleTest t
  return ()

test_EachNotEnoughVars = do
  let t = Each ["x", "y"] expected []
      expected = [["a"]]
  Left (SomeException _) <- try (assembleTest t)
  return ()

test_useTemplateVarsInTemplateVars = do
  let t = Let [("x", "1")]
              [Let [("y", "$x")]
                   [ConfigInstance (IID "test") [MountDuringBuild "$y"]]]
      expected = "1"
  [AssembledConfig _ [ConfigMount _ (MountPoint x)]] <- assembleTest t
  when (x /= expected) (error (printf "Expected '%s' got '%s'" expected x))

test_commandLineExtraArgsInTemplateVars = do
  let t = ConfigInstance (IID "test") [MountDuringBuild "${arg_1}"]
      expected = "1"
      args = [("arg_1", expected)]
  [AssembledConfig _ [ConfigMount _ (MountPoint x)]] <- assembleTest' t args
  when (x /= expected) (error (printf "Expected '%s' got '%s'" expected x))

test_canBuildExampleConfig = assembleTest exampleCfg
  where
    exampleCfg =
      FromDirectory
        (Path "examples/test-cloud-config1/")
        (TemplateFiles ["meta-data", "user-data"])
        [ Let [("domain", "te.st"),
               ("ip_prefix", "192.168.178"),
               ("ntp-server", "192.168.178.92")]
               [ Let [("host", "test-server-1.${domain}")
                     ,("ip_suffix", "13")]
                     [ConfigInstance (IID "${host}")
                                     [CloudInit [CI_ISO, CI_VFAT, CI_DIR]
                                                "EXPORT/${instance_id}-cloud-init"]]
               , Let [("host", "test-server-2.${domain}")
                     ,("ip_suffix", "14")]
                     [ConfigInstance (IID "${host}")
                                     [CloudInit [] "EXPORT/${instance_id}-cloud-init"
                                     ,MountDuringBuild "/mnt/${instance_id}"]]
              ]
         ]

assembleTest t = run "test" emptyCP (mempty {verbosity = Just LogTrace})
                                    (assemble t)
assembleTest' t args = run "test" emptyCP (mempty {verbosity = Just LogTrace
                                                  ,envVars = args})
                                          (assemble t)
