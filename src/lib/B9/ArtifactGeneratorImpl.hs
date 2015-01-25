module B9.ArtifactGeneratorImpl (assemble) where

import B9.ArtifactGenerator
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
import Control.Monad.Error
import System.FilePath
import System.Directory
import Text.Printf
import Text.Show.Pretty (ppShow)

import Test.QuickCheck

-- | Given a subdirectory and a config generator, run the config generator to
-- produce the configuration artifacts inside that subdirectory and return a list of ready-to-use 'Artifact'
assemble :: ArtifactGenerator -> B9 [AssembledArtifact]
assemble cfgGen = do
  b9cfgEnvVars <- envVars <$> getConfig
  case execCGParser (parseArtifactGenerator cfgGen) (ArtifactEnv b9cfgEnvVars []) of
    Left (CGError err) -> error err
    Right igs -> do
      buildDir <- getBuildDir
      let outDir = buildDir </> "config-instances"
      ensureDir (outDir ++ "/")
      mapM (createInstance outDir) igs

parseArtifactGenerator :: ArtifactGenerator -> CGParser ()
parseArtifactGenerator g =
  case g of
    Sources srcs gs ->
      withArtifactSources srcs (mapM_ parseArtifactGenerator gs)
    Let bs gs ->
      withBindings bs (mapM_ parseArtifactGenerator gs)
    Each keySet valueSets gs -> do
      allBindings <- eachBindingSet g keySet valueSets
      mapM_ ($ mapM_ parseArtifactGenerator gs)
            (withBindings <$> allBindings)
    Artifact iid assemblies ->
      writeInstanceGenerator iid assemblies
    EmptyArtifact ->
      return ()

withArtifactSources :: [ArtifactSource] -> CGParser () -> CGParser ()
withArtifactSources srcs = local (\ce -> ce {ceSources = ceSources ce ++ srcs})

withBindings :: [(String,String)] -> CGParser () -> CGParser ()
withBindings bs = local (addBindings bs)
  where
   addBindings :: [(String, String)] -> ArtifactEnv -> ArtifactEnv
   addBindings newEnv ce =
     let newEnvSubst = map resolveBinding newEnv
         resolveBinding (k,v) = (k, subst oldEnv v)
         oldEnv = ceEnv ce
     in ce { ceEnv = nubBy ((==) `on` fst) (newEnvSubst ++ oldEnv)}

eachBindingSet :: ArtifactGenerator
               -> [String]
               -> [[String]]
               -> CGParser [[(String,String)]]
eachBindingSet g vars valueSets =
  if all ((== length vars) . length) valueSets
     then return (zip vars <$> valueSets)
     else (cgError (printf "Error in 'Each' binding during configuration \
                           \generation in:\n '%s'.\n\nThe variable list\n\
                           \%s\n has %i entries, but this binding set\n%s\n\n\
                           \has a different number of entries!\n"
                           (ppShow g)
                           (ppShow vars)
                           (length vars)
                           (ppShow (head (dropWhile ((== length vars) . length)
                                                    valueSets)))))

writeInstanceGenerator :: InstanceId -> [ArtifactAssembly] -> CGParser ()
writeInstanceGenerator iid assemblies = do
  env <- ask
  tell [IG iid env assemblies]

-- | Monad for creating Instance generators.
newtype CGParser a =
  CGParser { runCGParser :: WriterT [InstanceGenerator]
                                   (ReaderT ArtifactEnv
                                            (Either CGError))
                                   a
           }
  deriving ( Functor, Applicative, Monad
           , MonadReader ArtifactEnv
           , MonadWriter [InstanceGenerator]
           , MonadError CGError
           )

data ArtifactEnv = ArtifactEnv { ceEnv :: [(String, String)]
                           , ceSources :: [ArtifactSource] }
  deriving (Read, Show, Typeable, Data, Eq)

data InstanceGenerator = IG InstanceId ArtifactEnv [ArtifactAssembly]
  deriving (Read, Show, Typeable, Data, Eq)

newtype CGError = CGError String
  deriving (Read, Show, Typeable, Data, Eq, Error)

cgError :: String -> CGParser a
cgError msg = throwError (CGError msg)

execCGParser :: CGParser ()
             -> ArtifactEnv
             -> Either CGError [InstanceGenerator]
execCGParser = runReaderT . execWriterT . runCGParser


createInstance :: FilePath -> InstanceGenerator -> B9 AssembledArtifact
createInstance = undefined

--toSourceGenerator :: InstanceGenerator

-- | Internal data type simplifying the rather complex source generation by
--   bioling down 'ArtifactSource's to a flat list of uniform 'SourceGenerator's.
data SourceGenerator = SGConcat [SGFrom] SGPerm FilePath
  deriving (Read, Show, Typeable, Data, Eq)
data SGFrom = SGFrom SGType FilePath
  deriving (Read, Show, Typeable, Data, Eq)
data SGType = SGT | SGF
  deriving (Read, Show, Typeable, Data, Eq)
type SGPerm = (Int,Int,Int)

-- | Internal CEM action to generate configuration artifacts.
-- assembleCfg :: ArtifactGenerator -> CEM ()
-- assembleCfg g =
--   case g of
--     Sources srcs gs -> addArtifactSources srcs gs
--     Let bindings gs -> local (addBindings bindings) (mapM_ assembleCfg gs)
--     Each vars valueSets gs ->
--       let bindingSets = generateEachBinding vars valueSets
--           in mapM_ assembleCfg (flip Let gs <$> bindingSets)
--     ArtifactInstance (IID iidStrTemplate) assemblies -> do
--       env <- asks ceEnv
--       let iid@(IID iidStr) = IID (subst env iidStrTemplate)
--       uniqueIID@(IID uniqueIIDStr) <- generateUniqueIID iid
--       local (addBindings [(uniqueInstanceIdKey, uniqueIIDStr)
--                          ,(instanceIdKey, iidStr)])
--             (createTargets uniqueIID iid assemblies)
--   where
--     addBindings :: [(String, String)] -> ArtifactEnv -> ArtifactEnv
--     addBindings newEnv ce =
--       let newEnvSubst = map resolveBinding newEnv
--           resolveBinding (k,v) = (k, subst oldEnv v)
--           oldEnv = ceEnv ce
--       in ce { ceEnv = nubBy ((==) `on` fst) (newEnvSubst ++ oldEnv)}
--
--     generateEachBinding :: [String] -> [[String]] -> [[(String,String)]]
--     generateEachBinding vars valueSets =
--       if all ((== length vars) . length) valueSets
--          then zip vars <$> valueSets
--          else error (printf "Error in 'Each' binding during configuration \
--                             \generation in:\n '%s'.\n\nThe variable list\n\
--                             \%s\n has %i entries, but this binding set\n%s\n\n\
--                             \has a different number of entries!\n"
--                             (ppShow g)
--                             (ppShow vars)
--                             (length vars)
--                             (ppShow (head (dropWhile ((== length vars) . length)
--                                                      valueSets))))
--
--     createTargets :: InstanceId -> InstanceId -> [ArtifactAssembly] -> CEM ()
--     createTargets uniqueIID iid assemblies = do
--       instanceDir <- materializeArtifacturation uniqueIID
--       targets <- mapM (createTarget instanceDir) assemblies
--       tell [AssembledArtifact iid (join targets)]
--
-- generateUniqueIID :: InstanceId -> CEM InstanceId
-- generateUniqueIID (IID iid) = do
--   buildId <- liftB9 getBuildId
--   return (IID (printf "%s-%s" iid buildId))
--
-- addDirectory :: SystemPath -> TemplateFiles -> [ArtifactGenerator] -> CEM ()
-- addDirectory sysPath (TemplateFiles teTemplates) gs = do
--   env <- asks ceEnv
--   let tes = subst env <$> teTemplates
--   dir <- resolve (substPath env sysPath)
--   entries <- liftIO (getDirectoryContents dir)
--   fileEntries <- mapM (liftIO . doesFileExist . (dir </>)) entries
--   let files = snd <$> filter fst (fileEntries `zip` entries)
--   let (filesTe, filesNonTe) = partition (`elem` tes) files
--   when (length tes /= length filesTe)
--     (error (printf "Error in configuration generator.\n\
--                    \Not all listed 'TemplateFiles' could be found.\n\
--                    \Directory: '%s'\n\
--                    \Requested Files: %s\n\
--                    \Available Files: %s\n"
--                    dir
--                    (show tes)
--                    (show files)))
--   liftB9 (traceL (printf "Adding template files from '%s': %s"
--                          dir (show filesTe)))
--   liftB9 (traceL (printf "Adding non-template files from '%s': %s"
--                          dir (show filesNonTe)))
--   local (addTemplates (dir, filesTe) . addFiles (dir, filesNonTe))
--         (mapM_ assembleCfg gs)
--   where
--     addTemplates ts ce = ce { ceTemplateFiles = ts : ceTemplateFiles ce }
--     addFiles fs ce = ce { ceNonTemplateFiles = fs : ceNonTemplateFiles ce }
--

-- | Create a directory in the build directory for a configuration instance containing
-- all source files, with all templates processed.
-- materializeArtifacturation :: InstanceId -> CEM FilePath
-- materializeArtifacturation (IID iid) = do
--   ce <- ask
--   let instanceDir = outDir </> iid
--       ArtifactEnv sources env outDir = ce
--   liftB9 (dbgL (printf "materializing configuration '%s'" iid))
--   liftB9 (traceL (printf "configuration environment: %s" (show env)))
--   ensureDir (instanceDir ++ "/")
--   mapM_ (materializeSource instanceDir) sources
--   return instanceDir
--
-- -- | Create output file(s) in 'instanceDir' from a 'ArtifactSource'.
-- materializeSource
--

-- | Create the actual configuration target, either just a mountpoint, or an ISO
-- or VFAT image.
-- createTarget :: FilePath -> ArtifactAssembly -> CEM [ArtifactTarget]
-- createTarget configDir (MountDuringBuild mountPointTemplate) = do
--   env <- asks ceEnv
--   let mountPoint = subst env mountPointTemplate
--   liftB9 (dbgL (printf "add config mount point '%s' -> '%s'"
--                        configDir mountPointTemplate))
--   liftB9 (infoL (printf "MOUNTED CI_DIR '%s' TO '%s'"
--                         (takeFileName configDir)
--                         mountPoint))
--   return [ArtifactMount configDir (MountPoint mountPoint)]
-- createTarget configDir (CloudInit types pathTemplate) = do
--   mapM (create_ configDir pathTemplate) types
--   where
--     create_ instanceDir ciDirTemplate CI_DIR = do
--       env <- asks ceEnv
--       let ciDir = subst env ciDirTemplate
--       ensureDir (ciDir ++ "/")
--       liftB9 $ dbgL (printf "creating cloud init directory '%s'" ciDir)
--       files <- getDirectoryFiles instanceDir
--       liftB9 $ traceL (printf "copying files: %s" (show files))
--       liftIO (mapM_
--                 (uncurry copyFile)
--                 (((instanceDir </>) &&& (ciDir </>)) <$> files))
--       liftB9 (infoL (printf "CREATED CI_DIR: '%s'" (takeFileName ciDir)))
--       return (CloudInitTarget CI_DIR ciDir)
--
--     create_ instanceDir isoFileNameTemplate CI_ISO = do
--       outDir <- asks ceOutDir
--       env <- asks ceEnv
--       let isoFile = subst env isoFileNameTemplate <.> "iso"
--           tmpFile = outDir </> takeFileName isoFile
--       ensureDir tmpFile
--       liftB9 $ do
--         dbgL (printf "creating cloud init iso temp image '%s',\
--                      \ destination file: '%s" tmpFile isoFile)
--         cmd (printf "genisoimage\
--                     \ -output '%s'\
--                     \ -volid cidata\
--                     \ -rock\
--                     \ -d '%s' 2>&1"
--                     tmpFile
--                     instanceDir)
--         dbgL (printf "moving cloud init iso image '%s' to '%s'"
--                      tmpFile
--                      isoFile)
--       ensureDir isoFile
--       liftIO (copyFile tmpFile isoFile)
--       liftB9 (infoL (printf "CREATED CI_ISO IMAGE: '%s'" (takeFileName isoFile)))
--       return (CloudInitTarget CI_ISO isoFile)
--
--     create_ instanceDir vfatFileTemplate CI_VFAT = do
--       outDir <- asks ceOutDir
--       env <- asks ceEnv
--       let vfatFile = subst env vfatFileTemplate <.> "vfat"
--           tmpFile = outDir </> takeFileName vfatFile
--       ensureDir tmpFile
--       files <- (map (instanceDir </>)) <$> getDirectoryFiles instanceDir
--       liftB9 $ do
--         dbgL (printf "creating cloud init vfat image '%s'" tmpFile)
--         traceL (printf "adding '%s'" (show files))
--         cmd (printf "truncate --size 2M '%s'" tmpFile)
--         cmd (printf "mkfs.vfat -n cidata '%s' 2>&1" tmpFile)
--         cmd (intercalate " " ((printf "mcopy -oi '%s' " tmpFile)
--                               : (printf "'%s'" <$> files))
--              ++ " ::")
--         dbgL (printf "moving cloud init vfat image '%s' to '%s'" tmpFile vfatFile)
--       ensureDir vfatFile
--       liftIO (copyFile tmpFile vfatFile)
--       liftB9 (infoL (printf "CREATED CI_VFAT IMAGE: '%s'" (takeFileName vfatFile)))
--       return (CloudInitTarget CI_ISO vfatFile)

-- * tests

test_configEntriesAreOverwritten = do
  let t = Let [("x", "1")
              ,("y", "$x")]
              [Let [("x", expected)]
                   [Artifact (IID "test") [MountDuringBuild "${y}"]]]
      expected = "2"
  [AssembledArtifact _ [ArtifactMount _ (MountPoint x)]] <- assembleTest t
  when (x /= expected) (error (printf "Expected '%s' got '%s'" expected x))

test_Each = do
  let t = Each ["x", "y"]
               expected
               [Artifact (IID "$x$y") [MountDuringBuild "egal"]]
      expected = [["1", "a"], ["2", "b"]]
  [ AssembledArtifact (IID v1) [ArtifactMount _ _]
    , AssembledArtifact (IID v2) [ArtifactMount _ _]] <- assembleTest t
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
                   [Artifact (IID "test") [MountDuringBuild "$y"]]]
      expected = "1"
  [AssembledArtifact _ [ArtifactMount _ (MountPoint x)]] <- assembleTest t
  when (x /= expected) (error (printf "Expected '%s' got '%s'" expected x))

test_commandLineExtraArgsInTemplateVars = do
  let t = Artifact (IID "test") [MountDuringBuild "${arg_1}"]
      expected = "1"
      args = [("arg_1", expected)]
  [AssembledArtifact _ [ArtifactMount _ (MountPoint x)]] <- assembleTest' t args
  when (x /= expected) (error (printf "Expected '%s' got '%s'" expected x))


assembleTest t = run "test" emptyCP (mempty {verbosity = Just LogTrace})
                                    (assemble t)
assembleTest' t args = run "test" emptyCP (mempty {verbosity = Just LogTrace
                                                  ,envVars = args})
                                          (assemble t)

test_GenerateNoInstanceGeneratorsForEmptyArtifact =
  let (Right igs) = execCGParser (parseArtifactGenerator EmptyArtifact) undefined
  in igs == []

prop_GenerateNoInstanceGeneratorsForArtifactWithoutArtifactInstance =
  forAll (arbitrary `suchThat` (not . containsArtifactInstance))
         (\g -> execCGParser (parseArtifactGenerator g) undefined == Right [])
  where
    containsArtifactInstance g =
      let nested = case g of
                     Sources _ gs -> gs
                     Let _ gs -> gs
                     Each _ _ gs -> gs
                     Artifact _ _ -> []
                     EmptyArtifact -> []
      in any containsArtifactInstance nested
