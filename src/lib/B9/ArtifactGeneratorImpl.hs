{-|
Mostly effectful functions to assemble artifacts.
-}
module B9.ArtifactGeneratorImpl where

import B9.ArtifactGenerator
import B9.B9Monad
import B9.B9Config
import B9.VmBuilder
import B9.Vm
import B9.DiskImageBuilder
import B9.ConfigUtils hiding (tell)
import B9.Content.StringTemplate
import B9.Content.Generator
import B9.Content.AST

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Data
import Data.Generics.Schemes
import Data.Generics.Aliases
import Data.List
import Data.Function
import Control.Arrow
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error
import System.FilePath
import System.Directory
import Text.Printf
import Text.Show.Pretty (ppShow)

-- | Return a list of relative paths for the /local/ files to be generated
-- by the ArtifactGenerator. This excludes 'Shared' and Transient image targets.
getArtifactOutputFiles :: ArtifactGenerator -> Either String [FilePath]
getArtifactOutputFiles g =
  concatMap getAssemblyOutputFiles
  <$> map takeAssembly
  <$> evalArtifactGenerator undefined undefined [] g
  where
    takeAssembly (IG _ _ a) = a

-- | Run an artifact generator to produce the artifacts.
assemble :: ArtifactGenerator -> B9 [AssembledArtifact]
assemble artGen = do
  b9cfgEnvVars <- envVars <$> getConfig
  buildId <- getBuildId
  buildDate <- getBuildDate
  case evalArtifactGenerator buildId buildDate b9cfgEnvVars artGen of
    Left err -> error err
    Right is -> createAssembledArtifacts is

-- | Evaluate an 'ArtifactGenerator' into a list of low-level build instructions
-- that can be built with 'createAssembledArtifacts'.
evalArtifactGenerator :: String
                      -> String
                      -> BuildVariables
                      -> ArtifactGenerator
                      -> Either String [InstanceGenerator [SourceGenerator]]
evalArtifactGenerator buildId buildDate b9cfgEnvVars artGen =
  let ag = parseArtifactGenerator artGen
      e = CGEnv ((buildDateKey, buildDate) : (buildIdKey, buildId) : b9cfgEnvVars) []
  in case execCGParser ag e of
    Left (CGError err) ->
      Left (printf "error parsing: %s: %s" (ppShow artGen) err)
    Right igs ->
      case execIGEnv `mapM` igs of
       Left err ->
         Left (printf "Failed to parse:\n%s\nError: %s" (ppShow artGen) err)
       Right is ->
         Right is

-- | Parse an artifacto generator inside a 'CGParser' monad.
parseArtifactGenerator :: ArtifactGenerator -> CGParser ()
parseArtifactGenerator g =
  case g of
    Sources srcs gs ->
      withArtifactSources srcs (mapM_ parseArtifactGenerator gs)
    Let bs gs ->
      withBindings bs (mapM_ parseArtifactGenerator gs)
    LetX bs gs ->
      withXBindings bs (mapM_ parseArtifactGenerator gs)
    EachT keySet valueSets gs -> do
      allBindings <- eachBindingSetT g keySet valueSets
      mapM_ ($ mapM_ parseArtifactGenerator gs) (withBindings <$> allBindings)
    Each kvs gs -> do
      allBindings <- eachBindingSet g kvs
      mapM_ ($ mapM_ parseArtifactGenerator gs) (withBindings <$> allBindings)
    Artifact iid assembly ->
      writeInstanceGenerator iid assembly
    EmptyArtifact ->
      return ()

-- | Execute a 'CGParser' action in an environment that contains a list of
-- 'ArtifactSource's.
withArtifactSources :: [ArtifactSource] -> CGParser () -> CGParser ()
withArtifactSources srcs = local (\ce -> ce { agSources = agSources ce ++ srcs })

withBindings :: [(String,String)] -> CGParser () -> CGParser ()
withBindings bs = local (addBindings bs)

addBindings :: [(String, String)] -> CGEnv -> CGEnv
addBindings bs ce =
  let addBinding env (k, v) = nubBy ((==) `on` fst) ((k, subst env v) : env)
      newEnv = foldl addBinding (agEnv ce) bs
  in ce { agEnv = newEnv }

withXBindings :: [(String,[String])] -> CGParser () -> CGParser ()
withXBindings bs cp = (`local` cp) `mapM_` (addBindings <$> allXBindings bs)
  where
    allXBindings ((k, vs):rest) = [(k, v) : c | v <- vs
                                              , c <- allXBindings rest]
    allXBindings [] = [[]]

eachBindingSetT :: ArtifactGenerator
                -> [String]
                -> [[String]]
                -> CGParser [[(String, String)]]
eachBindingSetT g vars valueSets =
  if all ((== length vars) . length) valueSets
    then return (zip vars <$> valueSets)
    else cgError
            (printf
               "Error in 'Each' binding during artifact generation in:\n '%s'.\n\nThe variable list\n%s\n has %i entries, but this binding set\n%s\n\nhas a different number of entries!\n"
               (ppShow g)
               (ppShow vars)
               (length vars)
               (ppShow (head (dropWhile ((== length vars) . length) valueSets))))

eachBindingSet :: ArtifactGenerator
               -> [(String, [String])]
               -> CGParser [[(String, String)]]
eachBindingSet g kvs = do
    checkInput
    return bindingSets
  where
    bindingSets = transpose [repeat k `zip` vs | (k,vs) <- kvs]
    checkInput =
        when
            (1 /= length (nub $ length . snd <$> kvs))
            (cgError
                 (printf
                      "Error in 'Each' binding: \n%s\nAll value lists must have the same length!"
                      (ppShow g)))


writeInstanceGenerator :: InstanceId -> ArtifactAssembly -> CGParser ()
writeInstanceGenerator (IID iidStrT) assembly = do
  env@(CGEnv bindings _) <- ask
  iid <- either (throwError . CGError) (return . IID) (substE bindings iidStrT)
  let env' = addBindings [(instanceIdKey, iidStr)] env
      IID iidStr = iid
  tell [IG iid env' assembly]

-- | Monad for creating Instance generators.
newtype CGParser a =
          CGParser
            { runCGParser :: WriterT [InstanceGenerator CGEnv] (ReaderT CGEnv (Either CGError)) a }
  deriving (Functor, Applicative, Monad, MonadReader CGEnv, MonadWriter [InstanceGenerator CGEnv], MonadError CGError)

data CGEnv = CGEnv { agEnv :: [(String, String)], agSources :: [ArtifactSource] }
  deriving (Read, Show, Eq)

data InstanceGenerator e = IG InstanceId e ArtifactAssembly
  deriving (Read, Show, Typeable, Data, Eq)

newtype CGError = CGError String
  deriving (Read, Show, Typeable, Data, Eq, Error)

cgError :: String -> CGParser a
cgError msg = throwError (CGError msg)

execCGParser :: CGParser ()
             -> CGEnv
             -> Either CGError [InstanceGenerator CGEnv]
execCGParser = runReaderT . execWriterT . runCGParser

execIGEnv :: InstanceGenerator CGEnv
          -> Either String (InstanceGenerator [SourceGenerator])
execIGEnv (IG iid (CGEnv env sources) assembly) = IG iid <$> sourceGens <*> pure (substAssembly env assembly)
  where
    sourceGens = join <$> mapM (toSourceGen env) sources

substAssembly :: [(String, String)] -> ArtifactAssembly -> ArtifactAssembly
substAssembly env = everywhere gsubst
  where
    gsubst :: Data a => a -> a
    gsubst = mkT substAssembly_ `extT` substImageTarget env
             `extT` substVmScript env

    substAssembly_ (CloudInit ts f) = CloudInit ts (sub f)
    substAssembly_ vm = vm

    sub = subst env

toSourceGen :: [(String, String)]
            -> ArtifactSource
            -> Either String [SourceGenerator]
toSourceGen env src =
  case src of
    FromFile t (Source conv f) -> do
      t' <- substE env t
      f' <- substE env f
      return [SGConcat env (SGFiles [Source conv f']) KeepPerm t']
    FromContent t c -> do
      t' <- substE env t
      return [SGConcat env (SGContent c) KeepPerm t']
    Concatenation t src' -> do
      sgs <- join <$> mapM (toSourceGen env) src'
      t' <- substE env t
      let froms = join (sgGetFroms <$> sgs)
      return [SGConcat env (SGFiles froms) KeepPerm t']
    SetPermissions o g a src' -> do
      sgs <- join <$> mapM (toSourceGen env) src'
      mapM (setSGPerm o g a) sgs
    FromDirectory fromDir src' -> do
      sgs <- join <$> mapM (toSourceGen env) src'
      fromDir' <- substE env fromDir
      return (setSGFromDirectory fromDir' <$> sgs)
    IntoDirectory toDir src' -> do
      sgs <- join <$> mapM (toSourceGen env) src'
      toDir' <- substE env toDir
      return (setSGToDirectory toDir' <$> sgs)

createAssembledArtifacts :: [InstanceGenerator [SourceGenerator]]
                         -> B9 [AssembledArtifact]
createAssembledArtifacts igs = do
  buildDir <- getBuildDir
  let outDir = buildDir </> "artifact-instances"
  ensureDir (outDir ++ "/")
  generated <- generateSources outDir `mapM` igs
  createTargets `mapM` generated

generateSources :: FilePath
                -> InstanceGenerator [SourceGenerator]
                -> B9 (InstanceGenerator FilePath)
generateSources outDir (IG iid sgs assembly) = do
  uiid@(IID uiidStr) <- generateUniqueIID iid
  dbgL (printf "generating sources for %s" uiidStr)
  let instanceDir = outDir </> uiidStr
  traceL (printf "generating sources for %s:\n%s\n" uiidStr (ppShow sgs))
  generateSourceTo instanceDir `mapM_` sgs
  return (IG uiid instanceDir assembly)

createTargets :: InstanceGenerator FilePath -> B9 AssembledArtifact
createTargets (IG uiid@(IID uiidStr) instanceDir assembly) = do
  targets <- createTarget uiid instanceDir assembly
  dbgL (printf "assembled artifact %s" uiidStr)
  return (AssembledArtifact uiid targets)

generateUniqueIID :: InstanceId -> B9 InstanceId
generateUniqueIID (IID iid) = do
  buildId <- getBuildId
  return (IID (printf "%s-%s" iid buildId))

generateSourceTo :: FilePath -> SourceGenerator -> B9 ()
generateSourceTo instanceDir (SGConcat env sgSource p to) = do
  let toAbs = instanceDir </> to
  ensureDir toAbs
  result <- case sgSource of
              SGFiles froms -> do
                sources <- mapM (sgReadSourceFile env) froms
                return (mconcat sources)
              SGContent c ->
                withEnvironment env (render c)
  traceL (printf "rendered: \n%s\n" (T.unpack (E.decodeUtf8 result)))
  liftIO (B.writeFile toAbs result)
  sgChangePerm toAbs p


sgReadSourceFile :: [(String, String)] -> SourceFile -> B9 B.ByteString
sgReadSourceFile env = withEnvironment env . readTemplateFile

sgChangePerm :: FilePath -> SGPerm -> B9 ()
sgChangePerm _ KeepPerm = return ()
sgChangePerm f (SGSetPerm (o, g, a)) = cmd (printf "chmod 0%i%i%i '%s'" o g a f)

-- | Internal data type simplifying the rather complex source generation by
--   bioling down 'ArtifactSource's to a flat list of uniform 'SourceGenerator's.
data SourceGenerator = SGConcat [(String, String)] SGSource SGPerm FilePath
  deriving (Read, Show, Eq)

data SGSource = SGFiles [SourceFile]
              | SGContent Content
  deriving (Read, Show, Eq)

data SGType = SGT
            | SGF
  deriving (Read, Show, Typeable, Data, Eq)

data SGPerm = SGSetPerm (Int, Int, Int)
            | KeepPerm
  deriving (Read, Show, Typeable, Data, Eq)


sgGetFroms :: SourceGenerator -> [SourceFile]
sgGetFroms (SGConcat _ (SGFiles fs) _ _) = fs
sgGetFroms _ = []

setSGPerm :: Int
          -> Int
          -> Int
          -> SourceGenerator
          -> Either String SourceGenerator
setSGPerm o g a (SGConcat env from KeepPerm dest) =
  Right (SGConcat env from (SGSetPerm (o, g, a)) dest)
setSGPerm o g a sg
  | o < 0 || o > 7 =
      Left (printf "Bad 'owner' permission %i in \n%s" o (ppShow sg))
  | g < 0 || g > 7 =
      Left (printf "Bad 'group' permission %i in \n%s" g (ppShow sg))
  | a < 0 || a > 7 =
      Left (printf "Bad 'all' permission %i in \n%s" a (ppShow sg))
  | otherwise =
      Left (printf "Permission for source already defined:\n %s" (ppShow sg))

setSGFromDirectory :: FilePath -> SourceGenerator -> SourceGenerator
setSGFromDirectory fromDir (SGConcat e (SGFiles fs) p d) =
  SGConcat e (SGFiles (setSGFrom <$> fs)) p d
  where
    setSGFrom (Source t f) = Source t (fromDir </> f)
setSGFromDirectory _fromDir sg = sg

setSGToDirectory :: FilePath -> SourceGenerator -> SourceGenerator
setSGToDirectory toDir (SGConcat e fs p d) =
  SGConcat e fs p (toDir </> d)

-- | Create the actual target, either just a mountpoint, or an ISO or VFAT
-- image.
createTarget :: InstanceId -> FilePath -> ArtifactAssembly -> B9 [ArtifactTarget]
createTarget iid instanceDir (VmImages imageTargets vmScript) = do
  dbgL (printf "Creating VM-Images in '%s'" instanceDir)
  success <- buildWithVm iid imageTargets instanceDir vmScript
  let err_msg = printf "Error creating 'VmImages' for instance '%s'" iidStr
      (IID iidStr) = iid
  unless success (errorL err_msg >> error err_msg)
  return [VmImagesTarget]
createTarget _ instanceDir (CloudInit types outPath) = mapM create_ types
  where
    create_ CI_DIR = do
      let ciDir = outPath
      ensureDir (ciDir ++ "/")
      dbgL (printf "creating directory '%s'" ciDir)
      files <- getDirectoryFiles instanceDir
      traceL (printf "copying files: %s" (show files))
      liftIO
        (mapM_
           (\(f, t) -> do
              ensureDir t
              copyFile f t)
           (((instanceDir </>) &&& (ciDir </>)) <$> files))
      infoL (printf "CREATED CI_DIR: '%s'" (takeFileName ciDir))
      return (CloudInitTarget CI_DIR ciDir)

    create_ CI_ISO = do
      buildDir <- getBuildDir
      let isoFile = outPath <.> "iso"
          tmpFile = buildDir </> takeFileName isoFile
      ensureDir tmpFile
      dbgL (printf "creating cloud init iso temp image '%s', destination file: '%s" tmpFile isoFile)
      cmd
        (printf
           "genisoimage -output '%s' -volid cidata -rock -d '%s' 2>&1"
           tmpFile
           instanceDir)
      dbgL (printf "moving iso image '%s' to '%s'" tmpFile isoFile)
      ensureDir isoFile
      liftIO (copyFile tmpFile isoFile)
      infoL (printf "CREATED CI_ISO IMAGE: '%s'" (takeFileName isoFile))
      return (CloudInitTarget CI_ISO isoFile)

    create_ CI_VFAT = do
      buildDir <- getBuildDir
      let vfatFile = outPath <.> "vfat"
          tmpFile = buildDir </> takeFileName vfatFile
      ensureDir tmpFile
      files <- map (instanceDir </>) <$> getDirectoryFiles instanceDir
      dbgL (printf "creating cloud init vfat image '%s'" tmpFile)
      traceL (printf "adding '%s'" (show files))
      cmd (printf "truncate --size 2M '%s'" tmpFile)
      cmd (printf "mkfs.vfat -n cidata '%s' 2>&1" tmpFile)
      cmd
        (unwords (printf "mcopy -oi '%s' " tmpFile : (printf "'%s'" <$> files))
         ++ " ::")
      dbgL (printf "moving vfat image '%s' to '%s'" tmpFile vfatFile)
      ensureDir vfatFile
      liftIO (copyFile tmpFile vfatFile)
      infoL (printf "CREATED CI_VFAT IMAGE: '%s'" (takeFileName vfatFile))
      return (CloudInitTarget CI_ISO vfatFile)
