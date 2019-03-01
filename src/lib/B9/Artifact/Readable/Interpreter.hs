{-|
Mostly effectful functions to assemble artifacts.
-}
module B9.Artifact.Readable.Interpreter(buildArtifacts, assemble, getArtifactOutputFiles) where

import           Control.Applicative            ( (<|>) )
import           Control.Arrow
import           Control.Eff                   as Eff
import           Control.Eff.Exception         as Eff
import           Control.Eff.Reader.Lazy       as Eff
import           Control.Eff.Writer.Lazy       as Eff
import           Control.Exception              ( Exception
                                                , SomeException
                                                , displayException
                                                )
import           Control.Lens                   ( view )
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Data
import           Data.Function                  ( (&) )
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import           Data.List
import           Data.Maybe
import qualified Data.Text.Lazy                as LazyT
import qualified Data.Text.Lazy.Encoding       as LazyE
import           Data.String
import           System.Directory
import           System.FilePath
import           System.IO.B9Extras             ( ensureDir
                                                , getDirectoryFiles
                                                )
import           Text.Printf
import           Text.Show.Pretty               ( ppShow )

import           B9.Artifact.Readable
import           B9.B9Config
import           B9.B9Monad
import           B9.B9Error
import           B9.Environment
import           B9.Artifact.Content
import           B9.Artifact.Content.Readable
import           B9.Artifact.Content.StringTemplate
import           B9.DiskImageBuilder
import           B9.Vm
import Control.Monad.IO.Class
import           Control.Monad
import           B9.VmBuilder

-- | Execute an 'ArtifactGenerator' and return a 'B9Invocation' that returns
-- the build id obtained by 'getBuildId'.
buildArtifacts :: ArtifactGenerator -> B9 String
buildArtifacts artifactGenerator = do
  traceL . ("CWD: " ++) =<< liftIO getCurrentDirectory
  infoL "BUILDING ARTIFACTS"
  getConfig >>= traceL . printf "USING BUILD CONFIGURATION: %v" . ppShow
  assemble artifactGenerator
  getBuildId

-- | Return a list of relative paths for the /local/ files to be generated
-- by the ArtifactGenerator. This excludes 'Shared' and Transient image targets.
getArtifactOutputFiles
  :: ArtifactGenerator
  -> Eff ArtifactEnv [FilePath]
getArtifactOutputFiles g =
  concatMap getOutputs
    <$> evalArtifactGenerator "no build-id" "no build-date" g
 where
  getOutputs (IG _ sgs a) =
    let toOutFile (AssemblyGeneratesOutputFiles fs) = fs
        toOutFile (AssemblyCopiesSourcesToDirectory pd) =
            let sourceFiles = sourceGeneratorOutputFile <$> sgs
            in  (pd </>) <$> sourceFiles
    in  getAssemblyOutput a >>= toOutFile

-- | Run an artifact generator to produce the artifacts.
assemble :: ArtifactGenerator -> B9 [AssembledArtifact]
assemble artGen = do
  b9cfgEnvVars <- view envVars <$> getConfig
  buildId      <- getBuildId
  buildDate    <- getBuildDate
  case Eff.run (evalArtifactGenerator buildId buildDate artGen) of
    Left  err -> error (displayException err)
    Right is  -> createAssembledArtifacts is

-- | Evaluate an 'ArtifactGenerator' into a list of low-level build instructions
-- that can be built with 'createAssembledArtifacts'.
evalArtifactGenerator
  :: String
  -> String
  -> ArtifactGenerator
  -> Eff ArtifactEnv [InstanceGenerator [SourceGenerator]]
evalArtifactGenerator buildId buildDate artGen =
  withSubstitutedStringBindings [(buildDateKey, buildDate), (buildIdKey, buildId)] go
   `catchB9Error`
  (throwB9Error . printf "Failed to eval:\n%s\nError: %s" (ppShow artGen) . displayException)
  where
    go = do
     ((), igs) <- runMonoidWriter $ interpretGenerator artGen
     traverse execIGEnv igs

-- | Parse an artifacto generator inside a 'ArtifactInterpreter' monad.
interpretGenerator :: ArtifactGenerator -> Eff ArtifactInterpreter ()
interpretGenerator g = case g of
  Sources srcs gs -> withArtifactSources srcs (mapM_ interpretGenerator gs)
  Let bs gs -> withSubstitutedStringBindings bs (mapM_ interpretGenerator gs)
  LetX bs gs -> withXBindings bs (mapM_ interpretGenerator gs)
  EachT keySet valueSets gs -> do
    allBindings <- eachBindingSetT g keySet valueSets
    sequence_
      (flip withSubstitutedStringBindings (mapM_ interpretGenerator gs) <$> allBindings)
  Each kvs gs -> do
    allBindings <- eachBindingSet g kvs
    sequence_ $ do
      b <- allBindings
      return (withSubstitutedStringBindings b (mapM_ interpretGenerator gs))
  Artifact iid assembly -> interpretAssembly iid assembly
  EmptyArtifact         -> return ()

-- | Execute a 'ArtifactInterpreter' action in an environment that contains a list of
-- 'ArtifactSource's.
withArtifactSources :: [ArtifactSource] -> Eff ArtifactInterpreter s -> Eff ArtifactInterpreter s
withArtifactSources srcs =  local (++ srcs)

withXBindings :: [(String, [String])] -> Eff ArtifactInterpreter () -> Eff ArtifactInterpreter ()
withXBindings bs cp =
  (`withSubstitutedStringBindings` cp) `mapM_` (allXBindings bs)
 where
  allXBindings ((k, vs) : rest) =
    [ (k, v) : c | v <- vs, c <- allXBindings rest ]
  allXBindings [] = [[]]

eachBindingSetT
  :: ArtifactGenerator
  -> [String]
  -> [[String]]
  -> Eff ArtifactInterpreter [[(String, String)]]
eachBindingSetT g vars valueSets = if all ((== length vars) . length) valueSets
  then return (zip vars <$> valueSets)
  else cgError
    (printf
      "Error in 'Each' binding during artifact generation in:\n '%s'.\n\nThe variable list\n%s\n has %i entries, but this binding set\n%s\n\nhas a different number of entries!\n"
      (ppShow g)
      (ppShow vars)
      (length vars)
      (ppShow (head (dropWhile ((== length vars) . length) valueSets)))
    )

eachBindingSet
  :: ArtifactGenerator -> [(String, [String])] -> Eff ArtifactInterpreter [[(String, String)]]
eachBindingSet g kvs = do
  checkInput
  return bindingSets
 where
  bindingSets = transpose [ repeat k `zip` vs | (k, vs) <- kvs ]
  checkInput  = when
    (1 /= length (nub $ length . snd <$> kvs))
    (cgError
      (printf
        "Error in 'Each' binding: \n%s\nAll value lists must have the same length!"
        (ppShow g)
      )
    )

interpretAssembly :: InstanceId -> ArtifactAssembly -> Eff ArtifactInterpreter ()
interpretAssembly (IID iidStrTemplate) assembly = do
  iid@(IID iidStr) <- IID <$> subst iidStrTemplate
  env <- AssemblyInput <$> askEnvironment <*> ask
  withSubstitutedStringBindings [(fromString instanceIdKey, fromString iidStr)]
    (tell [IG iid env assembly])

-- | Monad for creating Instance generators.
type ArtifactInterpreter =
      Writer [InstanceGenerator AssemblyInput]
        ': ArtifactEnv

runCGParser :: AssemblyInput -> Eff ArtifactInterpreter  a -> (Either SomeException (a, [InstanceGenerator AssemblyInput]))
runCGParser x y = runMonoidWriter y & runCGParserOnly x & Eff.run


-- | Monad for creating Instance generators.
type ArtifactEnv =
      Reader [ArtifactSource]
        ': EnvironmentReader
        ': ExcB9
        ': '[]

runCGParserOnly :: AssemblyInput -> Eff ArtifactEnv a -> Eff '[] (Either SomeException a)
runCGParserOnly x y =
    y
  & runReader (agSources x)
  & runEnvironmentReader (agEnv x)
  & runExcB9

data AssemblyInput = AssemblyInput
  { agEnv     :: Environment
  , agSources :: [ArtifactSource]
  } deriving (Show, Eq)

data InstanceGenerator e =
  IG InstanceId
     e
     ArtifactAssembly
  deriving (Read, Show, Typeable, Data, Eq)

newtype CGError =
  CGError String
  deriving (Read, Show, Typeable, Data, Eq)

instance Exception CGError

cgError :: String -> Eff ArtifactInterpreter a
cgError = undefined

execCGParser
  :: Eff ArtifactInterpreter () -> AssemblyInput -> Either SomeException [InstanceGenerator AssemblyInput]
execCGParser = undefined

execIGEnv
  :: Member ExcB9 e
  => InstanceGenerator AssemblyInput
  -> Eff e (InstanceGenerator [SourceGenerator])
execIGEnv (IG iid (AssemblyInput env sources) assembly) =
  runEnvironmentReader env $ do
    assembly' <- substAssembly assembly
    srcs <- join <$> traverse toSourceGen sources
    pure (IG iid srcs assembly')

substAssembly :: forall e . ('[ExcB9, EnvironmentReader] <:: e) => ArtifactAssembly -> Eff e ArtifactAssembly
substAssembly = everywhereM gsubst
 where
  gsubst :: Data a => a -> Eff e a
  gsubst =
    mkM substAssembly_ `extM` substImageTarget `extM` substVmScript
  substAssembly_ (CloudInit ts f) = CloudInit ts <$> subst f
  substAssembly_ vm               = pure vm


toSourceGen :: ('[ExcB9, EnvironmentReader] <:: e) => ArtifactSource -> Eff e [SourceGenerator]
toSourceGen src = do
  env <- askEnvironment
  case src of
    FromFile t (Source conv f) -> do
      t' <- subst t
      f' <- subst f
      return [SG env (SGFiles [Source conv f']) KeepPerm t']
    FromContent t c -> do
      t' <- subst t
      return [SG env (SGContent c) KeepPerm t']
    SetPermissions o g a src' -> do
      sgs <- join <$> mapM toSourceGen src'
      traverse (setSGPerm o g a) sgs
    FromDirectory fromDir src' -> do
      sgs      <- join <$> mapM toSourceGen src'
      fromDir' <- subst fromDir
      return (setSGFromDirectory fromDir' <$> sgs)
    IntoDirectory toDir src' -> do
      sgs    <- join <$> mapM toSourceGen src'
      toDir' <- subst toDir
      return (setSGToDirectory toDir' <$> sgs)

createAssembledArtifacts
  :: [InstanceGenerator [SourceGenerator]] -> B9 [AssembledArtifact]
createAssembledArtifacts igs = do
  buildDir <- getBuildDir
  let outDir = buildDir </> "artifact-instances"
  ensureDir (outDir ++ "/")
  generated <- generateSources outDir `mapM` igs
  createTargets `mapM` generated

generateSources
  :: FilePath
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
generateUniqueIID (IID iid) = IID . printf "%s-%s" iid <$> getBuildId

generateSourceTo :: FilePath -> SourceGenerator -> B9 ()
generateSourceTo instanceDir (SG env sgSource p to) = do
  let toAbs = instanceDir </> to
  ensureDir toAbs
  result <- case sgSource of
    SGFiles froms -> do
      sources <- mapM (sgReadSourceFile env) froms
      return (mconcat sources)
    SGContent c -> renderContentGenerator env c
  traceL (printf "rendered: \n%s\n" (LazyT.unpack (LazyE.decodeUtf8 result)))
  liftIO (Lazy.writeFile toAbs result)
  sgChangePerm toAbs p

sgReadSourceFile :: Environment -> SourceFile -> B9 Lazy.ByteString
sgReadSourceFile env sf =
  runContentGenerator env =<< readTemplateFile sf

sgChangePerm :: FilePath -> SGPerm -> B9 ()
sgChangePerm _ KeepPerm = return ()
sgChangePerm f (SGSetPerm (o, g, a)) =
  cmd (printf "chmod 0%i%i%i '%s'" o g a f)

-- | Internal data type simplifying the rather complex source generation by
--   bioling down 'ArtifactSource's to a flat list of uniform 'SourceGenerator's.
data SourceGenerator =
  SG Environment
     SGSource
     SGPerm
     FilePath
  deriving (Show, Eq)

-- | Return the (internal-)output file of the source file that is generated.
sourceGeneratorOutputFile :: SourceGenerator -> FilePath
sourceGeneratorOutputFile (SG _ _ _ f) = f

data SGSource
  = SGFiles [SourceFile]
  | SGContent Content
  deriving (Read, Show, Eq)

data SGPerm
  = SGSetPerm (Int, Int, Int)
  | KeepPerm
  deriving (Read, Show, Typeable, Data, Eq)

sgGetFroms :: SourceGenerator -> [SourceFile]
sgGetFroms (SG _ (SGFiles fs) _ _) = fs
sgGetFroms _                       = []

setSGPerm
  :: Member ExcB9 e => Int -> Int -> Int -> SourceGenerator -> Eff e SourceGenerator
setSGPerm o g a (SG env from KeepPerm dest) =
  pure (SG env from (SGSetPerm (o, g, a)) dest)
setSGPerm o g a sg
  | o < 0 || o > 7 = throwB9Error
    (printf "Bad 'owner' permission %i in \n%s" o (ppShow sg))
  | g < 0 || g > 7 = throwB9Error
    (printf "Bad 'group' permission %i in \n%s" g (ppShow sg))
  | a < 0 || a > 7 = throwB9Error
    (printf "Bad 'all' permission %i in \n%s" a (ppShow sg))
  | otherwise = throwB9Error
    (printf "Permission for source already defined:\n %s" (ppShow sg))

setSGFromDirectory :: FilePath -> SourceGenerator -> SourceGenerator
setSGFromDirectory fromDir (SG e (SGFiles fs) p d) = SG
  e
  (SGFiles (setSGFrom <$> fs))
  p
  d
  where setSGFrom (Source t f) = Source t (fromDir </> f)
setSGFromDirectory _fromDir sg = sg

setSGToDirectory :: FilePath -> SourceGenerator -> SourceGenerator
setSGToDirectory toDir (SG e fs p d) = SG e fs p (toDir </> d)

-- | Create the actual target, either just a mountpoint, or an ISO or VFAT
-- image.
createTarget
  :: InstanceId -> FilePath -> ArtifactAssembly -> B9 [ArtifactTarget]
createTarget iid instanceDir (VmImages imageTargets vmScript) = do
  dbgL (printf "Creating VM-Images in '%s'" instanceDir)
  success <- buildWithVm iid imageTargets instanceDir vmScript
  let err_msg      = printf "Error creating 'VmImages' for instance '%s'" iidStr
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
          copyFile f t
        )
        (((instanceDir </>) &&& (ciDir </>)) <$> files)
      )
    infoL (printf "CREATED CI_DIR: '%s'" (takeFileName ciDir))
    return (CloudInitTarget CI_DIR ciDir)
  create_ CI_ISO = do
    buildDir <- getBuildDir
    let isoFile = outPath <.> "iso"
        tmpFile = buildDir </> takeFileName isoFile
    ensureDir tmpFile
    dbgL
      (printf "creating cloud init iso temp image '%s', destination file: '%s"
              tmpFile
              isoFile
      )
    cmd
      (printf "genisoimage -output '%s' -volid cidata -rock -d '%s' 2>&1"
              tmpFile
              instanceDir
      )
    dbgL (printf "moving iso image '%s' to '%s'" tmpFile isoFile)
    ensureDir isoFile
    liftIO (copyFile tmpFile isoFile)
    infoL (printf "CREATED CI_ISO IMAGE: '%s'" (takeFileName isoFile))
    return (CloudInitTarget CI_ISO isoFile)
  create_ CI_VFAT = do
    buildDir <- getBuildDir
    let vfatFile = outPath <.> "vfat"
        tmpFile  = buildDir </> takeFileName vfatFile
    ensureDir tmpFile
    files <- map (instanceDir </>) <$> getDirectoryFiles instanceDir
    dbgL (printf "creating cloud init vfat image '%s'" tmpFile)
    traceL (printf "adding '%s'" (show files))
    cmd (printf "truncate --size 2M '%s'" tmpFile)
    cmd (printf "mkfs.vfat -n cidata '%s' 2>&1" tmpFile)
    cmd
      (  unwords (printf "mcopy -oi '%s' " tmpFile : (printf "'%s'" <$> files))
      ++ " ::"
      )
    dbgL (printf "moving vfat image '%s' to '%s'" tmpFile vfatFile)
    ensureDir vfatFile
    liftIO (copyFile tmpFile vfatFile)
    infoL (printf "CREATED CI_VFAT IMAGE: '%s'" (takeFileName vfatFile))
    return (CloudInitTarget CI_ISO vfatFile)
