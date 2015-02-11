module B9.ArtifactGeneratorImpl where

import B9.ArtifactGenerator
import B9.B9Monad
import B9.B9Config
import B9.VmBuilder
import B9.ConcatableSyntax
import B9.Vm
import B9.DiskImageBuilder
import B9.ConfigUtils hiding (tell)
import B9.PropLists

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

import Test.QuickCheck

-- | Run an artifact generator to produce the artifacts.
assemble :: ArtifactGenerator -> B9 [AssembledArtifact]
assemble artGen = do
  b9cfgEnvVars <- envVars <$> getConfig
  buildId <- getBuildId
  buildDate <- getBuildDate
  let ag = parseArtifactGenerator artGen
      e = Environment
            ((buildDateKey, buildDate):(buildIdKey, buildId):b9cfgEnvVars)
            []
  case execCGParser ag e of
    Left (CGError err) ->
      error (printf "error parsing: %s: %s" (ppShow artGen)  err)
    Right igs ->
      case execIGEnv `mapM` igs of
        Left err ->
          error (printf "Failed to parse:\n%s\nError: %s"
                                   (ppShow artGen)
                                   err)
        Right is ->
          createAssembledArtifacts is

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
      mapM_ ($ mapM_ parseArtifactGenerator gs)
            (withBindings <$> allBindings)
    Each kvs gs -> do
      allBindings <- eachBindingSet g kvs
      mapM_ ($ mapM_ parseArtifactGenerator gs)
             (withBindings <$> allBindings)
    Artifact iid assembly ->
      writeInstanceGenerator iid assembly
    EmptyArtifact ->
      return ()

withArtifactSources :: [ArtifactSource] -> CGParser () -> CGParser ()
withArtifactSources srcs = local (\ce -> ce {agSources = agSources ce ++ srcs})

withBindings :: [(String,String)] -> CGParser () -> CGParser ()
withBindings bs = local (addBindings bs)

addBindings :: [(String, String)] -> Environment -> Environment
addBindings bs ce =
  let addBinding env (k,v) = nubBy ((==) `on` fst) ((k, subst env v):env)
      newEnv = foldl addBinding (agEnv ce) bs
  in ce { agEnv = newEnv }

withXBindings :: [(String,[String])] -> CGParser () -> CGParser ()
withXBindings bs cp = do
  (flip local cp) `mapM_` (addBindings <$> (allXBindings bs))
  where
    allXBindings ((k,vs):rest) = [(k,v):c | v <- vs, c <- allXBindings rest]
    allXBindings [] = [[]]

eachBindingSetT :: ArtifactGenerator
                -> [String]
                -> [[String]]
                -> CGParser [[(String,String)]]
eachBindingSetT g vars valueSets =
  if all ((== length vars) . length) valueSets
     then return (zip vars <$> valueSets)
     else (cgError (printf "Error in 'Each' binding during artifact \
                           \generation in:\n '%s'.\n\nThe variable list\n\
                           \%s\n has %i entries, but this binding set\n%s\n\n\
                           \has a different number of entries!\n"
                           (ppShow g)
                           (ppShow vars)
                           (length vars)
                           (ppShow (head (dropWhile ((== length vars) . length)
                                                    valueSets)))))

eachBindingSet :: ArtifactGenerator
                -> [(String,[String])]
                -> CGParser [[(String,String)]]
eachBindingSet g kvs = do
  checkInput
  return bindingSets
  where
    bindingSets = transpose [repeat k `zip` vs | (k, vs) <- kvs ]
    checkInput = when (1 /= (length $ nub $ length . snd <$> kvs))
                      (cgError (printf "Error in 'Each' binding: \n%s\n\
                                       \All value lists must have the same\
                                       \length!"
                                       (ppShow g)))


writeInstanceGenerator :: InstanceId -> ArtifactAssembly -> CGParser ()
writeInstanceGenerator (IID iidStrT) assembly = do
  env@(Environment bindings _) <- ask
  iid <- either (throwError . CGError) (return . IID) (substE bindings iidStrT)
  let env' = addBindings [(instanceIdKey, iidStr)] env
      IID iidStr = iid
  tell [IG iid env' assembly]

-- | Monad for creating Instance generators.
newtype CGParser a =
  CGParser { runCGParser :: WriterT [InstanceGenerator Environment]
                                   (ReaderT Environment
                                            (Either CGError))
                                   a
           }
  deriving ( Functor, Applicative, Monad
           , MonadReader Environment
           , MonadWriter [InstanceGenerator Environment]
           , MonadError CGError
           )

data Environment = Environment { agEnv :: [(String, String)]
                               , agSources :: [ArtifactSource] }
  deriving (Read, Show, Typeable, Data, Eq)

data InstanceGenerator e = IG InstanceId e ArtifactAssembly
  deriving (Read, Show, Typeable, Data, Eq)

newtype CGError = CGError String
  deriving (Read, Show, Typeable, Data, Eq, Error)

cgError :: String -> CGParser a
cgError msg = throwError (CGError msg)

execCGParser :: CGParser ()
             -> Environment
             -> Either CGError [InstanceGenerator Environment]
execCGParser = runReaderT . execWriterT . runCGParser

execIGEnv :: InstanceGenerator Environment
          -> Either String (InstanceGenerator [SourceGenerator])
execIGEnv (IG iid (Environment env sources) assembly) = do
  IG iid <$> sourceGens <*> pure (substAssembly env assembly)
  where
    sourceGens = join <$> mapM (toSourceGen env) sources

substAssembly :: [(String,String)] -> ArtifactAssembly -> ArtifactAssembly
substAssembly env p = everywhere gsubst p
  where gsubst :: forall a. Data a => a -> a
        gsubst = mkT substAssembly_
                   `extT` (substImageTarget env)
                     `extT` (substVmScript env)

        substAssembly_ (CloudInit ts f) = CloudInit ts (sub f)
        substAssembly_ vm = vm

        sub = subst env

toSourceGen :: [(String, String)]
            -> ArtifactSource
            -> Either String [SourceGenerator]
toSourceGen env src =
  case src of
    Template f -> do
      f' <- substE env f
      return [SGConcat Append env [SGFrom SGT f'] KeepPerm (takeFileName f')]
    Templates fs ->
      join <$> mapM (toSourceGen env . Template) fs
    File f -> do
      f' <- substE env f
      return [SGConcat Append env [SGFrom SGF f'] KeepPerm (takeFileName f')]
    Files fs ->
      join <$> mapM (toSourceGen env . File) fs
    Concatenation t src' -> do
      sgs <- join <$> mapM (toSourceGen env) src'
      t' <- substE env t
      let froms = join (sgGetFroms <$> sgs)
      return [SGConcat Append env froms KeepPerm t']
    YamlObjects t src' -> do
      sgs <- join <$> mapM (toSourceGen env) src'
      t' <- substE env t
      let froms = join (sgGetFroms <$> sgs)
      return [SGConcat MergeYamlObjects env froms KeepPerm t']
    ErlangTerms t src' -> do
      sgs <- join <$> mapM (toSourceGen env) src'
      t' <- substE env t
      let froms = join (sgGetFroms <$> sgs)
      return [SGConcat MergeErlangTerms env froms KeepPerm t']
    YamlLiteral t y ->

      return []
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
generateSourceTo instanceDir g@(SGConcat strat env froms p to) = do
  let toAbs = instanceDir </> to
  ensureDir toAbs
  sources <- mapM (sgReadSourceFile env) froms
  case sgConcatSources strat sources of
    Left e -> error (printf "error in %s: %s" (ppShow g) e)
    Right result -> do
      traceL (printf "rendered: \n%s\n" (T.unpack (E.decodeUtf8 result)))
      liftIO (B.writeFile toAbs result)
      sgChangePerm toAbs p

sgConcatSources :: SGConcatStrategy -> [B.ByteString] -> Either String B.ByteString
sgConcatSources strat srcs =
  case strat of
    Append ->
      let out :: Either String B.ByteString
          out = concatSources srcs
      in out >>= return . encodeSyntax
    MergeErlangTerms ->
      let out :: Either String ErlangPropList
          out = concatSources srcs
      in out >>= return . encodeSyntax
    MergeYamlObjects ->
      let out :: Either String YamlObject
          out = concatSources srcs
      in out >>= return . encodeSyntax

sgReadSourceFile :: [(String,String)] -> SGFrom -> B9 B.ByteString
sgReadSourceFile _ (SGFrom SGF f) = do
  traceL (printf "reading '%s'" f)
  liftIO (B.readFile f)
sgReadSourceFile env (SGFrom SGT f) = do
  traceL (printf "reading template '%s'" f)
  c <- liftIO (B.readFile f)
  case substEB env c of
    Left e -> error (printf "Error in '%s': %s" f e)
    Right r -> return r

sgChangePerm :: FilePath -> SGPerm -> B9 ()
sgChangePerm _ KeepPerm = return ()
sgChangePerm f (SGSetPerm (o,g,a)) = cmd (printf "chmod 0%i%i%i '%s'" o g a f)

-- | Internal data type simplifying the rather complex source generation by
--   bioling down 'ArtifactSource's to a flat list of uniform 'SourceGenerator's.
data SourceGenerator = SGConcat SGConcatStrategy [(String,String)] [SGFrom] SGPerm FilePath
  deriving (Read, Show, Typeable, Data, Eq)
data SGFrom = SGFrom SGType FilePath
  deriving (Read, Show, Typeable, Data, Eq)
data SGType = SGT | SGF
  deriving (Read, Show, Typeable, Data, Eq)
data SGPerm = SGSetPerm (Int,Int,Int) | KeepPerm
  deriving (Read, Show, Typeable, Data, Eq)

data SGConcatStrategy = Append | MergeErlangTerms | MergeYamlObjects
  deriving (Read, Show, Typeable, Data, Eq)

sgGetFroms :: SourceGenerator -> [SGFrom]
sgGetFroms (SGConcat _ _ fs _ _) = fs

setSGPerm :: Int -> Int -> Int -> SourceGenerator
          -> Either String SourceGenerator
setSGPerm o g a (SGConcat strat env from KeepPerm dest) =
  Right (SGConcat strat env from (SGSetPerm (o,g,a)) dest)
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
setSGFromDirectory fromDir (SGConcat s e fs p d) =
  SGConcat s e (setSGFrom <$> fs) p d
  where
    setSGFrom (SGFrom t f) = SGFrom t (fromDir </> f)

setSGToDirectory :: FilePath -> SourceGenerator -> SourceGenerator
setSGToDirectory toDir (SGConcat s e fs p d) =
  SGConcat s e fs p (toDir </> d)

-- | Create the actual target, either just a mountpoint, or an ISO or VFAT
-- image.
createTarget :: InstanceId -> FilePath -> ArtifactAssembly -> B9 [ArtifactTarget]
createTarget iid instanceDir (VmImages imageTargets vmScript) = do
  dbgL (printf "Creating VM-Images in '%s'" instanceDir)
  buildWithVm iid imageTargets instanceDir vmScript
  return [VmImagesTarget]
createTarget _ instanceDir (CloudInit types outPath) = do
  mapM create_ types
  where
    create_ CI_DIR = do
      let ciDir = outPath
      ensureDir (ciDir ++ "/")
      dbgL (printf "creating directory '%s'" ciDir)
      files <- getDirectoryFiles instanceDir
      traceL (printf "copying files: %s" (show files))
      liftIO (mapM_
                (\(f,t) -> do
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
      dbgL (printf "creating cloud init iso temp image '%s',\
                   \ destination file: '%s" tmpFile isoFile)
      cmd (printf "genisoimage\
                  \ -output '%s'\
                  \ -volid cidata\
                  \ -rock\
                  \ -d '%s' 2>&1"
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
      files <- (map (instanceDir </>)) <$> getDirectoryFiles instanceDir
      dbgL (printf "creating cloud init vfat image '%s'" tmpFile)
      traceL (printf "adding '%s'" (show files))
      cmd (printf "truncate --size 2M '%s'" tmpFile)
      cmd (printf "mkfs.vfat -n cidata '%s' 2>&1" tmpFile)
      cmd (intercalate " " ((printf "mcopy -oi '%s' " tmpFile)
                            : (printf "'%s'" <$> files))
           ++ " ::")
      dbgL (printf "moving vfat image '%s' to '%s'" tmpFile vfatFile)
      ensureDir vfatFile
      liftIO (copyFile tmpFile vfatFile)
      infoL (printf "CREATED CI_VFAT IMAGE: '%s'" (takeFileName vfatFile))
      return (CloudInitTarget CI_ISO vfatFile)

-- * tests

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
                     EachT _ _ gs -> gs
                     Each _ gs -> gs
                     Artifact _ _ -> []
                     EmptyArtifact -> []
      in any containsArtifactInstance nested
