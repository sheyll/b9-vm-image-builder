module B9Spec.Core.Dsl.Basic.BasicSpec (spec) where

import B9Spec.Prelude
import B9.Core.Dsl.Basic.Mono
import B9.Core.Dsl.Interpreter
import B9.Core.Dsl.Types.BuildStep
import Control.Monad.Writer.Class
import Data.IORef
import B9.Core.Util.DynMap as DynMap

spec :: Spec
spec =
  describe "CanBuild (Mono v)" $
  it "concatenates strings" $
  do (x,y,ref) <- testDi
     x `shouldBe` y
     y' <- readIORef ref
     x `shouldBe` y'

testDi :: IO (String,String,IORef String)
testDi =
  do ref1 <- newIORef ""
     ref2 <- newIORef "test3"
     ((t3,t1,gr),am) <-
       execBuildPlan $
       do (t3,t1) <-
            interpret $
            do t1 <- create (InitialValue "test1")
               t2 <- create (InitialValue "test2")
               t3 <- getLastArtifactValue t1
               t4 <- refWriter ref1
               t5 <- refReader ref2
               w <- outputFile "/tmp/xxx"
               bind t1 Append w
               bind t2 Append w
               bind t1 Append t4
               bind t5 Append t1
               bind t2 Append t1
               bind t2 Append t1
               bind t1 Append t2
               Append " 222 " `apply` t2
               Append " 333 " `apply` t1
               return (t3,t1)
          Just g <- dependencyGraph
          m <- use vToH
          return (t3,t1,printDependencyGraph g m)
     let Just a1 =
           DynMap.lookup (ArtifactsKey t1)
                         am
         Just a3 =
           do l3 <-
                DynMap.lookup (ArtifactsKey t3)
                              am
              getLast l3
     putStrLn gr
     return (a1,a3,ref1)

data OutputFile

outputFile
  :: (CanBuild (IoCall String ()))
  => FilePath -> BuildStepMonad (Handle (IoCall String ()))
outputFile = ioWriter . writeFile

fileReader
  :: (CanBuild (IoCall String ()))
  => FilePath -> BuildStepMonad (Handle (IoCall String ()))
fileReader = ioWriter . writeFile

refWriter
  :: (CanBuild (IoCall v ()))
  => IORef v -> BuildStepMonad (Handle (IoCall v ()))
refWriter = ioWriter . writeIORef

refReader
  :: (CanBuild (IoCall () v))
  => IORef v -> BuildStepMonad (Handle (IoCall () v))
refReader = ioReader . readIORef

ioWriter
  :: (CanBuild (IoCall v ()))
  => (v -> B9IO ()) -> BuildStepMonad (Handle (IoCall v ()))
ioWriter = ioCall

ioReader
  :: (CanBuild (IoCall () v))
  => B9IO v -> BuildStepMonad (Handle (IoCall () v))
ioReader = ioCall . const

ioCall
  :: (CanBuild (IoCall a r))
  => (a -> B9IO r) -> BuildStepMonad (Handle (IoCall a r))
ioCall = create . IoCall

data IoCall :: Type -> Type -> Type where

instance (Monoid v,Typeable v,Typeable r) => CanBuild (IoCall v r) where
  data InitArgs (IoCall v r) = IoCall (v -> B9IO r)
  type BuilderWriter (IoCall v r) = v
  type Artifact (IoCall v r) = r
  initialiseBuilder _ = ()
  buildArtifact (IoCall f) () w = f w

instance (CanBuild (IoCall v r)) => ApplicableTo (IoCall v r) (Append v) where
  execAction (Append v) = tell v


type RunResult = (Int, String, String)
data FileSpec = FileSpec FilePath

data B9Op a where
  NextId :: (Ord x, Eq x,Enum x, Show x, Typeable x) => (x -> a) -> B9Op a
  Run :: CanRun c => c -> (RunResult c -> a) -> B9Op a
  Pushd :: FilePath -> a -> B9Op a
  Popd :: a -> B9Op a
  WriteFile :: String -> FileSpec -> a -> B9Op a
  ImportFile :: FilePath -> FileSpec -> a -> B9Op a

data LayerInfo where
  DefineLayer :: Symbol    -> Symbol     -> LayerInfo
  (:>)        :: LayerInfo -> Dependency -> LayerInfo

data Resource where
  FileName :: Symbol -> Resource
  UserName :: Symbol -> Resource
  Layer    :: Symbol -> Resource
  IpPort   :: Nat    -> Resource
  Hostname :: Symbol -> Resource
  HttpUrl  :: Symbol -> Resource

data Dependency where
  Parameter :: Resource -> Dependency
  Reserve   :: Resource -> Dependency
  Provide   :: Resource -> Dependency
  Need      :: Resource -> Dependency

type TestLayer = DefineLayer "test" "0.3"
       :> Need      (Layer "centos-7")
       :> Parameter (UserName "app")
       :> Reserve   (UserName "app")
       :> Parameter (FileName "/app/test")
       :> Reserve   (FileName "/app/test/lib")
       :> Provide   (FileName "/app/test/plugins")
       
