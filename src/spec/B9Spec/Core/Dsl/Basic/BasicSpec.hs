module B9Spec.Core.Dsl.Basic.BasicSpec (spec) where

import B9Spec.Prelude
import B9.Core.Dsl.Basic.Mono
import B9.Core.Dsl.Interpreter
import B9.Core.Dsl.Types.BuildStep

import Data.IORef

spec :: Spec
spec = describe "CanBuild (Mono v)" $ do it "concatenates strings" pending

testDi =
  execBuildPlan $
  do interpret $
       do t1 <- create (InitialValue "test1")
          t2 <- create (InitialValue "test2")
          t3 <- getLastArtifactValue t2
          bind t2 Append t1
          Append " 222 " `apply` t2
          Append " 333 " `apply` t1
     Just g <- dependencyGraph
     m <- use vToH
     return (printDependencyGraph g m)

newtype Ref :: (Monoid v, Typeable v) => v -> RefImpl -> Type
type RefImpl = Type -> Type

instance (Typeable v, Monoid v) => CanBuild (Ref v (impl :: RefImpl)) where
  data InitArgs (Ref v impl) = NewRef (v -> B9IO (impl v))
  data BuilderT (Ref v impl) a = RefBuilder (v -> B9IO (impl v)) (BuilderT (Mono (Last v)) a)
  type Artifact (Ref v impl) = impl v
  initialiseBuilder (NewRef mkRef) = RefBuilder (initialiseBuilder EmptyValue) mkRef
  buildArtifact (RefBuilder monoBuilder mkRef) =
    buildArtifact monoBuilder >>= mkRef

instance (CanBuild (Ref v impl)) => ApplicableTo (Ref v impl) (Append v) where
  execAction (Append v) = RefBuilder (execAction (Append v))
