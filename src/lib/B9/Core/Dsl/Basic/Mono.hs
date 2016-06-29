-- | A pure builder for 'Monoid's based on a 'Writer' monad.
module B9.Core.Dsl.Basic.Mono where

import B9.Core.Prelude
import B9.Core.Dsl.Types.BuildStep
import Control.Monad.Writer

-- | A pure-value-builder, that combines monoidal content.
data Mono (v :: Type)

-- | An action that appends to its target a pure value.
data Append v =
  Append v

-- | Builder for pure values that are monoids.
instance (Monoid v,Typeable v) => CanBuild (Mono v) where
  data InitArgs (Mono v) = InitialValue v
                       | EmptyValue
  type BuilderWriter (Mono v) = v
  type Artifact (Mono v) = v
  initialiseBuilder (InitialValue v) = tell v
  initialiseBuilder EmptyValue = pure ()
  buildArtifact = pure . execWriter . runMonoW

instance (Typeable v) => Show (InitArgs (Mono v)) where
  show _ = show (typeRep (Proxy :: Proxy v))

instance (Monoid v,CanBuild (Mono v)) => ApplicableTo (Mono v) (Append v) where
  execAction (Append v) = MonoW (tell v)

-- | Get an artifact value. (If it is a 'Monoid')
getArtifactValue
  :: (CanBuild src,v ~ Artifact src,Monoid v)
  => Handle src -> BuildStepMonad (Handle (Mono v))
getArtifactValue src =
  do val <- create EmptyValue
     bind src Append val
     return val

-- | An 'Mono' to store only the 'Last' value.
type LastValue src = Mono (Last src)

-- | Get an artifact value from any builder.
getLastArtifactValue
  :: (CanBuild src,Artifact src ~ v)
  => Handle src -> BuildStepMonad (Handle (LastValue v))
getLastArtifactValue src =
  do val <- create EmptyValue
     bind src (Append . Last . Just) val
     return val
