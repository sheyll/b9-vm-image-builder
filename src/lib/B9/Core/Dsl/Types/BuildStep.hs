{-# LANGUAGE UndecidableInstances #-}

-- | The internal core definitions for the DSL of B9.
-- At the core is the 'BuildStep' GADT that specifies the basic build steps.
--
-- The build steps are wrapped into a free monad called 'BuildPlanT', to
-- decoupled declaration and interpretation of buildplans.
--
-- This means that a plan that consists of these steps:
--
-- > do
-- >    foo <- create EmptyContent
-- >    bar <- create EmptyContent
-- >    bind foo Append bar
-- >    apply bar Append "pure-value"
--
-- Might be executed
module B9.Core.Dsl.Types.BuildStep
       (module B9.Core.Dsl.Types.BuildStep, module ReExport) where

import B9.Core.Prelude
import B9.Core.Dsl.Types.Handle as ReExport
import B9.Core.System.B9IO as ReExport
import Control.Monad.RWS

-- | A single step in a large build plan. The indiviual steps are restricted to
-- types from the type families below. These steps form a very basic,
-- declerative language, to describe artifacts and data dependencies between
-- artifacts. Meaning is created by the type class instances of 'CanBuild' and
-- 'ApplicableTo', and the respective interpreters.
data BuildStep next where
        Create ::
            (Typeable a, CanBuild a) =>
            InitArgs a -> (Handle a -> next) -> BuildStep next
        Apply ::
            (Typeable a, ApplicableTo a action) =>
            action -> Handle a -> next -> BuildStep next
        Bind ::
            (CanBuild src, CanBuild dst, Artifact src ~ srcOut, Typeable dst,
             ApplicableTo dst dstIn) =>
            Handle src ->
              (srcOut -> dstIn) -> Handle dst -> next -> BuildStep next

-- | A 'BuildStepMonad' is a 'Free' monad of 'BuildStep's.
type BuildStepMonad = Free BuildStep

-- | The requirement to create a free monad of a type is a functor instance.
instance Functor BuildStep where
  fmap f (Create src k) = Create src (f . k)
  fmap f (Apply action a next) = Apply action a (f next)
  fmap f (Bind a aToAction b next) = Bind a aToAction b (f next)

-- | Create a builder and return the handle representing the build output.
create :: CanBuild a
       => InitArgs a -> BuildStepMonad (Handle a)
create src = liftF $ Create src id

-- | Apply an artifact using an /action/ and an action specific (pure) value.
apply :: (ApplicableTo a action)
      => action -> Handle a -> BuildStepMonad ()
apply action handle = liftF $ Apply action handle ()

-- | Bind the 'Artifact' referred to by the second 'Handle' into the artifact
-- identified by the first handle using a given action. This is similar to
-- 'apply', but use the 'Artifact' value of an artifact identified by a
-- 'Handle' instead of using a pure value.
bind
  :: (CanBuild src,CanBuild dst,Artifact src ~ t,ApplicableTo dst action)
  => Handle src -> (t -> action) -> Handle dst -> BuildStepMonad ()
bind src toAction dst = liftF $ Bind src toAction dst ()

-- * Artifact Creation
-- | Type class for builders producing artifacts as defined by the 'InitArgs'.
-- Builders have input, state and output, and are controlled by a build-life
-- cycle. An artifact may represent a real world artifact, like e.g. a file or
-- an OS process running on the host, which can only be created or modified
-- using effectfull computations.
--
-- An artifact can of course be a pure piece of data.
-- This makes sense in order to sequence the real world effects of other
-- artifacts. For example, to read contents from source files, then combine and
-- transform  that content in some way and write it to multiple output files,
-- there would be a pure artifact _in the middle_ gathereing the content from
-- the inputs, and, as soon as all inputs are provided, providing it to the
-- output files.
--
-- It is an explicit design goal to be able to interleave artifact generation,
-- such that modifications done to an artifact can be done depending on two
-- parameters, one could e.g. be passed in the free monad (BuildStep) and the
-- is attained only later when e.g. the actual contents have been built.
--
-- This is an example for usage of the types in a manner allowing this:
--
-- > do
-- >    outHandle <- initialiseBuilder (OutputFile "dest")
-- >    srcHandle <- initialiseBuilder (OutputFile "source")
-- >    docHandle <- (initialiseBuilder (Document EmptyYamlObject)
-- >    applyToArtifact outHandle docHandle $
-- >      \doc -> execAction (WriteFile . renderYaml) doc
-- >    applyToArtifact docHandle srcHandle $
-- >      \out -> execAction (AppendDocument . parseYaml) out
--
-- This can be simplified with a helper function:
-- > modifyWithInput buildHandle modification inputHandle =
-- >   applyToArtifact buildHandle inputHandle $ execAction modification
--
-- So the above could be rewritten as:
-- > outHandle <- allocateHandle (initialiseBuilder (OutputFile "dest"))
-- > srcHandle <- allocateHandle (initialiseBuilder (OutputFile "source"))
-- > docHandle <- allocateHandle (initialiseBuilder (Document EmptyYamlObject))
-- > modifyWithInput outHandle docHandle (WriteFile . renderYaml)
-- > modifyWithInput docHandle srcHandle (AppendDocument . parseYaml)
--
-- The lexicographical order of modifications is now different from the
-- execution sequence, given appropriate (in any case 'possible')
-- implementations of allocateHandle and modifyWithInput.
--
-- Another point is reordering of build actions to enable parallel execution of
-- independent sub tasks.
--
-- The fact that I want this thing to be a monad for syntactic reasons, as well
-- as the need for re-ordering of the 'BuildStep' /AST/ as one might put it,
-- calls for the use of 'Free'.
--
-- The free monad is run by an interpreter that reorders all the build steps.
-- Since the 'BuildStep' is existential, typeclasses are used by the default
-- interpreter.
--
-- An artifact is build in a two step process:
--
-- > initialiseBuilder -> (modification ->)* buildArtifact -> (extraction ->)* GC
-- > \_________________  _________________/\________________  ________________/
-- >                   \/                                   \/
-- >              Build  Phase                      Extraction Phase
--
-- The artifact in the build phase is modified by appending/composing values
-- wrapped inside a functor. This functor could for example be a state
-- monad, a reader, a writer, or maybe just 'IdentityT'.
-- After the build phase was ended using 'buildArtifact' it is possible
-- to extract content out of the artifact.
--
-- After build-phase it is assumed that the artifact has consumed all
-- modifications and has assumed it's final state, where it is ready to be
-- used as input to other artifact builds. The output of an artifact build
-- is not necessarily effectful, it isn't required for an 'ArtifactOutput'
-- to be a functor/monad/applicative or even a monoid.
class (Typeable k,Typeable a,Typeable (Artifact a),Typeable (BuilderState a),Monoid (BuilderWriter a),Typeable (BuilderWriter a)) => CanBuild (a :: k)  where
  -- | Contruction parameters required to create the artifact.
  -- You may wonder why on earth do I need a data family, why not just
  -- use 'a' directly?
  -- That would force a to have kind '*' which rules out uninhabited types.
  -- The recipe for using this is to define a phantom data type and an
  -- 'InitArgs' instance constructor with the same identifier, e.g.:
  --
  -- >
  -- > data OutFile c
  -- > instance (Put c) => CanBuild (OutFile c) where
  -- >    data InitArgs (OutFile c) = OutFile FilePath
  -- >
  -- > outFile :: (Put c) => FilePath -> BuildStepMonad (Handle (OutFile c))
  -- > outFile f = create (OutFile f)
  -- >
  --
  -- The 'InitArgs' of an artifact describes the static parameters  i.e.
  -- those that identify an artifacts manifestation in the real world. In case
  -- of file artifacts this could simply be the actual path to the file.
  -- In case of a VM-Image, it could be the actual file paths of the images,
  -- the image formats, partition table and so forth.
  data InitArgs a
  -- | The functor that maintains the build state during the build phase. This
  -- could be a state monad.
  type BuilderState a
  type (BuilderState a) = ()
  -- | The intermediate outputs during the build-phase.
  type BuilderWriter a
  type (BuilderWriter a) = ()
  -- | The output/result of the build-phase.
  type Artifact a
  -- | Create/Initialise the artifact from using the given parameters and return
  -- an action that contains the initial 'BuilderState'
  initialiseBuilder
    :: InitArgs a -> BuilderState a
  -- | Run the 'BuilderRWST' of an artifact in order to create the desired effect
  -- in 'B9IO' that e.g. copies files, converts vm-images, launches containers,
  -- etc.
  buildArtifact :: InitArgs a
                -> BuilderState a
                -> BuilderWriter a
                -> B9IO (Artifact a)

-- | Artifacts can be modified during the build phase. Instances define *how* an
-- @action@ is applied to the artifact builder @a@.
class (CanBuild a) => ApplicableTo a action  where
  execAction :: Monad m => action -> BuilderRWST m a ()

type BuilderRWST m a b = RWST (InitArgs a) (BuilderWriter a) (BuilderState a) m b

runBuilderRWST
  :: BuilderRWST m a b
  -> InitArgs a
  -> BuilderState a
  -> m (b,BuilderState a,BuilderWriter a)
runBuilderRWST = runRWST

type BuilderRWS a b = BuilderRWST Identity a b

runBuilderRWS
  :: BuilderRWS a b
  -> InitArgs a
  -> BuilderState a
  -> (b,BuilderState a,BuilderWriter a)
runBuilderRWS m i s = runIdentity (runBuilderRWST m i s)
