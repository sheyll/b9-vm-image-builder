-- | The internal core definitions for the DSL of B9.
-- At the core is the 'BuildStep' GADT that specifies the basic build steps.
--
-- The build steps are wrapped into a free monad called 'BuildPlanT', to
-- decoupled declaration and interpretation of buildplans.
--
-- This means that a plan that consists of these steps:
--
-- > do
-- >    foo <- createFrom EmptyContent
-- >    bar <- createFrom EmptyContent
-- >    merge foo Append bar
-- >    modifyWith bar Append "pure-value"
--
-- Might be executed
module B9.Dsl.Core (module B9.Dsl.Core, module X) where

import B9.Dsl.Handle as X
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.Writer

-- | A program is a free monad of 'BuildStep's over a type constructor @m@.
type BuildPlanT m = Free (BuildStep m)

-- | A single step in a large build plan. The indiviual steps are restricted
-- to types from the type families below.
data BuildStep m next where
        Create ::
            HasBuilder m a =>
            InitArgs m a -> (Handle a -> next) -> BuildStep m next
        Modify ::
            SupportsAction action m a =>
            Handle a -> Action action m a t -> t -> next -> BuildStep m next
        Merge ::
            (HasBuilder m src, Artifact m src ~ t,
             SupportsAction action m dst) =>
            Handle dst ->
              Action action m dst t -> Handle src -> next -> BuildStep m next

-- | The requirement to create a free monad of a type is a functor instance.
instance Functor (BuildStep m) where
  fmap f (Create src k) = Create src (f . k)
  fmap f (Modify hnd action pureArg next) = Modify hnd action pureArg (f next)
  fmap f (Merge hnd action hndArg next) = Merge hnd action hndArg (f next)

-- | Create an artifact.
createFrom
  :: HasBuilder m a
  => InitArgs m a -> BuildPlanT m (Handle a)
createFrom src = liftF $ Create src id

-- | Modify an artifact using an /action/ and an action specific (pure) value.
modifyWith
  :: SupportsAction action m a
  => Handle a -> Action action m a t -> t -> BuildPlanT m ()
modifyWith handle action argument = liftF $ Modify handle action argument ()

-- | Merge the 'Artifact' referred to by the second 'Handle' into the artifact
-- identified by the first handle using a given action. This is similar to
-- 'modifyWith', but use the 'Artifact' value of an artifact identified by a
-- 'Handle' instead of using a pure value.
merge
  :: (HasBuilder m src,Artifact m src ~ t,SupportsAction action m dst)
  => Handle dst -> Action action m dst t -> Handle src -> BuildPlanT m ()
merge handle action argument = liftF $ Merge handle action argument ()

-- * Artifact Creation
-- | Realworld artifacts that are built by a builder and in a build-life cycle.
-- An artifact may represent a real world artifact, like e.g. a file or a OS
-- process running on the host, which can only be created or modified using
-- effectfull computations.
--
-- An artifact can of course be a pure peice of data.
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
-- > outHandle <- allocateHandle (initialiseBuilder (OutputFile "dest"))
-- > srcHandle <- allocateHandle (initialiseBuilder (OutputFile "source"))
-- > docHandle <- allocateHandle (initialiseBuilder (Document EmptyYamlObject))
-- > applyToArtifact outHandle docHandle $
-- >   \doc -> buildAction (WriteFile . renderYaml) doc
-- > applyToArtifact docHandle srcHandle $
-- >   \out -> buildAction (AppendDocument . parseYaml) out
--
-- This can be simplified with a helper function:
-- > modifyWithInput buildHandle modification inputHandle =
-- >   applyToArtifact buildHandle inputHandle $ buildAction modification
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
-- Using the BuildStep free monad this can further be simplified to:
--
class (Show (InitArgs m a),Typeable (BuilderT m a),Typeable (Artifact m a),Functor (BuilderT m a),Applicative (BuilderT m a)) => HasBuilder m (a :: k)  where
  -- | Contruction parameters required to create the artifact.
  -- You may wonder why on earth do I need a data family, why not just
  -- use 'a' directly?
  -- That would force a to have kind '*' which rules out type level strings.
  -- The recipe for using this is to define a phantom data type and a
  -- 'InitArgs' instance constructor with the same identifier, e.g.:
  --
  -- >
  -- > data Foo
  -- > instance HasBuilder m Foo where
  -- >    data InitArgs m Foo = Foo
  --
  -- The 'InitArgs' of an artifact describes the static parameters  i.e.
  -- those that identify an artifacts manifestation in the real world. In case
  -- of file artifacts this could simply be the actual path to the file.
  -- In case of a VM-Image, it could be the actual file paths of the images,
  -- the image formats, partition table and so forth.
  data InitArgs m a
  -- | An artifact may have this life cycle:
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
  data BuilderT m a :: * -> *
  -- | Create/Initialise the artifact from using the given parameters and
  -- return an artifact 'BuilderT'.
  initialiseBuilder
    :: InitArgs m a -> BuilderT m a ()
  -- | Run the 'BuilderT' of an artifact in order to create the desired effect
  -- in 'm' that e.g. copies files, converts vm-images, launches missles, etc.
  buildArtifact
    :: BuilderT m a () -> m (Artifact m a)
  -- | After build-phase it is assumed that the artifact has consumed all
  -- modifications and has assumed it's final state, where it is ready to be
  -- used as input to other artifact builds. The output of an artifact build
  -- is not necessarily effectful, it isn't required for an 'ArtifactOutput'
  -- to be a functor/monad/applicative or even a monoid.
  type Artifact m a

-- * Build Actions
-- | Append values to a an artifact.
data Append

-- | Artifacts can be modified during the build phase using a build action. Each
-- individual modification-type will modify the artifact in a different way.
class (HasBuilder m a) => SupportsAction (action :: k) m a  where
  -- | The (injective) type indicating how an artifact is modified. It is a data
  -- family  rather than a type family since this type should really uniquely
  -- identify the underlying artifact in question. If it were to be used by
  -- several different artifact types, they should really be merged and the
  -- distinction should be placed into the content type. Since a modification
  -- might restrict/depend on the type of the modification parameter.
  -- Note however, that in the BuildActionArgument has to be GADT like in order to
  -- have different buildAction implementations for different parameter types.
  --
  -- For Example:
  --
  -- > instance SupportsAction Add m FooBuilder where
  -- >   data Action Add m FooBuilder k where
  -- >     AddByteStringFoo :: Action Add m FooBuilder ByteString
  -- >     AddFileToFoo     :: Action Add m FooBuilder FilePath
  -- >     AddSelectedFoo   :: (j -> ByteString)  -> Action Add m FooBuilder j
  --
  -- Using this instance the 'buildAction' implementation can depend on the type
  -- 'parameter':
  --
  -- >   buildAction (AddSelectedFoo conv) j = buildAction AddByteStringFoo (conv j)
  -- >   buildAction AddByteStringFoo bs     = fooAppend bs
  -- >   buildAction AddFileToFoo f          = fooAddFile f
  data Action action m a :: * -> *
  -- | Create a 'BulderT' that implements the action.
  buildAction
    :: Action action m a t -> t -> (BuilderT m a ())

-- | Pure values that are monoidal
data PureMonoid (v :: *) =
  PureMonoid

-- | Builder for pure values that are monoids.
instance (Typeable m,Applicative m,Typeable v,Monoid v) => HasBuilder m (PureMonoid v) where
  data InitArgs m (PureMonoid v) = FromPureValue v
                               | EmptyPureValue
  newtype BuilderT m (PureMonoid v) a = PMW{runPureMonoidWriter ::
                                          Writer v a}
                                    deriving (Functor, Applicative, Monad)
  type Artifact m (PureMonoid v) = v
  initialiseBuilder (FromPureValue v) = PMW $ tell v
  initialiseBuilder EmptyPureValue = pure ()
  buildArtifact = pure . execWriter . runPureMonoidWriter

instance (Typeable v) => Show (InitArgs m (PureMonoid v)) where
  show _ = "pure values of type " ++ show (typeRep (Proxy :: Proxy v))

instance (Artifact m (PureMonoid v) ~ v,Monoid v,HasBuilder m (PureMonoid v)) => SupportsAction Append m (PureMonoid v) where
  data Action Append m (PureMonoid v) t where
        AppendPure :: Action Append m (PureMonoid v) v
        AppendPureWith :: (t -> v) -> Action Append m (PureMonoid v) t
  buildAction AppendPure = PMW . tell
  buildAction (AppendPureWith f) = PMW . tell . f
