{-# LANGUAGE UndecidableInstances #-}

module B9.Core.DslInterpreter where

import B9.Core.Prelude
import B9.Core.System.B9IO
import B9.Core.Dsl.Types.BuildStep
import B9.Core.Util.DynMap as DynMap
import Control.Lens hiding (from, (<.>))
import Data.Graph as Graph
import qualified Data.Map as Map hiding (null)
import Data.Tree as Tree

-- | A monad to made from a 'BuildStepMonad' but with all dependencies worked out, such that the
-- build order is correct.
newtype BuildPlan a =
  BuildPlan {runBuildPlan :: State Ctx a}
  deriving (Functor,Applicative,Monad,MonadState Ctx)

-- | The internal state of the 'BuildPlan' monad
data Ctx =
  Ctx {_nextVertex :: Vertex
      ,_hToV :: Map.Map SomeHandle Vertex
      ,_vToH :: Map.Map Vertex SomeHandle
      ,_dependencies :: [Edge]
      ,_builders :: Map.Map SomeHandle (ArtifactBuilder ())
      ,_bindings :: Bindings}

instance Default Ctx where
  def = Ctx 0 def def [] Map.empty DynMap.empty

type Bindings = DynMap.DynMap Binding

data Binding

instance DynMap.Entry Binding k where
  data Key Binding k = BindingKey (Handle k)
  type Value Binding k = BuilderRWST (Reader ArtifactMap) k ()
  toSomeKey (BindingKey h) = SomeKey (show h)

type ArtifactBuilder = StateT ArtifactMap (ReaderT Bindings B9IO)

runArtifactBuilder :: ArtifactBuilder a
                   -> ArtifactMap
                   -> Bindings
                   -> B9IO (a,ArtifactMap)
runArtifactBuilder b = runReaderT . runStateT b

type ArtifactMap = DynMap.DynMap ArtifactsEntry

data ArtifactsEntry

instance DynMap.Entry ArtifactsEntry k where
  data Key ArtifactsEntry k = ArtifactsKey (Handle k)
  type Value ArtifactsEntry k = Artifact k
  toSomeKey (ArtifactsKey h) = SomeKey (show h)

makeLenses ''Ctx

-- | Interpret a 'BuildStepMonad' into a 'BuildPlan'.
interpret :: BuildStepMonad a -> BuildPlan a
interpret = foldFree go
  where go :: BuildStep x -> BuildPlan x
        go (Create initialParameter k) =
          do h <- allocHandle
             insertBuilder h initialParameter
             return (k h)
        go (Apply action handle next) =
          do appendBinding handle
                           (execAction action)
             return next
        go (Bind sourceHandle action destinationHandle next) =
          do sourceHandle --> destinationHandle
             appendBinding destinationHandle $
               do Just a <-
                    lift (asks (DynMap.lookup (ArtifactsKey sourceHandle)))
                  execAction (action a)
             return next

-- | * Builder and Artifact accessors
insertBuilder
  :: CanBuild a
  => Handle a -> InitArgs a -> BuildPlan ()
insertBuilder h i =
  do builders . at (SomeHandle h) ?= builder
     bindings %=
       DynMap.insert (BindingKey h)
                     (return ())
  where builder =
          do Just b <- lift (asks (DynMap.lookup (BindingKey h)))
             as <- get
             let ((),s,w) =
                   runReader (runBuilderRWST b
                                             i
                                             (initialiseBuilder i))
                             as
             a <- lift (lift (buildArtifact i s w))
             modify (DynMap.insert (ArtifactsKey h)
                                   a)

appendBinding :: CanBuild a
              => Handle a
              -> BuilderRWST (Reader ArtifactMap) a ()
              -> BuildPlan ()
appendBinding h b =
  bindings %=
  DynMap.adjust (>> b)
                (BindingKey h)

-- | Create a new unique handle and store it in the state.
allocHandle :: (Typeable a)
            => BuildPlan (Handle a)
allocHandle =
  do v <- addVertex
     let h = mkHandle (printf "Handle #%0.10d" v)
     void $ storeHandle h v
     return h

-- | Add a handle to the vertex <-> handle maps in the state and return the existential 'SomeHandle'
-- that was stored in place of the polymorphic 'Handle a'.
storeHandle
  :: Typeable a
  => Handle a -> Vertex -> BuildPlan SomeHandle
storeHandle h v =
  do let h' = SomeHandle h
     hToV . at h' ?= v
     vToH . at v ?= h'
     return h'

-- | Return a new and unique vertex (i.e. artifact id)
addVertex :: BuildPlan Vertex
addVertex =
  do v <- use nextVertex
     nextVertex += 1
     return v

-- | Add a dependency of one resource to another: @addDependency x y@ means first build @x@ then
-- @y@.
addDependency
  :: (Typeable a,Typeable b)
  => Handle a -> Handle b -> BuildPlan ()
addDependency = (-->)

-- | An alias for 'addDependency': @x --> y@ means @addDependency x y@ - i.e. first build @x@ then
-- @y@.
(-->) :: (Typeable a,Typeable b)
      => Handle a -> Handle b -> BuildPlan ()
h --> h' =
  do Just v <- lookupVertex h
     Just v' <- lookupVertex h'
     dependencies <>= [(v,v')]

-- | Return the vertex of a handle.
lookupVertex
  :: Typeable a
  => Handle a -> BuildPlan (Maybe Vertex)
lookupVertex h = use (hToV . at (SomeHandle h))

-- | Execution of the 'BuildPlan'. Compose all build steps into an 'IoProgram', with all build
-- actions executed in the order determined by the data-dependencies between buildsteps.
execBuildPlan
  :: BuildPlan r -> B9IO (r,ArtifactMap)
execBuildPlan di =
  let ((r,g),ctx) =
        runState (runBuildPlan ((,) <$> di <*> dependencyGraph)) def
      vs = maybe [0 .. _nextVertex ctx - 1] topSort g
      go v =
        fromMaybe (return ())
                  (do h <- ctx ^. vToH . at v
                      ctx ^. builders . at h)
  in runArtifactBuilder (mapM_ go vs >> return r)
                        DynMap.empty
                        (ctx ^. bindings)

-- | Generate a graph from the artifact dependencies in the compiler context.
dependencyGraph :: BuildPlan (Maybe Graph)
dependencyGraph =
  do maxVertex <- use nextVertex
     if maxVertex > 0
        then do deps <- use dependencies
                return (Just (buildG (0,maxVertex - 1) deps))
        else return Nothing

-- | Show the dependency graph from the compiler context.
printDependencyGraph
  :: Graph -> Map.Map Vertex SomeHandle -> String
printDependencyGraph g handles =
  unlines ("digraph artifactDependencyGraph {" :
           fmap (printEdge handles)
                (edges g) ++
           "}" :
           "Dependency forest:" :
           Tree.drawForest (fmap (printVertex handles) <$> dff g) :
           "Build order:" :
           (printVertex handles <$> topSort g))

-- | Convert an edge to a formatted string
printEdge
  :: Map.Map Vertex SomeHandle -> Edge -> String
printEdge handles (u,v) =
  printf "  %s   ->    %s"
         (show (printVertex handles u))
         (show (printVertex handles v))

-- | Convert a vertex to a formatted string
printVertex
  :: Map.Map Vertex SomeHandle -> Vertex -> String
printVertex handles v = printf "%s" (printSomeHandle (Map.lookup v handles))

-- | Convert maybe a handle to a string
printSomeHandle :: Maybe SomeHandle -> String
printSomeHandle (Just (SomeHandle h)) = show h
printSomeHandle Nothing = "??error??"
