{-# LANGUAGE UndecidableInstances #-}

module B9.Core.Dsl.Interpreter where

import B9.Core.Prelude
import B9.Core.System.B9IO
import B9.Core.Dsl.Types.BuildStep
import B9.Core.Util.DynMap as DynMap
import Control.Lens hiding (from, (<.>))
import Data.Graph as Graph
import qualified Data.Map as Map hiding (null)
import Data.Tree as Tree

-- | A monad to made from a 'BuildStepMonad' but with all dependencies worked
-- out, such that the build order is correct.
newtype BuildPlan a =
  BuildPlan {runBuildPlan :: State Ctx a}
  deriving (Functor,Applicative,Monad,MonadState Ctx)

-- | The internal state of the 'BuildPlan' monad
data Ctx =
  Ctx {_nextVertex :: Vertex
      ,_hToV :: Map.Map SomeHandle Vertex
      ,_vToH :: Map.Map Vertex SomeHandle
      ,_dependencies :: [Edge]
      ,_builderMap :: BuilderMap
      ,_generatorMap :: Map.Map SomeHandle Generator}

instance Default Ctx where
  def = Ctx 0 def def [] DynMap.empty def

type Generator = (ArtifactMap,BuilderMap) -> B9IO (ArtifactMap,BuilderMap)

type BuilderMap = DynMap.DynMap BuildersEntry

type ArtifactMap = DynMap.DynMap ArtifactsEntry

instance DynMap.Entry BuildersEntry k where
  data Key BuildersEntry k = BuildersKey (Handle k)
  type Value BuildersEntry k = BuilderCtx k
  toSomeKey (BuildersKey h) = SomeKey (show h)

type BuilderCtx k = [ArtifactMap -> (BuilderT k ())]

data BuildersEntry


instance DynMap.Entry ArtifactsEntry k where
  data Key ArtifactsEntry k = ArtifactsKey (Handle k)
  type Value ArtifactsEntry k = Artifact k
  toSomeKey (ArtifactsKey h) = SomeKey (show h)

data ArtifactsEntry

makeLenses ''Ctx

-- | Interpret a 'BuildStepMonad' into a 'BuildPlan'.
interpret :: BuildStepMonad a -> BuildPlan a
interpret = foldFree go
  where go :: BuildStep x -> BuildPlan x
        go (Create initialParameter k) =
          do h <- allocHandle
             putBuilderT h
                         [const (initialiseBuilder initialParameter)]
             generatorMap . at (SomeHandle h) ?= mkGenerator h
             return $ k h
        go (Apply action handle next) =
          do addBuilderT handle
                         [const (execAction action)]
             return next
        go (Bind sourceHandle action destinationHandle next) =
          do sourceHandle --> destinationHandle
             addBuilderT
               destinationHandle
               [execAction . action . fromJust .
                DynMap.lookup (ArtifactsKey sourceHandle)]
             return next

-- | Create a 'Generator' for a 'Handle'.
mkGenerator :: (CanBuild a)
            => Handle a -> Generator
mkGenerator handle (artifactMapIn,builderMapIn) =
  do let bkey = BuildersKey handle
         akey = ArtifactsKey handle
         builder =
           sequenceA_ $ map ($ artifactMapIn) $ join $ maybeToList $
           DynMap.lookup bkey builderMapIn
     artifact <- buildArtifact builder
     let artifactMapOut = DynMap.insert akey artifact artifactMapIn
         builderMapOut = DynMap.delete bkey builderMapIn
     return (artifactMapOut,builderMapOut)

-- | * Builder and Artifact accessors
putBuilderT
  :: (Typeable (BuilderCtx a))
  => Handle a -> BuilderCtx a -> BuildPlan ()
putBuilderT handle b =
  builderMap %=
  DynMap.insert (BuildersKey handle)
                b

addBuilderT
  :: (Typeable (BuilderCtx a))
  => Handle a -> BuilderCtx a -> BuildPlan ()
addBuilderT handle b =
  builderMap %=
  DynMap.insertWith (++)
                    (BuildersKey handle)
                    b

-- | Create a new unique handle and store it in the state.
allocHandle
  :: (Typeable a)
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
  :: BuildPlan a -> B9IO (a,ArtifactMap)
execBuildPlan di =
  let ((a,depGraph),ctx) =
        runState (runBuildPlan ((,) <$> di <*> dependencyGraph)) def
      orderedVertices = maybe [0 .. _nextVertex ctx - 1] topSort depGraph
      vToG =
        Map.map (flip Map.lookup (_generatorMap ctx))
                (_vToH ctx)
      builders = catMaybes $ map (join . flip Map.lookup vToG) orderedVertices
  in do (artifactsOut,_builderOut) <-
          foldr (=<<) (return (DynMap.empty,_builderMap ctx)) builders
        return (a,artifactsOut)

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
printVertex handles v =
  printf "%s" (printSomeHandle (Map.lookup v handles))

-- | Convert maybe a handle to a string
printSomeHandle :: Maybe SomeHandle -> String
printSomeHandle (Just (SomeHandle h)) = show h
printSomeHandle Nothing = "??error??"
