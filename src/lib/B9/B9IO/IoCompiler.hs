-- | Compile a 'ProgramT' to 'IoProgram' that can be executed in the real-world.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module B9.B9IO.IoCompiler where

import B9.B9IO
import B9.Dsl.Core
import B9.Logging
import Control.Lens         hiding (from, (<.>))
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Data.Data
import Data.Graph           as Graph
import Data.Map             as Map hiding (null)
import Data.Singletons
import Data.Tree            as Tree
import Text.Printf          (printf)

-- | The monad transformer used to compile a 'ProgramT'
newtype IoCompiler a = IoCompiler
    { runIoCompiler :: StateT Ctx IoProgram a
    } deriving (Functor,Applicative,Monad,MonadState Ctx)

instance (a ~ ()) => CanLog (IoCompiler a) where
    logMsg l str = liftIoProgram (logMsg l str)

instance MonadIoProgram IoCompiler where
    liftIoProgram = IoCompiler . lift

-- | An alias for 'ProgramT's over 'IoCompiler'
type Program a = ProgramT IoCompiler a

-- | This monad contains all information gathered in 'Ctx' but is
-- 'ReaderT'. This is mainly to prevent an action added with 'addAction' to be
-- able to change the state, especially by adding more actions (which would not
-- be executed).
newtype IoProgBuilder a = IoProgBuilder
    { runIoProgBuilder :: ReaderT Ctx IoProgram a
    } deriving (Functor,Applicative,Monad,MonadReader Ctx)

instance (a ~ ()) => CanLog (IoProgBuilder a) where
    logMsg l str = liftIoProgram (logMsg l str)

instance MonadIoProgram IoProgBuilder where
    liftIoProgram = IoProgBuilder . lift

-- | State for artifacts required to generate the output program is
--   held in a single 'Map' containing the existential key/value types
--  'SomeHandle' -> 'SomeState'. This way the IoCompiler remains extensible.
type ArtifactStates = Map SomeHandle SomeState

-- | A existential type for holding state for artifacts
data SomeState where
        SomeState :: Typeable a => a -> SomeState
    deriving Typeable

-- | The internal state of the 'IoCompiler' monad
data Ctx = Ctx
    { _nextVertex :: Vertex
    , _actions :: Map Vertex [IoProgBuilder ()]
    , _hToV :: Map SomeHandle Vertex
    , _vToH :: Map Vertex SomeHandle
    , _dependencies :: [Edge]
    , _artifactStates :: ArtifactStates
    }

instance Default Ctx where
    def = Ctx 0 def def def [] def

makeLenses ''Ctx

-- | * Artifact state accessors

unpackCast :: (Typeable b) => Maybe SomeState -> Maybe b
unpackCast x =
  case x of
    Nothing -> Nothing
    Just (SomeState s) -> cast s

useArtifactState
    :: (Typeable b)
    => Handle a -> IoCompiler (Maybe b)
useArtifactState hnd =
    unpackCast <$> use (artifactStates . at (SomeHandle hnd))

putArtifactState
    :: (Typeable b)
    => Handle a -> b -> IoCompiler ()
putArtifactState hnd st =
    artifactStates . at (SomeHandle hnd) ?= SomeState st

modifyArtifactState
    :: (Typeable b)
    => Handle a -> (Maybe b -> Maybe b) -> IoCompiler ()
modifyArtifactState hnd f =
    artifactStates . at (SomeHandle hnd) %= mstate . f . mcast
    where
      mcast (Just (SomeState b')) = cast b'
      mcast Nothing = Nothing
      mstate = fmap SomeState

getArtifactState
    :: (Typeable b)
    => Handle a -> IoProgBuilder (Maybe b)
getArtifactState hnd =
    unpackCast <$> view (artifactStates . at (SomeHandle hnd))


-- | Compile a 'Program' to an 'IoProgram'
compile
    :: forall a.
       Program a -> IoProgram a
compile p = evalStateT (runIoCompiler compileSt) def
  where
    compileSt :: IoCompiler a
    compileSt = do
        liftIoProgram
            (do b <- getBuildId
                dbgL
                    "==[B9-PREPARE]=======================================================["
                    b
                    "]")
        result <- interpret p
        runAllActions
        liftIoProgram
            (do b <- getBuildId
                dbgL
                    "==[B9-FINISHED]======================================================["
                    b
                    "]")
        return result

-- | Compile a 'Program' but run no actions, instead just print out information
-- about the program using 'logTrace'
inspect :: Show a => Program a -> IoProgram String
inspect p = evalStateT (runIoCompiler compileSt) def
  where
    compileSt = do
        res <- interpret p
        mG <- dependencyGraph
        case mG of
            Just g -> do
                handles <- use vToH
                return (printDependencyGraph g handles)
            Nothing ->
                return ("No artifacts." ++ show res)

-- | Run all actions in correct order according to the dependency graph.
runAllActions :: IoCompiler ()
runAllActions = do
    liftIoProgram
             (do b <- getBuildId
                 traceL
                     "==[B9-EXECUTE]=======================================================["
                     b
                     "]")
    mG <- dependencyGraph
    case mG of
        Just g -> forM_ (topSort g) runActionForVertex
        Nothing -> liftIoProgram (traceL "No artifacts.")
  where
    runActionForVertex vertex = do
        Just actionsForVertex <- use (actions . at vertex)
        execIoProgBuilder (sequence_ actionsForVertex)

-- | Run an 'IoProgBuilder' action.
execIoProgBuilder :: IoProgBuilder a -> IoCompiler a
execIoProgBuilder a = do
    ctx <- get
    IoCompiler (lift (runReaderT (runIoProgBuilder a) ctx))

-- | Generate a graph from the artifact dependencies in the compiler context.
dependencyGraph :: IoCompiler (Maybe Graph)
dependencyGraph = do
    maxVertex <- use nextVertex
    if maxVertex > 0
        then do
            deps <- use dependencies
            return (Just (buildG (0, maxVertex - 1) deps))
        else return Nothing

-- | Show the dependency graph from the compiler context.
printDependencyGraph :: Graph -> Map Vertex SomeHandle -> String
printDependencyGraph g handles =
    unlines
        ("digraph artifactDependencyGraph {" :
         fmap (printEdge handles) (edges g) ++
         "}" :
         "Dependency forest:" :
         Tree.drawForest (fmap (printVertex handles) <$> dff g) :
         "Build order:" : (printVertex handles <$> topSort g))

-- | Convert an edge to a formatted string
printEdge :: Map Vertex SomeHandle -> Edge -> String
printEdge handles (u,v) =
    printf
        "  %s   ->    %s"
        (show (printVertex handles u))
        (show (printVertex handles v))

-- | Convert a vertex to a formatted string
printVertex :: Map Vertex SomeHandle -> Vertex -> String
printVertex handles v =
    printf "%s(%d)" (printSomeHandle (Map.lookup v handles)) v

-- | Convert maybe a handle to a string
printSomeHandle :: Maybe SomeHandle -> String
printSomeHandle (Just (SomeHandle h)) = show h
printSomeHandle Nothing = "??error??"

-- * Utilities

-- | Create a new unique handle and store it in the state.
allocHandle :: (SingKind ('KProxy :: KProxy k)
               ,Show (Demote (a :: k)))
               => Sing a
               -> String
               -> IoCompiler (Handle a, SomeHandle)
allocHandle sa str = do
    v <- addVertex
    let h = formatHandle v sa str
    h' <- storeHandle h v
    actions . at v ?=
        [liftIoProgram (traceL "==[B9-EXEC-ARTIFACT]==============[" h "]")]
    return (h, h')

-- | Setup a predefined global handle
allocPredefinedHandle
    :: (SingKind ('KProxy :: KProxy k), Show (Demote (a :: k)))
    => Handle a -> IoCompiler ()
allocPredefinedHandle h = do
    v <- addVertex
    void (storeHandle h v)
    actions . at v ?= []

-- | Add a handle to the vertex <-> handle maps in the state and return the
-- existential 'SomeHandle' that was stored in place of the polymorphic 'Handle
-- a'.
storeHandle :: Handle a -> Vertex -> IoCompiler SomeHandle
storeHandle h v = do
    let h' = SomeHandle h
    hToV . at h' ?= v
    vToH . at v ?= h'
    return h'

-- | Return a new and unique vertex (i.e. artifact id)
addVertex :: IoCompiler Vertex
addVertex = do
    v <- use nextVertex
    nextVertex += 1
    return v

-- | Generate a handle with formatted title
formatHandle :: (SingKind ('KProxy :: KProxy k)
                ,Show (Demote (a :: k)))
                => Vertex -> Sing a -> String -> Handle a
formatHandle v sa str =
    Handle
        sa
        (if str == ""
             then show v
             else str ++ "-" ++ show v)

-- | Add a dependency of one resource to another
(-->) :: Handle a -> Handle b -> IoCompiler ()
h --> h' = do
    Just v <- lookupVertex h
    Just v' <- lookupVertex h'
    dependencies <>= [(v, v')]

-- | Return the vertex of a handle.
lookupVertex :: Handle a -> IoCompiler (Maybe Vertex)
lookupVertex h = use (hToV . at (SomeHandle h))

-- * Support for 'IoProgBuilder's

-- | Add a build action to a handle
addAction :: Handle a -> IoProgBuilder () -> IoCompiler ()
addAction h a = do
  Just v <- lookupVertex h
  actions . at v . traverse <>= [a]
