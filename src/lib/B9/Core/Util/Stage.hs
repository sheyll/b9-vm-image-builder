-- | Composable effectful stages. Each stage provides functionality that
-- other stages built upon, and can (type-) safely rely upon.
--
-- The implementation sits on the shoulders of 'more-extensible-effects'.
module B9.Core.Util.Stage where

import Control.Monad.Eff
import Data.Kind
import Data.Proxy
import Data.Typeable

type On s e = (HasStageDepedencies s e, Member (Stage s) e)

class (Monoid (StageOut s)) => IsStage s where
  type StageCfg s
  type StageCfg s = ()
  type StageOut s
  type StageOut s = ()
  type HasStageDepedencies s (e :: [Type -> Type]) :: Constraint
  type HasStageDepedencies s e = ()
  beforeStage
    :: (HasStageDepedencies s e)
    => s -> Eff e (StageCfg s)
  afterStage
    :: (HasStageDepedencies s e)
    => proxy s -> StageCfg s -> StageOut s -> Eff e ()
  afterStage _ _ _ = return ()

data Stage s a where
        AskStage :: (IsStage s) => proxy s -> Stage s (StageCfg s)
        TellStage :: (IsStage s) => proxy s -> StageOut s -> Stage s ()
    deriving (Typeable)

type StageE s e = Stage s ': e

askStage
  :: (IsStage s, Member (Stage s) e)
  => proxy s -> Eff e (StageCfg s)
askStage = send . AskStage

tellStage
  :: (Monoid (StageOut s), IsStage s, Member (Stage s) e)
  => proxy s -> StageOut s -> Eff e ()
tellStage = ((.) . (.)) send TellStage

runStageE
  :: forall s e a.
     (Monoid (StageOut s), IsStage s, HasStageDepedencies s e)
  => s -> Eff (Stage s ': e) a -> Eff e a
runStageE params m = do
  cfg <- beforeStage params
  (res, o) <- handleRelay ret (handle cfg) m
  let px = Proxy :: Proxy s
  afterStage px cfg o
  return res
  where
    ret :: a -> Eff e (a, StageOut s)
    ret x = return (x, mempty)

    handle :: StageCfg s -> Handler (Stage s) e (a, StageOut s)
    handle cfg (AskStage _px) k = k cfg
    handle _cfg (TellStage _px o) k = do
      (a, o') <- k ()
      return (a, o `mappend` o')
