-- | Error handling in B9 via extensible effects.
-- B9 wraps errors in `SomeException`.
--
-- @since 0.5.64
module B9.B9Error
  ( throwSomeException
  , throwSomeException_
  , throwB9Error
  , throwB9Error_
  , errorOnException
  , ExcB9
  , runExcB9
  , B9Error(MkB9Error)
  , fromB9Error
  , catchB9Error
  , catchB9ErrorAsEither
  )
where

import           Control.Exception              ( toException
                                                , SomeException
                                                , Exception
                                                , displayException
                                                )
import           Control.Eff                   as Eff
import           Control.Eff.Exception         as Eff
import           Control.Monad
import           Data.String                    ( IsString(..) )

-- | The exception effect used in most places in B9.
--  This is `Exc` specialized with `SomeException`.
--
-- @since 0.5.64
type ExcB9 = Exc SomeException

-- | This is a simple runtime exception to indicate that B9 code encountered
-- some exceptional event.
--
-- @since 0.5.64
newtype B9Error = MkB9Error { fromB9Error :: String }
  deriving (Show, IsString)

instance Exception B9Error

-- | Run an `ExcB9`.
--
-- @since 0.5.64
runExcB9 :: Eff (ExcB9 ': e) a -> Eff e (Either SomeException a)
runExcB9 = runError

-- | Run an `ExcB9` and rethrow the exception with `error`.
--
-- @since 0.5.64
errorOnException :: Eff (ExcB9 ': e) a -> Eff e a
errorOnException = runError >=> either (error . displayException) pure

-- | 'SomeException' wrapped into 'Exc'ecption 'Eff'ects
--
-- @since 0.5.64
throwSomeException :: (Member ExcB9 e, Exception x) => x -> Eff e a
throwSomeException = throwError . toException

-- | 'SomeException' wrapped into 'Exc'ecption 'Eff'ects
--
-- @since 0.5.64
throwSomeException_ :: (Member ExcB9 e, Exception x) => x -> Eff e ()
throwSomeException_ = throwError_ . toException


-- | 'SomeException' wrapped into 'Exc'ecption 'Eff'ects
--
-- @since 0.5.64
throwB9Error :: Member ExcB9 e => String -> Eff e a
throwB9Error = throwSomeException . MkB9Error

-- | 'SomeException' wrapped into 'Exc'ecption 'Eff'ects
--
-- @since 0.5.64
throwB9Error_ :: Member ExcB9 e => String -> Eff e ()
throwB9Error_ = throwSomeException_ . MkB9Error

-- | Catch exceptions.
--
-- @since 0.5.64
catchB9Error
  :: Member ExcB9 e => Eff e a -> (SomeException -> Eff e a) -> Eff e a
catchB9Error = catchError


-- | Catch exceptions and return them via 'Either'.
--
-- @since 0.5.64
catchB9ErrorAsEither
  :: Member ExcB9 e => Eff e a -> Eff e (Either SomeException a)
catchB9ErrorAsEither x = catchB9Error (Right <$> x) (pure . Left)
