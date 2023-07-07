-- | This module describes application error types.
module Error
  ( Error (..)
  , WithError
  , ErrorWithSource (..)

    -- * Helper functions
  , throwError
  , catchError
  , liftError
  , throwOnNothing
  , throwOnNothingM
  ) where

import Relude

import Control.Monad.Except qualified as E

-- | Errors in application.
data Error
  = NotImplemented
  | BookNotAvailable
  deriving stock (Show, Eq)

-- | Type alias for errors that has access to 'CallStack'.
type WithError m = (HasCallStack, E.MonadError ErrorWithSource m)

-- | Wrapper around error type with attached source code position.
data ErrorWithSource = ErrorWithSource
  { ewsSourcePosition :: !CallStack
  , ewsError :: !Error
  }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Specialized version of 'E.throwError' that attaches source code position of
-- the place where this error was thrown.
throwError :: WithError m => Error -> m a
throwError = withFrozenCallStack do
  E.throwError . ErrorWithSource callStack
{-# INLINE throwError #-}

-- | Specialized version of 'E.catchError'.
catchError :: WithError m => m a -> (Error -> m a) -> m a
catchError action handler = action `E.catchError` (handler . ewsError)
{-# INLINE catchError #-}

-- | Lift errors from 'Either' by re-throwing them with attached source position.
liftError :: WithError m => Either Error a -> m a
liftError = either throwError pure
{-# INLINE liftError #-}

-- | Extract the value from a 'Maybe', throwing the given @err@ if the value
-- does not exist.
throwOnNothing :: WithError m => Error -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack (maybe (throwError err) pure)
{-# INLINE throwOnNothing #-}

-- | Extract the value from a 'Maybe' in @m@, throwing the given @err@ if
-- the value does not exist.
throwOnNothingM :: WithError m => Error -> m (Maybe a) -> m a
throwOnNothingM err action = withFrozenCallStack (action >>= throwOnNothing err)
{-# INLINE throwOnNothingM #-}
