-- | This module defines application and environment.
module Monad
  ( App
  , runWithEnv
  ) where

import Relude

import Control.Exception (catch, throwIO)
import Control.Monad.Except (MonadError (..))

import Env (Env, buildEnv)
import Error (ErrorWithSource)

-- | Main application monad.
newtype App a = App
  { runApp :: ReaderT Env IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader Env
    )

-- | This instance allows to throw and catch errors that are visible in type
-- definitions. The implementation relies on underlying 'IO' machinery.
instance MonadError ErrorWithSource App where
  throwError :: ErrorWithSource -> App a
  throwError = liftIO . throwIO
  {-# INLINE throwError #-}

  catchError :: App a -> (ErrorWithSource -> App a) -> App a
  catchError action handler = App . ReaderT $ \env -> do
    let run = usingReaderT env . runApp
    run action `catch` \e -> run $ handler e
  {-# INLINE catchError #-}

-- | Runs provided action with new 'Env'.
runWithEnv :: App a -> IO a
runWithEnv (runApp -> action) =
  buildEnv >>= flip usingReaderT action
{-# INLINE runWithEnv #-}
