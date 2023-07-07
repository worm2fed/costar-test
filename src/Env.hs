-- | This module defines application and environment.
module Env
  ( Env (..)
  , buildEnv
  , modifyLibrary
  , getLibrary
  , logLibraryState
  ) where

import Relude

import Data.Map.Strict qualified as Map
import Text.Pretty.Simple (pPrint)

import Library.Domain.Library (Library (..))

-- | App environment.
newtype Env = Env
  { eLibrary :: IORef Library
  -- ^ We store library here.
  -- NOTE: This is not production solution!
  }

-- | Create 'Env'.
buildEnv :: IO Env
buildEnv = do
  eLibrary <-
    newIORef
      Library
        { lBooksInLibrary = Map.empty
        , lBooksBorrowed = Map.empty
        , lPatrons = Map.empty
        }
  pure Env{..}

-- | Helper to modify 'Library' in 'Env'.
modifyLibrary :: (MonadReader Env m, MonadIO m) => (Library -> Library) -> m ()
modifyLibrary f = do
  storage <- asks eLibrary
  library <- readIORef storage
  atomicWriteIORef storage $ f library

-- | Helper to get 'Library' from 'Env'.
getLibrary :: (MonadReader Env m, MonadIO m) => m Library
getLibrary = asks eLibrary >>= readIORef

-- | Helper to log current 'Library' state.
logLibraryState :: (MonadReader Env m, MonadIO m) => m ()
logLibraryState = getLibrary >>= pPrint
