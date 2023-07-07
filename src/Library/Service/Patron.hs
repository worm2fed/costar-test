module Library.Service.Patron
  ( createPatron
  ) where

import Relude

import Data.UUID.V4 qualified as UUID

import Library.Domain.Name (Name)
import Library.Domain.Patron (Patron (..))

-- | Creates a new 'Patron'.
createPatron :: MonadIO tx => Name -> tx Patron
createPatron name = do
  -- NOTE: actually this is bad place to issue library internal number
  libraryCard <- liftIO UUID.nextRandom
  pure $
    Patron
      { pName = name
      , pNumber = libraryCard
      }
