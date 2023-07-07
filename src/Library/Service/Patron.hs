module Library.Service.Patron
  ( createPatron
  ) where

import Relude

import Data.UUID.V4 qualified as UUID

import Library.Domain.LibraryCardNumber (LibraryCardNumber)
import Library.Domain.Name (Name)
import Library.Domain.Patron (Patron (..))

-- | Creates a new 'Patron'.
createPatron :: MonadIO tx => Name -> LibraryCardNumber -> tx Patron
createPatron name card = do
  uuid <- liftIO UUID.nextRandom
  pure $
    Patron
      { pId = uuid
      , pName = name
      , pNumber = card
      }
