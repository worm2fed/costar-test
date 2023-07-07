module Library.Service.Patron
  ( createPatron
  ) where

import Relude

import Library.Domain.LibraryNumber (LibraryCardNumber)
import Library.Domain.Name (Name)
import Library.Domain.Patron (Patron (..))

-- | Creates a new 'Patron'.
createPatron :: MonadIO tx => Name -> LibraryCardNumber -> tx Patron
createPatron name card = do
  pure $
    Patron
      { pName = name
      , pNumber = card
      }
