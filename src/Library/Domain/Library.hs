module Library.Domain.Library
  ( Library (..)
  ) where

import Library.Domain.Book (Book)
import Library.Domain.LibraryNumber (LibraryCardNumber)

data Library = Library
  { lBooks :: [Book]
  -- ^ Books in library.
  , lPatrons :: [LibraryCardNumber]
  -- ^ Library patrons.
  }
