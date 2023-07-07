module Library.Domain.Library
  ( Library (..)
  ) where

import Data.Map (Map)

import Data.UUID (UUID)
import Library.Domain.Book (Book)
import Library.Domain.Patron (Patron)

data Library = Library
  { lBooksInLibrary :: Map UUID Book
  -- ^ Books in library.
  , lBooksBorrowed :: Map UUID UUID
  -- ^ Books that was borrowed (book id - patron id).
  , lPatrons :: Map UUID Patron
  -- ^ Library patrons.
  }
