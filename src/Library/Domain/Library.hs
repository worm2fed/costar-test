module Library.Domain.Library
  ( Library (..)
  ) where

import Data.Map (Map)

import Data.UUID (UUID)
import Library.Domain.Book (Book)
import Library.Domain.Patron (Patron)

data Library = Library
  { lBooks :: Map UUID Book
  -- ^ Books in library.
  , lPatrons :: Map UUID Patron
  -- ^ Library patrons.
  }
