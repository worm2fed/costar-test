module Library.Domain.Patron
  ( Patron (..)
  ) where

import Library.Domain.LibraryNumber (LibraryCardNumber)
import Library.Domain.Name (Name)

data Patron = Patron
  { pName :: Name
  -- ^ Patron name.
  , pNumber :: LibraryCardNumber
  -- ^ Library card number.
  }
