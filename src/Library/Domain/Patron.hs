module Library.Domain.Patron
  ( Patron (..)
  ) where

import Relude

import Library.Domain.LibraryNumber (LibraryCardNumber)

data Patron = Patron
  { pName :: Text
  -- ^ Patron name.
  , pNumber :: LibraryCardNumber
  -- ^ Library card number.
  }
