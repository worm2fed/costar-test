module Library.Domain.Patron
  ( Patron (..)
  ) where

import Relude

import Data.UUID (UUID)

import Library.Domain.LibraryCardNumber (LibraryCardNumber)
import Library.Domain.Name (Name)

data Patron = Patron
  { pId :: UUID
  -- ^ Unique patron id.
  , pName :: Name
  -- ^ Patron name.
  , pNumber :: LibraryCardNumber
  -- ^ Library card number.
  }
  deriving stock (Show)
