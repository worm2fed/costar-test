module Library.Domain.LibraryNumber
  ( LibraryCardNumber
  ) where

import Data.UUID (UUID)

-- | Library card number.
type LibraryCardNumber = UUID
