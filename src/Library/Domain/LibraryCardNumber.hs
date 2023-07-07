module Library.Domain.LibraryCardNumber
  ( LibraryCardNumber
  ) where

import Data.UUID (UUID)

-- | Library card number.
type LibraryCardNumber = UUID
