module Library.Domain.BookStatus
  ( BookStatus
  ) where

import Relude

-- | Current 'Book' status.
data BookStatus = Available | Borrowed
  deriving stock (Show, Eq)
