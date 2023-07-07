module Library.Domain.BookStatus
  ( BookStatus (..)
  ) where

import Relude

import Data.Time (UTCTime)

-- | Current 'Book' status.
data BookStatus = Available | NotAvailable | Borrowed UTCTime
  deriving stock (Show, Eq)
