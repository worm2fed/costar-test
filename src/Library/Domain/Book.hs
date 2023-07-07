module Library.Domain.Book
  ( Book (..)
  ) where

import Relude

import Library.Domain.BookStatus (BookStatus)
import Library.Domain.ISBN (ISBN)

data Book = Book
  { bTitle :: Text
  -- ^ Book title.
  , bAuthor :: Text
  -- ^ Book author.
  , bISBN :: ISBN
  -- ^ International Standard Book Number.
  , bStatus :: BookStatus
  }
