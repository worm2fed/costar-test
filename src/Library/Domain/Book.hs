module Library.Domain.Book
  ( Book (..)
  ) where

import Library.Domain.BookStatus (BookStatus)
import Library.Domain.BookTitle (BookTitle)
import Library.Domain.ISBN (ISBN)
import Library.Domain.Name (Name)

data Book = Book
  { bTitle :: BookTitle
  -- ^ Book title.
  , bAuthor :: Name
  -- ^ Book author.
  , bISBN :: ISBN
  -- ^ International Standard Book Number.
  , bStatus :: BookStatus
  }
