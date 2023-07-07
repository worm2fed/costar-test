module Library.Domain.Book
  ( Book (..)
  ) where

import Data.UUID (UUID)

import Library.Domain.BookStatus (BookStatus)
import Library.Domain.BookTitle (BookTitle)
import Library.Domain.ISBN (ISBN)
import Library.Domain.Name (Name)

data Book = Book
  { bId :: UUID
  -- ^ Unique book id.
  , bTitle :: BookTitle
  -- ^ Book title.
  , bAuthor :: Name
  -- ^ Book author.
  , bISBN :: ISBN
  -- ^ International Standard Book Number.
  , bStatus :: BookStatus
  }
