module Library.Service.Book
  ( createBook
  ) where

import Relude

import Library.Domain.Book (Book (..))
import Library.Domain.BookStatus (BookStatus (..))
import Library.Domain.BookTitle (BookTitle)
import Library.Domain.ISBN (ISBN)
import Library.Domain.Name (Name)

-- | Creates a new 'Book'.
createBook :: Monad tx => BookTitle -> Name -> ISBN -> tx Book
createBook title author isbn =
  pure $
    Book
      { bTitle = title
      , bAuthor = author
      , bISBN = isbn
      , bStatus = Available
      }
