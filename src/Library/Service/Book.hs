module Library.Service.Book
  ( createBook
  , markBookBorrowed
  , markBookReturned
  ) where

import Relude

import Data.Time (UTCTime)
import Data.UUID.V4 qualified as UUID

import Library.Domain.Book (Book (..))
import Library.Domain.BookStatus (BookStatus (..))
import Library.Domain.BookTitle (BookTitle)
import Library.Domain.ISBN (ISBN)
import Library.Domain.Name (Name)

-- | Creates a new 'Book'.
createBook :: MonadIO tx => BookTitle -> Name -> ISBN -> tx Book
createBook title author isbn = do
  uuid <- liftIO UUID.nextRandom
  pure $
    Book
      { bId = uuid
      , bTitle = title
      , bAuthor = author
      , bISBN = isbn
      , bStatus = Available
      }

-- | Mark 'Book' as borrowed.
markBookBorrowed :: Book -> UTCTime -> Book
markBookBorrowed book due = book{bStatus = Borrowed due}

-- | Mark 'Book' as returned.
markBookReturned :: Book -> Book
markBookReturned book = book{bStatus = Available}
