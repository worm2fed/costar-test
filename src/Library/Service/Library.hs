module Library.Service.Library
  ( addBook
  , removeBook
  , findBook
  , addPatron
  , deletePatron
  , getBookAvailability
  , borrowBook
  , returnBook
  ) where

import Relude

import Data.Time (UTCTime)
import Data.UUID.V4 qualified as UUID

import Error (Error (..), WithError, throwError)
import Library.Domain.Book (Book (..))
import Library.Domain.BookStatus (BookStatus (..))
import Library.Domain.BookTitle (BookTitle)
import Library.Domain.ISBN (ISBN)
import Library.Domain.Name (Name)
import Library.Domain.Patron (Patron (..))
import Library.Repository.Library (LibraryRepository)
import Library.Repository.Library qualified as R
import Library.Service.Book (createBook, markBookBorrowed, markBookReturned)
import Library.Service.Patron (createPatron)

-- | Add 'Book' to 'Library'.
addBook
  :: (MonadIO tx, LibraryRepository tx)
  => BookTitle
  -> Name
  -> ISBN
  -> tx ()
addBook title author isbn = do
  book <- createBook title author isbn
  R.addBook book

-- | Removes 'Book' from 'Library'.
-- TODO: check that book is not borrowed
removeBook :: LibraryRepository tx => Book -> tx ()
removeBook = R.removeBook

-- | Finds 'Book' in 'Library'.
findBook
  :: LibraryRepository tx
  => Maybe BookTitle
  -> Maybe Name
  -> Maybe ISBN
  -> tx (Maybe Book)
findBook = R.findBook

-- | Add 'Patron' to 'Library'.
addPatron :: (MonadIO tx, LibraryRepository tx) => Name -> tx ()
addPatron name = do
  patron <- liftIO UUID.nextRandom >>= createPatron name
  R.addPatron patron

-- | Delete 'Patron' from 'Library'.
-- TODO: check that patron do not have borrowed books.
deletePatron :: LibraryRepository tx => Patron -> tx ()
deletePatron = R.deletePatron

-- | Gets 'Book's 'BookStatus' in 'Library'.
getBookAvailability :: LibraryRepository tx => Book -> tx (Maybe BookStatus)
getBookAvailability = R.getBookAvailability

-- | Borrow 'Book' in 'Library' by 'Patron'.
borrowBook
  :: (LibraryRepository tx, WithError tx)
  => Patron
  -> Book
  -> UTCTime
  -> tx ()
borrowBook patron book@Book{bStatus} due = do
  unless (bStatus == Available) $
    throwError BookNotAvailable
  -- TODO: add check that due is not in past
  R.borrowBook patron $
    markBookBorrowed book due

-- | Return 'Book' to 'Library' by 'Patron'.
returnBook :: LibraryRepository tx => Patron -> Book -> tx ()
returnBook patron book = do
  -- TODO: add check that due book is returned in time.
  R.returnBook patron $
    markBookReturned book
