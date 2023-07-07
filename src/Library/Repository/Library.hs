module Library.Repository.Library
  ( LibraryRepository (..)
  ) where

import Relude

import Library.Domain.Book (Book)
import Library.Domain.BookStatus (BookStatus)
import Library.Domain.BookTitle (BookTitle)
import Library.Domain.ISBN (ISBN)
import Library.Domain.Library (Library)
import Library.Domain.Name (Name)
import Library.Domain.Patron (Patron)

class Monad tx => LibraryRepository tx where
  -- | Add 'Book' to 'Library'.
  addBook
    :: Library
    -> Book
    -> tx ()

  -- | Removes 'Book' from 'Library'.
  removeBook
    :: Library
    -> Book
    -> tx ()

  -- | Finds 'Book' in 'Library'.
  findBook
    :: Library
    -> Maybe BookTitle
    -> Maybe Name
    -> Maybe ISBN
    -> tx (Maybe Book)

  -- | Gets 'Book's 'BookStatus' in 'Library'.
  getBookAvailability
    :: Library
    -> Book
    -> tx BookStatus

  -- | Borrow 'Book' in 'Library' by 'Patron'.
  borrowBook
    :: Library
    -> Patron
    -> Book
    -> tx ()

  -- | Return 'Book' to 'Library' by 'Patron'.
  returnBook
    :: Library
    -> Patron
    -> Book
    -> tx ()
