module Library.Repository.Library
  ( LibraryRepository (..)
  ) where

import Relude

import Library.Domain.Book (Book)
import Library.Domain.BookStatus (BookStatus)
import Library.Domain.BookTitle (BookTitle)
import Library.Domain.ISBN (ISBN)
import Library.Domain.Name (Name)
import Library.Domain.Patron (Patron)

class Monad tx => LibraryRepository tx where
  -- | Add 'Book' to 'Library'.
  addBook
    :: Book
    -> tx Book

  -- | Removes 'Book' from 'Library'.
  removeBook
    :: Book
    -> tx ()

  -- | Finds 'Book' in 'Library'.
  findBook
    :: Maybe BookTitle
    -> Maybe Name
    -> Maybe ISBN
    -> tx (Maybe Book)

  -- | Add 'Patron' to 'Library'.
  addPatron
    :: Patron
    -> tx Patron

  -- | Delete 'Patron' from 'Library'.
  deletePatron
    :: Patron
    -> tx ()

  -- | Gets 'Book's 'BookStatus' in 'Library'.
  getBookAvailability
    :: Book
    -> tx (Maybe BookStatus)

  -- | Borrow 'Book' in 'Library' by 'Patron'.
  borrowBook
    :: Patron
    -> Book
    -> tx ()

  -- | Return 'Book' to 'Library' by 'Patron'.
  returnBook
    :: Patron
    -> Book
    -> tx ()
