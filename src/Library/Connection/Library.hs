{-# OPTIONS_GHC -Wno-orphans #-}

module Library.Connection.Library
  (
  ) where

import Relude

import Data.Map.Strict qualified as Map

import Env (Env (..), modifyLibrary)
import Library.Domain.Book (Book (..))
import Library.Domain.BookStatus (BookStatus)
import Library.Domain.BookTitle (BookTitle)
import Library.Domain.ISBN (ISBN)
import Library.Domain.Library (Library (..))
import Library.Domain.Name (Name)
import Library.Domain.Patron (Patron)
import Library.Repository.Library (LibraryRepository (..))
import Monad (App)

instance LibraryRepository App where
  addBook
    :: Book
    -> App ()
  addBook book@Book{bId} = do
    modifyLibrary $ \library@Library{..} ->
      library{lBooks = Map.insert bId book lBooks}
