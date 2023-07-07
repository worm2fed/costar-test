{-# OPTIONS_GHC -Wno-orphans #-}

module Library.Connection.Library
  (
  ) where

import Relude

import Data.Map.Strict qualified as Map

import Env (getLibrary, modifyLibrary)
import Library.Domain.Book (Book (..))
import Library.Domain.BookStatus (BookStatus)
import Library.Domain.BookTitle (BookTitle)
import Library.Domain.ISBN (ISBN)
import Library.Domain.Library (Library (..))
import Library.Domain.Name (Name)
import Library.Domain.Patron (Patron (..))
import Library.Repository.Library (LibraryRepository (..))
import Monad (App)

instance LibraryRepository App where
  addBook :: Book -> App Book
  addBook book@Book{bId} = do
    modifyLibrary $ \library@Library{..} ->
      library{lBooksInLibrary = Map.insert bId book lBooksInLibrary}
    pure book

  removeBook :: Book -> App ()
  removeBook Book{bId} = do
    modifyLibrary $ \library@Library{..} ->
      library{lBooksInLibrary = Map.delete bId lBooksInLibrary}

  findBook :: Maybe BookTitle -> Maybe Name -> Maybe ISBN -> App (Maybe Book)
  findBook Nothing Nothing Nothing = pure Nothing
  findBook title author isbn = do
    Library{..} <- getLibrary
    let books =
          Map.filter
            ( \Book{..} ->
                fromMaybe bISBN isbn == bISBN
                  && fromMaybe bTitle title == bTitle
                  && fromMaybe bAuthor author == bAuthor
            )
            lBooksInLibrary
    pure . fmap snd . viaNonEmpty head $ Map.toList books

  addPatron :: Patron -> App Patron
  addPatron patron@Patron{pId} = do
    modifyLibrary $ \library@Library{..} ->
      library{lPatrons = Map.insert pId patron lPatrons}
    pure patron

  deletePatron :: Patron -> App ()
  deletePatron Patron{pId} = do
    modifyLibrary $ \library@Library{..} ->
      library{lPatrons = Map.delete pId lPatrons}

  getBookAvailability :: Book -> App (Maybe BookStatus)
  getBookAvailability Book{bId} = do
    Library{..} <- getLibrary
    pure $ bStatus <$> Map.lookup bId lBooksInLibrary

  borrowBook :: Patron -> Book -> App ()
  borrowBook Patron{pId} book@Book{bId} = do
    modifyLibrary $ \library@Library{..} ->
      library
        { lBooksInLibrary = Map.adjust (const book) bId lBooksInLibrary
        , lBooksBorrowed = Map.insert bId pId lBooksBorrowed
        }

  returnBook :: Patron -> Book -> App ()
  returnBook _patron book@Book{bId} = do
    -- NOTE: for now there is no any check that book is returned by patron
    modifyLibrary $ \library@Library{..} ->
      library
        { lBooksInLibrary = Map.adjust (const book) bId lBooksInLibrary
        , lBooksBorrowed = Map.delete bId lBooksBorrowed
        }
