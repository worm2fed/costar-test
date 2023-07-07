module Application
  ( application
  ) where

import Relude

import Data.Time (addUTCTime, getCurrentTime)
import Text.Pretty.Simple (pPrint)

import Env (logLibraryState)
import Error (catchError)
import Library.Connection.Library ()
import Library.Service.Library
  ( addBook
  , addPatron
  , borrowBook
  , findBook
  , getBookAvailability
  , removeBook
  , returnBook
  )
import Monad (runWithEnv)

application :: IO ()
application = runWithEnv $ do
  now <- liftIO getCurrentTime
  let borrowTime = addUTCTime 10000 now

  void $ addBook "book 1" "test" "1"
  void $ addBook "book 2" "test" "2"
  book <- addBook "book 3" "test" "3"
  void $ addBook "book 4" "test" "4"
  logLibraryState

  findBook Nothing Nothing (Just "4") >>= mapM_ removeBook
  logLibraryState

  void $ addPatron "John"
  patron <- addPatron "David"
  logLibraryState

  borrowBook patron book borrowTime
  logLibraryState
  -- currently following is possible, because we work with old state, but
  -- we can avoid it by adding check of availability inside `borrowBook`
  getBookAvailability book >>= pPrint
  borrowBook patron book now
  -- but this way - it's not possible
  ( findBook Nothing Nothing (Just "3")
      >>= mapM_ (\b -> borrowBook patron b borrowTime)
    )
    `catchError` \e -> putTextLn $ show e

  returnBook patron book
  getBookAvailability book >>= pPrint
