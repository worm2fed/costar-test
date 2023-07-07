module Application
  ( application
  ) where

import Relude

import Error (Error (..), throwError)
import Monad (runWithEnv)

application :: IO ()
application = runWithEnv $ do
  throwError NotImplemented
