module Main
  ( main
  ) where

import Relude

import Main.Utf8 (withUtf8)

import Application (application)

main :: IO ()
main = withUtf8 $ do
  hSetBuffering stdout LineBuffering
  application
