{-# OPTIONS_GHC -threaded #-}

module Main (
  main
) where

import           Prelude
import           Relude

import           Example
import           Flow
import           Potato
import           Layout2Test

import           GHC.IO.Handle
import           GHC.IO.Handle.FD
import           System.IO



main :: IO ()
--main = exampleMain
--main = potatoMain
--main = mainWithDebug
--main = layoutTestMain
main = easyExample


mainWithDebug :: IO ()
mainWithDebug = do
  -- vty takes over stdout so this only will work with stderr
  fd <- openFile "stderr.txt" WriteMode
  hDuplicateTo fd stderr  -- redirect stdout to file
  hPutStrLn stderr "STDERR" -- will print to stderr
  flowMain
  hClose fd
