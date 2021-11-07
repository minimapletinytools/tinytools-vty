{-# OPTIONS_GHC -threaded #-}

module Main (
  main
) where

import           Prelude
import           Relude

import           Potato.Flow.Vty.Main

import           GHC.IO.Handle
import           GHC.IO.Handle.FD
import           System.IO
import System.Directory
import System.FilePath
import System.Console.ANSI



main :: IO ()
main = mainWithDebug
--main = potatoMain
--main = layoutTestMain
--main = easyExample

-- TODO add to https://hackage.haskell.org/package/ansi-terminal-0.11
-- this won't work on Mac for whatever reason :(
pushTitleStack :: String
pushTitleStack = "\ESC[22;0t"
popTitleStack :: String
popTitleStack = "\ESC[23;0t"

mainWithDebug :: IO ()
mainWithDebug = do
  -- vty takes over stdout so this only will work with stderr
  fd <- openFile "stderr.txt" WriteMode
  hDuplicateTo fd stderr  -- redirect stdout to file
  hPutStrLn stderr "STDERR" -- will print to stderr

  -- pull/create config files
  configPath <- getXdgDirectory XdgConfig "potato"
  createDirectoryIfMissing True configPath
  doesConfigExist <- doesFileExist (configPath </> "potato.config")
  config <- if doesConfigExist
    then return () -- TODO load the config file
    else return ()
  -- TODO do stuff with config



  hPutStr stdout pushTitleStack

  flowMain

  hPutStr stdout popTitleStack
  -- TODO do this for mac
  --hSetTitle stdout ""

  hClose fd
