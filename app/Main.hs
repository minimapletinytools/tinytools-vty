{-# OPTIONS_GHC -threaded #-}

module Main (
  main
) where

import           Prelude
import           Relude

import           Potato.Flow.Vty.Main
import           Potato.Flow
import           Potato.Flow.TestStates
import           Potato.Flow.TutorialState


import           GHC.IO.Handle
import           GHC.IO.Handle.FD
import           System.IO
import System.Directory
import           Options.Applicative



data PotatoCLIOptions = PotatoCLIOptions {
  _potatoCLIOptions_args :: [String]
  , _potatoCLIOptions_empty :: Bool
}

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser PotatoCLIOptions
sample = PotatoCLIOptions
  <$> many (argument str (metavar "FILE"))
  <*> switch
    ( long "empty"
    <> short 'e'
    <> help "open an empty document" )


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
  let 
    optsparser = info (sample <**> helper)
      ( fullDesc
      <> progDesc "optionally enter the filename you'd like to open"
      <> header "tinytools-vty: an ASCII diagram editor" )


  opts <- execParser optsparser

  -- vty takes over stdout so this only will work with stderr
  fd <- openFile "stderr.txt" WriteMode
  hDuplicateTo fd stderr  -- redirect stdout to file
  hPutStrLn stderr "STDERR" -- will print to stderr

  -- pull/create config files
  -- TODO finish config file stuff
  --configPath <- getXdgDirectory XdgConfig "potato"
  --createDirectoryIfMissing True configPath
  --doesConfigExist <- doesFileExist (configPath </> "potato.config")


  -- see if the argument file we passed in exists or not
  minitfile <- if _potatoCLIOptions_empty opts
    then
      return Nothing
    else case nonEmpty (_potatoCLIOptions_args opts) of
      Nothing -> return Nothing
      Just (x:|_) -> do
        exists <- doesFileExist x
        return $ if exists then Just x else Nothing

  homeDir <- getHomeDirectory

  let
    config = MainPFWidgetConfig {
      _mainPFWidgetConfig_initialFile = minitfile
      , _mainPFWidgetConfig_homeDirectory = homeDir
      , _mainPFWidgetConfig_initialState = if _potatoCLIOptions_empty opts
        then (owlpfstate_newProject, emptyControllerMeta)
        -- TODO load tutorial here owlpfstate_tutorial
        --else owlpfstate_attachments1
        else tutorialState
    }

  -- set the title
  hPutStr stdout pushTitleStack

  potatoMainWidget $ mainPFWidget config

  -- unset the title
  hPutStr stdout popTitleStack
  -- TODO do this for mac
  --hSetTitle stdout ""

  hClose fd
