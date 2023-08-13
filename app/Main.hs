{-# OPTIONS_GHC -threaded #-}

module Main (
  main
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.TestStates
import           Potato.Flow.TutorialState
import           Potato.Flow.Vty.Main
import qualified Paths_tinytools_vty (version)



import           Control.Exception                    (try)
import           GHC.IO.Handle
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.IO                            hiding (putStrLn)
import Data.Version (showVersion)
import qualified Data.Text as T

import qualified Graphics.Vty                         as V
import qualified Graphics.Vty.UnicodeWidthTable.IO    as V
import qualified Graphics.Vty.UnicodeWidthTable.Query as V




data PotatoCLIOptions = PotatoCLIOptions {
  _potatoCLIOptions_args                        :: [String]
  , _potatoCLIOptions_empty                     :: Bool
  , _potatoCLIOptions_generateUnicodeWidthTable :: Bool
}


sample :: Parser PotatoCLIOptions
sample = PotatoCLIOptions
  <$> many (argument str (metavar "FILE"))
  <*> switch
    ( long "empty"
    <> short 'e'
    <> help "open an empty document" )
  <*> switch
    ( long "widthtable"
    <> help "generate unicode width table for your terminal using vty" )



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

  if _potatoCLIOptions_generateUnicodeWidthTable opts
    then do
      mTermName <- V.currentTerminalName
      case mTermName of
        Just termName -> do
          configdir <- tinytoolsConfigDir
          let
            fn = configdir </> (termName <> "_termwidthfile")
          rslt <- try $ do
            wt <- V.buildUnicodeWidthTable V.defaultUnicodeTableUpperBound
            createDirectoryIfMissing False configdir
            V.writeUnicodeWidthTable fn wt
          case rslt of
            Right _ -> do
              putStrLn "\n"
              putStrLn $ "successfully wrote " <> fn
              exitSuccess
            Left (SomeException e) -> do
              putStrLn "\n"
              die $ "failed to generate or write unicode width table " <> fn <> " with exception " <> show e
        Nothing -> do
          die "failed to generate unicode width table because could not obtain terminal name"

    else return ()

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
      , _mainPFWidgetConfig_showWelcome = True
      , _mainPFWidgetConfig_version_tinytools_vty = T.pack $ showVersion Paths_tinytools_vty.version
    }

  -- set the title
  hPutStr stdout pushTitleStack

  potatoMainWidget $ mainPFWidget config

  -- unset the title
  hPutStr stdout popTitleStack
  -- TODO do this for mac
  --hSetTitle stdout ""

  hClose fd
