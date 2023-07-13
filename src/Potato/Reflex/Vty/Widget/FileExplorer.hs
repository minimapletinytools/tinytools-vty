{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Reflex.Vty.Widget.FileExplorer (
  FileExplorerWidgetConfig(..)
  , FileExplorerWidget(..)
  , holdFileExplorerWidget
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Controller
import           Potato.Flow.Vty.Attrs
import           Potato.Flow.Vty.Input
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget
import Potato.Flow.Vty.PotatoReader
import Potato.Reflex.Vty.Widget.TextInputHelpers
import           Potato.Reflex.Vty.Widget.ScrollBar



import Control.Exception (catch)
import           Control.Monad.Fix
import           Data.Align
import           Data.Dependent.Sum          (DSum ((:=>)))
import qualified Data.IntMap.Strict          as IM
import qualified Data.List                   as L
import qualified Data.Sequence               as Seq
import qualified Data.Text                   as T
import           Data.Text.Zipper
import qualified Data.Text.Zipper            as TZ
import           Data.These
import qualified System.Directory as FP
import qualified System.FilePath as FP

import qualified Graphics.Vty                as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty


fetchDirectory :: forall t m. (MonadWidget t m) => Event t FP.FilePath -> m (Event t [(Bool, FP.FilePath)])
fetchDirectory ev = let
    catchfn :: SomeException -> IO [(Bool, FP.FilePath)]
    catchfn = const (return [])
  in performEvent $ ffor ev $ \dir -> liftIO $ (flip catch catchfn) $ do
    --workingDir <- FP.getCurrentDirectory
    contents <- FP.getDirectoryContents dir
    contentsWithFolder <- forM contents $ \d -> FP.doesDirectoryExist (FP.combine dir d) >>= return . (,FP.combine dir d)
    let
      sortfn (d1,_) (d2,_) = case (d1,d2) of
        (True, False) -> LT
        (False, True) -> GT
        _ -> EQ
    return $ (L.sortBy sortfn) contentsWithFolder

data FileExplorerWidgetConfig t = FileExplorerWidgetConfig {
  _fileExplorerWidgetConfig_clickDownStyle :: Behavior t V.Attr

  -- TODO we don't need full filepath
  , _fileExplorerWidgetConfig_fileFilter :: FP.FilePath -> Bool
  , _fileExplorerWidgetConfig_initialFile :: FP.FilePath
}

data FileExplorerWidget t = FileExplorerWidget {
  -- the filename in the entry area
  _fileExplorerWidget_filename :: Behavior t Text
  -- the directory <> filename in the entry area
  , _fileExplorerWidget_fullfilename :: Behavior t FP.FilePath
  , _fileExplorerWidget_doubleClickFile :: Event t FP.FilePath -- pretty sure this is just single click for now -__-
  , _fileExplorerWidget_directory :: Dynamic t FP.FilePath
  , _fileExplorerWidget_returnOnfilename :: Event t () -- fires when you hit the "return" key in file name input area
  , _fileExplorerWidget_doubleClick :: Event t () -- fires when you double click on an existing valid file
}

data FileClick = FileClick {
    _fileClick_isDouble :: Bool
    , _fileClick_isFolder :: Bool
    , _fileClick_file :: FP.FilePath
  }
-- TODO reduce constraints, don't use HasPotato
holdFileExplorerWidget :: forall t m. (MonadLayoutWidget t m, HasPotato t m)
  => FileExplorerWidgetConfig t
  -> m (FileExplorerWidget t)
holdFileExplorerWidget FileExplorerWidgetConfig {..} = mdo

  baseStyle <- theme
  isInitialFileDir <- liftIO (FP.doesDirectoryExist _fileExplorerWidgetConfig_initialFile)

  -- select + click is one way to do choose file to save to but double click is prob better...
  --selectedDyn :: Dynamic t (Maybe Int)
  --selectedDyn <- holdDyn Nothing $ leftmost [selectEv, ]

  -- set up directory/filename text field stuff
  pb <- getPostBuild
  let
    initialDirFileEv :: Event t (FilePath, FilePath) = flip pushAlways pb $ \_ -> do
      let
        dir = FP.takeDirectory _fileExplorerWidgetConfig_initialFile
        file = FP.takeFileName _fileExplorerWidgetConfig_initialFile
      return $ if isInitialFileDir then (_fileExplorerWidgetConfig_initialFile, "") else (dir, file)
    foldDirDynFn new old = case FP.takeFileName new of
      "." -> old
      ".." -> FP.takeDirectory old
      _ -> new
  dirDyn <- foldDyn foldDirDynFn "" (leftmost [fmap fst initialDirFileEv, clickFolderEvent, setFolderEvent])
  fetchDirComplete <- fetchDirectory (updated dirDyn)
  dirContentsDyn <- holdDyn [] fetchDirComplete

  let
    -- set up directory list widget
    dirWidget filenameinentryfielddyn xs = (grout . stretch) 1 $ row $ mdo
      r <- (grout . stretch) 1 $ col $ do
        let 
          scrolledContents = ffor vScrollDyn $ \vscroll -> fmap leftmost $ forM (drop vscroll xs) $ \(isFolder, path) -> do
            (grout . fixed) 1 $ row $ do
              let
                clickable = _fileExplorerWidgetConfig_fileFilter path

                -- TODO design proper styling... (maybe prefix folders with > instead of style differently?)

              -- TODO make it so you need to click on the name???
              (singleClick', downDyn) <- singleClickWithDownState V.BLeft
              doubleClick <- doubleClickSimple 

              -- TODO highlight if matches the file entry box
              let
                styleBeh = join $ ffor2 (current filenameinentryfielddyn) (current downDyn) $ \fn d -> if d || T.unpack fn == filename then _fileExplorerWidgetConfig_clickDownStyle else baseStyle
                filename = FP.takeFileName path
                pathtext' = T.pack filename
                pathtext = if isFolder
                  then "> " <> pathtext'
                  else if clickable
                    then " *" <> pathtext'
                    else "  " <> pathtext'

              localTheme (const styleBeh) $ do
                text (constant pathtext)

              let 
                singleClick = ffilter (not . _singleClick_didDragOff) singleClick'
                makefileclick isdouble = FileClick {
                    _fileClick_isDouble = isdouble
                    , _fileClick_isFolder = isFolder
                    , _fileClick_file = path
                  }
                alignWithFn th = case th of
                  This _ -> makefileclick False
                  That _ -> makefileclick True
                  These _ _ -> makefileclick True
                fileclickev = alignWith alignWithFn singleClick (traceEvent "double" doubleClick)

              if isFolder || clickable
                then return fileclickev
                else return never
        join . fmap (switchHold never) . networkView $ scrolledContents
      let 
        vScrollWidth = 2
      vScrollDyn <- (grout . fixed) (constDyn vScrollWidth) $ col $ do
        vScrollBar vScrollWidth (constDyn (length xs))
      return r

  -- put it all together
  (clickEvent, setFolderRawEvent, filenameDyn, enterEv) <- col $ do
    -- TODO consider combining filename and directory into one...
    let
      setFileEv = fmap T.pack $ leftmost [fmap snd initialDirFileEv, fmap FP.takeFileName clickFileEvent]


    -- input for filename
    (fninputfid, (filenameDyn', enterEv')) <- (tile . fixed) 1 $ row $ do
      (grout . fixed) 10 $ text "filename"
      (tile' . stretch) 1 $ (,) <$> filenameInput "" setFileEv <*> key V.KEnter

    -- focus the filename input
    requestFocus (pb $> Refocus_Id fninputfid)

    -- input for directory
    setFolderRawEvent' <- (tile . fixed) 1 $ row $ do
      (grout . fixed) 10 $ text "directory"
      (tile . stretch) 1 $ do
        let indirev = (updated (fmap T.pack dirDyn))
        dirdyn <- filenameInput "" indirev
        return $ difference (updated $ fmap T.unpack dirdyn) indirev

    -- click in dir list event
    (clickEvent') <- (grout . stretch) 5 $ box (constant singleBoxStyle) $ do
      join . fmap (switchHold never) $ networkView (ffor dirContentsDyn (dirWidget filenameDyn'))

    return (clickEvent', setFolderRawEvent', filenameDyn', enterEv')

  -- perform the IO query to get the folder contents
  mSetFolderEvent <- performEvent $ ffor setFolderRawEvent $ \dir -> liftIO $ do
    exists <- FP.doesDirectoryExist dir
    if exists then return $ Just dir else return Nothing
  let
    setFolderEvent = fmapMaybe id mSetFolderEvent

  let
    -- TODO consider using doubleClickEvent (you might want to implement highlight selection in this case)
    maybeClickFolder fc = if _fileClick_isFolder fc && not (_fileClick_isDouble fc) then Just (_fileClick_file fc) else Nothing
    maybeClickFile fc = if not (_fileClick_isFolder fc) && not (_fileClick_isDouble fc) then Just (_fileClick_file fc) else Nothing 
    maybeDoubleClickFile fc = if not (_fileClick_isFolder fc) && _fileClick_isDouble fc then Just (_fileClick_file fc) else Nothing

    clickFolderEvent = fmapMaybe maybeClickFolder clickEvent
    clickFileEvent = fmapMaybe maybeClickFile clickEvent
    doubleClickFileEvent = fmapMaybe maybeDoubleClickFile clickEvent

    fullfilenameDyn = ffor2 dirDyn filenameDyn $ \dir fn -> FP.combine dir (T.unpack fn)

  return $ FileExplorerWidget {
      _fileExplorerWidget_filename  = current filenameDyn
      , _fileExplorerWidget_fullfilename = current fullfilenameDyn
      , _fileExplorerWidget_doubleClickFile = clickFileEvent
      , _fileExplorerWidget_directory = dirDyn
      , _fileExplorerWidget_returnOnfilename = void enterEv
      , _fileExplorerWidget_doubleClick = void doubleClickFileEvent
    }
