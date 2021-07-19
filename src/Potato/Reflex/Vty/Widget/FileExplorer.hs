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
  forM contents $ \d -> FP.doesDirectoryExist (FP.combine dir d) >>= return . (,FP.combine dir d)

data FileExplorerWidgetConfig t = FileExplorerWidgetConfig {
  -- TODO styling vars

  -- TODO we don't need full filepath
  _fileExplorerWidgetConfig_fileFilter :: FP.FilePath -> Bool
  , _fileExplorerWidgetConfig_initialDirectory :: FP.FilePath
}

-- WIP TODO FINISH
data FileExplorerWidget t = FileExplorerWidget {
  _fileExplorerWidget_doubleClickFile :: Event t FP.FilePath
  , _fileExplorerWidget_directory :: Dynamic t FP.FilePath
}

holdFileExplorerWidget :: forall t m. (MonadWidget t m)
  => FileExplorerWidgetConfig t
  -> m (FileExplorerWidget t)
holdFileExplorerWidget FileExplorerWidgetConfig {..} = mdo

  -- set up v scrolling stuff
  kup <- key V.KUp
  kdown <- key V.KDown
  m <- mouseScroll
  let
    requestedScroll :: Event t Int
    requestedScroll = leftmost
      [ 1 <$ kdown
      , (-1) <$ kup
      , ffor m $ \case
          ScrollDirection_Up -> (-1)
          ScrollDirection_Down -> 1
      ]
    updateLine maxN delta ix = min (max 0 (ix + delta)) maxN
    scrollEv = leftmost [
        attach (length <$> (current dirContentsDyn)) requestedScroll
        -- quick hack to reset scroll after changing folders
        , (updated dirContentsDyn $> (1,0))
      ]
  vScrollDyn :: Dynamic t Int <- foldDyn (\(maxN, delta) ix -> updateLine (maxN - 1) delta ix) 0 scrollEv

  -- select + click is one way to do choose file to save to but double click is prob better...
  --selectedDyn :: Dynamic t (Maybe Int)
  --selectedDyn <- holdDyn Nothing $ leftmost [selectEv, ]

  -- set up directory stuff
  pb <- getPostBuild
  let
    foldDirDynFn new old = case new of
      "." -> old
      ".." -> FP.takeDirectory old
      _ -> new
  dirDyn <- foldDyn foldDirDynFn "" (leftmost [pb $> _fileExplorerWidgetConfig_initialDirectory, clickFolderEvent, setFolderEvent])
  fetchDirComplete <- fetchDirectory (updated dirDyn)
  dirContentsDyn <- holdDyn [] fetchDirComplete

  let
    dirWidget vscroll xs = forM (drop vscroll xs) $ \(isFolder, path) -> do
      (grout . fixed) 1 $ do
        let
          clickable = _fileExplorerWidgetConfig_fileFilter path
          style = if isFolder || clickable
            -- TODO
            then V.defAttr
            else V.defAttr
        text (constant $ T.pack (FP.takeFileName path ))
        -- TODO double click
        click' <- singleClick V.BLeft
        let click = ffilter (not . _singleClick_didDragOff) click'
        if isFolder
          then return $ (click $> Left path)
          else if clickable
            then return $ (click $> Right path)
            else return never

  (clickEvents, setFolderRawEvent) <- initLayout $ col $ do
    setFolderRawEvent' <- (grout . fixed) 3 $ box (constant singleBoxStyle) $ do
      -- one of TZ inputs should be (updated dirDyn)
      text (constant "TODO INPUT BOX GOES HERE")
      -- TODO set folder event via text input
      return (never :: Event t FP.FilePath)
    clickEvents' <- networkView (ffor2 (traceDyn "poop" vScrollDyn) dirContentsDyn dirWidget)
    return (clickEvents', setFolderRawEvent')

  mSetFolderEvent <- performEvent $ ffor setFolderRawEvent $ \dir -> liftIO $ do
    exists <- FP.doesDirectoryExist dir
    if exists then return $ Just dir else return Nothing
  let
    setFolderEvent = fmapMaybe id mSetFolderEvent


  clickEvent :: Event t (Either FP.FilePath FP.FilePath) <- switchHold never $ (fmap leftmost clickEvents)

  let
    maybeLeft (Left a) = Just a
    maybeLeft _ = Nothing
    maybeRight (Right a) = Just a
    maybeRight _ = Nothing
    clickFolderEvent = fmapMaybe maybeLeft (traceEvent "click2" clickEvent)
    clickFileEvent = fmapMaybe maybeRight clickEvent

  return $ FileExplorerWidget {
      _fileExplorerWidget_doubleClickFile = clickFileEvent
      , _fileExplorerWidget_directory = dirDyn
    }
