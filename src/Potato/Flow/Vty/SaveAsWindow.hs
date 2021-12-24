-- TODO rename this file to PopupDialogs or something
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.SaveAsWindow where

import           Relude

import           Potato.Flow
import           Potato.Flow.Vty.Common
import           Potato.Reflex.Vty.Helpers
import Potato.Flow.Vty.PotatoReader
import Potato.Flow.Vty.Attrs
import Potato.Reflex.Vty.Widget.FileExplorer
import Potato.Reflex.Vty.Widget.Popup


import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Data.Align
import           Data.Char                         (isNumber)
import           Data.Dependent.Sum                (DSum ((:=>)))
import qualified Data.IntMap                       as IM
import qualified Data.List.Extra                   as L
import qualified Data.Maybe
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as TZ
import           Data.These
import           Data.Tuple.Extra

import qualified Graphics.Vty                      as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

import qualified System.FilePath as FP
import qualified System.Directory as FP

data SaveAsWindowConfig t = SaveAsWindowConfig {
  _saveAsWindowConfig_saveAs :: Event t FP.FilePath -- ^ Event to launch the popup window to save file as Text is previous file name or empty string
}

popupSaveAsWindow :: forall t m. (MonadWidget t m, HasPotato t m)
  => SaveAsWindowConfig t
  -> m (Event t FP.FilePath, Dynamic t Bool) -- ^ (file to save to, popup state)
popupSaveAsWindow SaveAsWindowConfig {..} = do
  -- TODO style everything
  let
    popupSaveAsEv = ffor _saveAsWindowConfig_saveAs $ \f0 -> mdo
      boxTitle (constant def) "Save As" $ do
        initManager_ $ col $ mdo
          fewidget <- (tile . stretch) 3 $ holdFileExplorerWidget $ FileExplorerWidgetConfig {
              _fileExplorerWidgetConfig_fileFilter = \fp -> FP.takeExtension fp == ".potato"
              , _fileExplorerWidgetConfig_initialFile = f0
            }
          (cancelEv, saveButtonEv) <- (tile . fixed) 3 $ row $ do
            cancelEv' <- (tile . stretch) 10 $ textButton def "cancel"
            saveEv' <- (tile . stretch) 10 $ textButton def "save"
            return (cancelEv', saveEv')
          -- DELETE
          -- IO file validity checkin
          {-mSaveAsFileEv <- performEvent $ ffor (tag (_fileExplorerWidget_fullfilename fewidget) saveEv) $ \ffn -> liftIO $ do
            exists <- FP.doesFileExist ffn
            return $ if exists
              then Just ffn else Nothing
          let saveAsFileEv = fmapMaybe id mSaveAsFileEv-}
          let
            saveEv = leftmost [_fileExplorerWidget_returnOnfilename fewidget, saveButtonEv]
            saveAsFileEv = tag (_fileExplorerWidget_fullfilename fewidget) saveEv
          return (cancelEv, saveAsFileEv)
    fmapfn w = \escEv clickOutsideEv -> fmap (\(cancelEv, outputEv) -> (leftmost [escEv, cancelEv, void outputEv], outputEv)) w
  popupPane def $ (fmap fmapfn popupSaveAsEv)


data SaveBeforeExitConfig t = SaveBeforeExitConfig {
  _saveBeforeExitConfig_exitWithChanges :: Event t ()
}
data SaveBeforeExitOutput t = SaveBeforeExitOutput {
  _saveBeforeExitOutput_save :: Event t FP.FilePath
  , _saveBeforeExitOutput_saveAs :: Event t ()
}

-- TODO somehow allow auto save on exit
popupSaveBeforeExit :: forall t m. (MonadWidget t m, HasPotato t m)
  => SaveBeforeExitConfig t
  -> m (SaveBeforeExitOutput t, Dynamic t Bool)
popupSaveBeforeExit SaveBeforeExitConfig {..} = do
  mopenfilebeh <- fmap _potatoConfig_appCurrentOpenFile askPotato
  mopenfile <- sample mopenfilebeh

  -- TODO style everything
  let
    popupSaveBeforeExitEv = ffor _saveBeforeExitConfig_exitWithChanges $ \f0 -> mdo
      boxTitle (constant def) "You have unsaved changes. Would you like to save?" $ do
        initManager_ $ col $ mdo
          (cancelEv, saveButtonEv, saveAsButtonEv) <- (tile . fixed) 3 $ row $ do
            cancelEv' <- (tile . stretch) 10 $ textButton def "cancel"
            saveEv' <- case mopenfile of
              Nothing -> return never
              Just x -> (tile . stretch) 10 $ (const x <<$>> textButton def "save")
            saveAsEv' <- (tile . stretch) 10 $ textButton def "save as"
            return (cancelEv', saveEv', saveAsEv')
          return (cancelEv, align saveButtonEv saveAsButtonEv)
    fmapfn w = \escEv clickOutsideEv -> fmap (\(cancelEv, outputEv) -> (leftmost [escEv, cancelEv, void outputEv], outputEv)) w
  (outputEv, stateDyn) <- popupPane def $ (fmap fmapfn popupSaveBeforeExitEv)
  return (uncurry SaveBeforeExitOutput (fanThese outputEv), stateDyn)
