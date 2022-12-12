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

  potatostylebeh <- fmap _potatoConfig_style askPotato

  let

    extension = ".potato"
    modifyFileNameFn fp = if FP.takeExtension fp == ""
      then fp <> extension
      else fp

    popupSaveAsEv = ffor _saveAsWindowConfig_saveAs $ \f0 -> mdo
      boxTitle (constant def) "Save As" $ do
        initManager_ $ col $ mdo
          fewidget <- (tile . stretch) 3 $ holdFileExplorerWidget $ FileExplorerWidgetConfig {
              _fileExplorerWidgetConfig_fileFilter = \fp -> FP.takeExtension fp == extension
              , _fileExplorerWidgetConfig_initialFile = f0
              , _fileExplorerWidgetConfig_clickDownStyle = fmap _potatoStyle_layers_softSelected potatostylebeh
            }
          (cancelEv, saveButtonEv) <- (tile . fixed) 3 $ row $ do
            cancelEv' <- (tile . stretch) 10 $ textButton def "cancel"

            -- TODO grey out if filename is empty
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
            -- TODO 
            saveEv' = leftmost [_fileExplorerWidget_returnOnfilename fewidget, saveButtonEv]

            -- TODO test
            -- only save if filename is non-empty
            saveEv = gate (fmap (not . T.null) (_fileExplorerWidget_filename fewidget)) saveEv'

            saveAsFileEv' = tag (_fileExplorerWidget_fullfilename fewidget) saveEv
            saveAsFileEv = fmap modifyFileNameFn saveAsFileEv'
          return (cancelEv, saveAsFileEv)
    fmapfn w = \escEv clickOutsideEv -> fmap (\(cancelEv, outputEv) -> (leftmost [escEv, cancelEv, void outputEv], outputEv)) w

  localTheme (const $ fmap _potatoStyle_normal potatostylebeh) $ do
    popupPane def $ (fmap fmapfn popupSaveAsEv)


data SaveBeforeExitConfig t = SaveBeforeExitConfig {
  _saveBeforeExitConfig_exitWithChanges :: Event t ()
}

-- TODO make this generic, via some PopupManager thingy or what not
-- you want to use the same NeedSave for close and open when there are unsaved changes
-- and after the save action, you want to redirect back to the open or quit operation
data SaveBeforeExitOutput t = SaveBeforeExitOutput {

  -- TODO you should be able to get this to work...
  --_saveBeforeExitOutput_save :: Event t FP.FilePath
  _saveBeforeExitOutput_save :: Event t ()

  , _saveBeforeExitOutput_saveAs :: Event t ()
  , _saveBeforeExitOutput_quit :: Event t ()
}

hackAlign3 :: (Reflex t) => Event t a -> Event t b -> Event t c -> Event t (These a (These b c))
hackAlign3 a b c = align a (align b c)

hackFanThese3 :: (Reflex t) =>  Event t (These a (These b c)) -> (Event t a, Event t b, Event t c)
hackFanThese3 ev = r where
  (a, bc) = fanThese ev
  (b, c) = fanThese bc
  r = (a,b,c)

-- TODO somehow allow auto save on exit
popupSaveBeforeExit :: forall t m. (MonadWidget t m, HasPotato t m)
  => SaveBeforeExitConfig t
  -> m (SaveBeforeExitOutput t, Dynamic t Bool)
popupSaveBeforeExit SaveBeforeExitConfig {..} = do
  mopenfilebeh <- fmap _potatoConfig_appCurrentOpenFile askPotato

  -- TODO unsure why this doesn't get resampled each time popup is created :(
  --mopenfile <- sample mopenfilebeh

  -- TODO style everything
  let
    popupSaveBeforeExitEv = ffor _saveBeforeExitConfig_exitWithChanges $ \f0 -> mdo
      boxTitle (constant def) "You have unsaved changes. Would you like to save?" $ do
        initManager_ $ col $ mdo
          (quitEv, cancelEv, saveButtonEv, saveAsButtonEv) <- do
            (tile . stretch) 0 $ col $ return ()
            (tile . fixed) 3 $ row $ do
              cancelEv' <- (tile . stretch) 9 $ textButton def "cancel"

              -- TODO you should be able to get this to work...
              --saveEv' <- case mopenfile of
              --  Nothing -> return never
              --  Just x -> (tile . stretch) 9 $ (const x <<$>> textButton def "save")
              saveEv' <- (tile . stretch) 9 $ textButton def "save"

              saveAsEv' <- (tile . stretch) 9 $ textButton def "save as"
              quitEv' <- (tile . stretch) 9 $ textButton def "quit"
              return (quitEv', cancelEv', saveEv', saveAsEv')
          return (cancelEv, hackAlign3 saveButtonEv saveAsButtonEv quitEv)
    fmapfn w = \escEv clickOutsideEv -> fmap (\(cancelEv, outputEv) -> (leftmost [escEv, cancelEv, void outputEv], outputEv)) w
  (outputEv, stateDyn) <- popupPane def $ (fmap fmapfn popupSaveBeforeExitEv)
  return (uncurry3 SaveBeforeExitOutput (hackFanThese3 outputEv), stateDyn)
