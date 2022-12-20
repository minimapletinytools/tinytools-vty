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
    popupSaveAsEv = ffor _saveAsWindowConfig_saveAs $ \f0 -> mdo
      boxTitle (constant def) "Save As" $ do
        initManager_ $ col $ mdo
          fewidget <- (tile . stretch) 3 $ holdFileExplorerWidget $ FileExplorerWidgetConfig {
              _fileExplorerWidgetConfig_fileFilter = \fp -> FP.takeExtension fp == kTinyToolsFileExtension
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
            -- do we really want to allow save on pressing enter?
            saveEv' = leftmost [_fileExplorerWidget_returnOnfilename fewidget, saveButtonEv]

            -- only save if filename is non-empty
            saveEv = gate (fmap (not . T.null) (_fileExplorerWidget_filename fewidget)) saveEv'

            saveAsFileEv' = tag (_fileExplorerWidget_fullfilename fewidget) saveEv
            saveAsFileEv = fmap addTinyToolsFileExtensionIfNecessary saveAsFileEv'
          return (cancelEv, saveAsFileEv)
    fmapfn w = \escEv clickOutsideEv -> fmap (\(cancelEv, outputEv) -> (leftmost [escEv, cancelEv, void outputEv], outputEv)) w

  localTheme (const $ fmap _potatoStyle_normal potatostylebeh) $ do
    popupPane def $ (fmap fmapfn popupSaveAsEv)


-- TODO rename to MaybeSaveBeforeAction...
data SaveBeforeActionConfig t = SaveBeforeActionConfig {
  _saveBeforeActionConfig_unsavedChangesBeh :: Behavior t Bool
  , _saveBeforeActionConfig_open :: Event t ()
  , _saveBeforeActionConfig_new :: Event t ()
  , _saveBeforeActionConfig_exit :: Event t ()
}

data SaveBeforeActionType = SaveBeforeActionType_Open | SaveBeforeActionType_New | SaveBeforeActionType_Exit | SaveBeforeActionType_None deriving (Show, Eq)

-- TODO make this generic, via some PopupManager thingy or what not
-- you want to use the same NeedSave for close and open when there are unsaved changes
-- and after the save action, you want to redirect back to the open or quit operation
data SaveBeforeActionOutput t = SaveBeforeActionOutput {

  -- TODO you should be able to get this to work...
  --_saveBeforeActionOutput_save :: Event t FP.FilePath
  _saveBeforeActionOutput_save :: Event t ()

  , _saveBeforeActionOutput_saveAs :: Event t ()

  , _saveBeforeActionOutput_new :: Event t ()
  , _saveBeforeActionOutput_open :: Event t ()
  , _saveBeforeActionOutput_exit :: Event t ()
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
  => SaveBeforeActionConfig t
  -> m (SaveBeforeActionOutput t, Dynamic t Bool)
popupSaveBeforeExit SaveBeforeActionConfig {..} = do
  mopenfilebeh <- fmap _potatoConfig_appCurrentOpenFile askPotato
  -- TODO unsure why this doesn't get resampled each time popup is created :(
  --mopenfile <- sample mopenfilebeh

  let
    
    combined = leftmost [SaveBeforeActionType_Open <$_saveBeforeActionConfig_open, SaveBeforeActionType_New <$_saveBeforeActionConfig_new, SaveBeforeActionType_Exit <$_saveBeforeActionConfig_exit]
    filteredEv = gate _saveBeforeActionConfig_unsavedChangesBeh combined
    skipEv = gate (fmap not _saveBeforeActionConfig_unsavedChangesBeh) combined


  potatostylebeh <- fmap _potatoConfig_style askPotato

  let
    popupSaveBeforeExitEv = ffor filteredEv $ \iev -> mdo
      boxTitle (constant def) "You have unsaved changes. Would you like to save?" $ do
        initManager_ $ col $ mdo
          (outEv, cancelEv, saveButtonEv, saveAsButtonEv) <- do
            (tile . stretch) 0 $ col $ return ()
            (tile . fixed) 3 $ row $ do
              cancelEv' <- (tile . stretch) 9 $ textButton def "cancel"

              -- TODO you should be able to get this to work...
              --saveEv' <- case mopenfile of
              --  Nothing -> return never
              --  Just x -> (tile . stretch) 9 $ (const x <<$>> textButton def "save")
              saveEv' <- (tile . stretch) 9 $ textButton def "save"

              saveAsEv' <- (tile . stretch) 9 $ textButton def "save as"
              outEv' <- (tile . stretch) 9 $ textButton def "ignore"
              return (outEv' $> iev, cancelEv', saveEv', saveAsEv')
          return (cancelEv, hackAlign3 saveButtonEv saveAsButtonEv outEv)
    fmapfn w = \escEv clickOutsideEv -> fmap (\(cancelEv, outputEv) -> (leftmost [escEv, cancelEv, void outputEv], outputEv)) w
  (outputEv, stateDyn) <- localTheme (const $ fmap _potatoStyle_normal potatostylebeh) $ do
    popupPane def $ (fmap fmapfn popupSaveBeforeExitEv)
  let 
    (saveEv, saveAsEv, doneEv) = hackFanThese3 outputEv

    bothEv = leftmost [skipEv, doneEv]

    sbao = SaveBeforeActionOutput {
        _saveBeforeActionOutput_save = saveEv
        , _saveBeforeActionOutput_saveAs = saveAsEv
        , _saveBeforeActionOutput_new = void $ ffilter (== SaveBeforeActionType_New) bothEv
        , _saveBeforeActionOutput_open = void $ ffilter (== SaveBeforeActionType_Open) bothEv
        , _saveBeforeActionOutput_exit = void $ ffilter (== SaveBeforeActionType_Exit) bothEv
      }


  return (sbao, stateDyn)
