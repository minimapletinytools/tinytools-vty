{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.OpenWindow where

import           Relude

import           Potato.Flow
import           Potato.Flow.Vty.Attrs
import           Potato.Flow.Vty.Common
import           Potato.Flow.Vty.PotatoReader
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget.FileExplorer
import           Potato.Reflex.Vty.Widget.Popup


import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Data.Align
import           Data.Char                             (isNumber)
import           Data.Dependent.Sum                    (DSum ((:=>)))
import qualified Data.IntMap                           as IM
import qualified Data.List.Extra                       as L
import qualified Data.Maybe
import qualified Data.Sequence                         as Seq
import qualified Data.Text                             as T
import qualified Data.Text.Zipper                      as TZ
import           Data.These
import           Data.Tuple.Extra

import qualified Graphics.Vty                          as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

import qualified System.Directory                      as FP
import qualified System.FilePath                       as FP

data OpenWindowConfig t = OpenWindowConfig {
  _openWindowConfig_open :: Event t FP.FilePath -- ^ Event to launch the popup window to open file starting in the given directory
}

popupOpenWindow :: forall t m. (MonadWidget t m, HasPotato t m)
  => OpenWindowConfig t
  -> m (Event t FP.FilePath, Dynamic t Bool) -- ^ (file to open, popup state)
popupOpenWindow OpenWindowConfig {..} = do

  potatostylebeh <- fmap _potatoConfig_style askPotato

  let

    popupOpenEv = ffor _openWindowConfig_open $ \d0 -> mdo
      boxTitle (constant def) "Open" $ do
        initManager_ $ col $ mdo
          fewidget <- (tile . stretch) 3 $ holdFileExplorerWidget $ FileExplorerWidgetConfig {
              _fileExplorerWidgetConfig_fileFilter = \fp -> FP.takeExtension fp == kTinyToolsFileExtension
              , _fileExplorerWidgetConfig_initialFile = d0
              , _fileExplorerWidgetConfig_clickDownStyle = fmap _potatoStyle_layers_softSelected potatostylebeh
            }
          (cancelEv, openButtonEv) <- (tile . fixed) 3 $ row $ do
            cancelEv' <- (tile . stretch) 10 $ textButton def "cancel"

            -- TODO grey out if filename is empty
            openEv' <- (tile . stretch) 10 $ textButton def "open"

            return (cancelEv', openEv')

          let
            -- do we really want to allow open on pressing enter?
            openEv'' = leftmost [_fileExplorerWidget_returnOnfilename fewidget, _fileExplorerWidget_doubleClick fewidget, openButtonEv]
            openEv' = tag (_fileExplorerWidget_fullfilename fewidget) openEv''
            openEv = fmap addTinyToolsFileExtensionIfNecessary openEv'

          return (cancelEv, openEv)
    fmapfn w = \escEv clickOutsideEv -> fmap (\(cancelEv, outputEv) -> (leftmost [escEv, cancelEv, void outputEv], outputEv)) w

  localTheme (const $ fmap _potatoStyle_normal potatostylebeh) $ do
    popupPane def $ (fmap fmapfn popupOpenEv)

