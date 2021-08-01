{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.SaveAsWindow (
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Vty.Common
import           Potato.Reflex.Vty.Helpers
import Potato.Flow.Vty.PotatoReader
import Potato.Flow.Vty.Attrs
import Potato.Reflex.Vty.Widget.TextInputHelpers
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

data SaveAsWindowConfig t = SaveAsWindowConfig {
  _saveAsWindowConfig_saveAs :: Event t Text -- ^ Text is previous file name or empty string
}

-- UNTESTED
popupSaveAsWindow :: forall t m. (MonadWidget t m, HasPotato t m)
  => SaveAsWindowConfig t
  -> m (Event t FP.FilePath, Dynamic t Bool) -- ^ (file to save to, popup state)
popupSaveAsWindow SaveAsWindowConfig {..} = do
  let
    popupSaveAsEv = ffor _saveAsWindowConfig_saveAs $ \filename -> do
      undefined

    fmapfn w = \escEv clickOutsideEv -> fmap (\outputEv -> (leftmost [escEv, void outputEv], outputEv)) w
  popupPane def (fmap fmapfn popupSaveAsEv)
