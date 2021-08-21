{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Alert where

import           Relude

import           Potato.Flow
import           Potato.Flow.Vty.Common
import           Potato.Reflex.Vty.Helpers
import Potato.Flow.Vty.PotatoReader
import Potato.Flow.Vty.Attrs
import Potato.Reflex.Vty.Widget.FileExplorer
import Potato.Reflex.Vty.Widget.Popup


import           Control.Monad.Fix
import qualified Data.Text                         as T

import qualified Graphics.Vty                      as V
import           Reflex
import           Reflex.Potato.Helpers
import           Reflex.Vty

-- UNTESTED
popupAlert :: forall t m. (MonadWidget t m, HasPotato t m)
  => Event t Text
  -> m (Dynamic t Bool) -- ^ (file to save to, popup state)
popupAlert alertEv = do
  -- TODO style
  let
    fmapfn alert = \escEv clickOutsideEv -> do
      okEv <- boxTitle (constant def) "😱ALERT😱" $ do
        initLayout $ col $ do
          (grout . stretch) 1 $ text (constant alert)
          (grout . fixed) 3 $ textButton def (constant "OK")
      return (leftmost [escEv, okEv], never)
  fmap snd $ popupPane def $ (fmap fmapfn alertEv)
