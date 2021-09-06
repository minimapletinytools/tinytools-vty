
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Layer (
  LayerWidgetConfig(..)
  , LayerWidget(..)
  , holdLayerWidget
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Controller
import           Potato.Flow.Vty.Attrs
import           Potato.Flow.Vty.Input
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget
import Potato.Flow.Vty.PotatoReader


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

import qualified Graphics.Vty                as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

--moveChar :: Char
--moveChar = '≡'
hiddenChar :: Char
hiddenChar = '-'
visibleChar :: Char
visibleChar = 'e'
lockedChar :: Char
lockedChar = '@'
unlockedChar :: Char
unlockedChar = 'a'
expandChar :: Char
expandChar = '»'
closeChar :: Char
closeChar = '«'

{-# INLINE if' #-}
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y


data LayerWidgetConfig t = LayerWidgetConfig {
  _layerWidgetConfig_layers    :: Dynamic t LayersState
  , _layerWidgetConfig_layersView    :: Dynamic t LayersViewHandlerRenderOutput
  , _layerWidgetConfig_selection :: Dynamic t Selection
}

data LayerWidget t = LayerWidget {
  _layerWidget_mouse :: Event t LMouseData
}

holdLayerWidget :: forall t m. (MonadWidget t m, HasPotato t m)
  => LayerWidgetConfig t
  -> m (LayerWidget t)
holdLayerWidget LayerWidgetConfig {..} = do

  potatostylebeh <- fmap _potatoConfig_style askPotato
  PotatoStyle {..} <- sample potatostylebeh

  regionWidthDyn <- displayWidth
  regionHeightDyn <- displayHeight


  let
    padTop = 0
    padBottom = 0
    regionDyn = ffor2 regionWidthDyn regionHeightDyn (,)

  -- ::actually draw images::
  let

    makeLayerImage :: Int -> LayersHandlerRenderEntry -> V.Image
    makeLayerImage width lhrentry = case lhrentry of
      LayersHandlerRenderEntryDummy ident -> r where
        r = V.text' lg_layer_selected . T.pack . L.take width
          $ replicate ident ' '
          <> replicate 10 '*'
      LayersHandlerRenderEntryNormal selected mdots lentry@LayerEntry{..} -> r where
        ident = layerEntry_depth lentry
        sowl = _layerEntry_superOwl
        rid = _superOwl_id sowl
        label = isOwl_name sowl
        
        attr = case selected of
          LHRESS_Selected -> _potatoStyle_selected
          LHRESS_InheritSelected -> _potatoStyle_selected
          LHRESS_ChildSelected -> _potatoStyle_softSelected
          _ -> _potatoStyle_normal

        identn = case mdots of
          Nothing -> ident
          Just x -> x

        r = V.text' attr . T.pack . L.take width $

          -- render identation and possible drop depth
          replicate identn ' '
          <> replicate (min 1 (ident - identn)) '|'
          <> replicate (max 0 (ident - identn - 1)) ' '

          -- <> [moveChar]
          <> if' (layerEntry_isFolder lentry) (if' _layerEntry_isCollapsed [expandChar] [closeChar]) []
          <> if' (lockHiddenStateToBool _layerEntry_hideState) [hiddenChar] [visibleChar]
          <> if' (lockHiddenStateToBool _layerEntry_lockState) [lockedChar] [unlockedChar]
          <> " "
          <> show rid
          <> " "
          <> T.unpack label
    layerImages :: Behavior t [V.Image]
    layerImages = current $ fmap ((:[]) . V.vertCat)
      $ ffor2 regionDyn (fmap _layersViewHandlerRenderOutput_entries _layerWidgetConfig_layersView) $ \(w,h) lhrentries ->
        map (makeLayerImage w) . L.take (max 0 (h - padBottom)) $ toList lhrentries
  tellImages layerImages
  let
    -- TODO scrolling?
    offset = V2 0 0
  inp <- makeLMouseDataInputEv offset True
  return $ LayerWidget inp
