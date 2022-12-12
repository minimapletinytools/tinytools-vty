
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
import Potato.Flow.Vty.Common
import Potato.Reflex.Vty.Widget.ScrollBar


import qualified Potato.Data.Text.Zipper
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



-- | simple conversion function
-- potato-flow does not want to depend on reflex an has a coppy of TextZipper library but they are pretty much the same
coerceZipper :: Potato.Data.Text.Zipper.TextZipper -> TZ.TextZipper
coerceZipper (Potato.Data.Text.Zipper.TextZipper a b c d) = TZ.TextZipper a b c d

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
closeChar = '⇊'

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
  , _layerWidget_newFolderEv :: Event t ()
}

layerContents :: forall t m. (MonadWidget t m, HasPotato t m)
  => LayerWidgetConfig t
  -> Dynamic t (Int, Int)
  -> m (Event t LMouseData)
layerContents LayerWidgetConfig {..} scrollDyn = do


  potatostylebeh <- fmap _potatoConfig_style askPotato
  PotatoStyle {..} <- sample potatostylebeh

  regionWidthDyn <- displayWidth
  regionHeightDyn <- displayHeight


  let
    padBottom = 0
    listRegionDyn = ffor2 regionWidthDyn regionHeightDyn (,)


    makeLayerImage :: Int -> LayersHandlerRenderEntry -> V.Image
    makeLayerImage width lhrentry = case lhrentry of
      LayersHandlerRenderEntryDummy ident -> r where
        r = V.text' lg_layer_selected . T.pack . L.take width
          $ replicate ident ' '
          <> replicate 10 '*'
      LayersHandlerRenderEntryNormal selected mdots mrenaming lentry@LayerEntry{..} -> r where
        ident = layerEntry_depth lentry
        sowl = _layerEntry_superOwl
        rid = _superOwl_id sowl
        label = hasOwlItem_name sowl

        attr = case selected of
          LHRESS_Selected -> _potatoStyle_selected
          LHRESS_InheritSelected -> _potatoStyle_selected
          LHRESS_ChildSelected -> _potatoStyle_layers_softSelected
          _ -> _potatoStyle_normal

        -- TODO correct styles so they aren't confused with selected styles (you should add colors)
        attrrenamingbg = _potatoStyle_layers_softSelected
        attrrenamingcur = _potatoStyle_selected

        identn = case mdots of
          Nothing -> ident
          Just x -> x - 1

        t1 = V.text' attr . T.pack $

          -- render identation and possible drop depth
          replicate identn ' '
          <> replicate (min 1 (ident - identn)) '|'
          <> replicate (max 0 (ident - identn - 1)) ' '

          -- render folder hide lock icons
          -- <> [moveChar]
          <> if' (layerEntry_isFolder lentry) (if' _layerEntry_isCollapsed [expandChar] [closeChar]) []
          <> if' (lockHiddenStateToBool _layerEntry_hideState) [hiddenChar] [visibleChar]
          <> if' (lockHiddenStateToBool _layerEntry_lockState) [lockedChar] [unlockedChar]
          <> " "
          <> show rid
          <> " "

        t2 = case mrenaming of
          Nothing -> V.text' attr label
          Just renaming -> img where
            dls = TZ.displayLines 999999 attrrenamingbg attrrenamingcur (coerceZipper renaming)
            img = V.vertCat . images $ TZ._displayLines_spans dls

        r = t1 V.<|> t2

    layerImages :: Behavior t [V.Image]
    layerImages = current $ fmap ((:[]) . V.vertCat)
      $ ffor3 listRegionDyn (fmap _layersViewHandlerRenderOutput_entries _layerWidgetConfig_layersView) scrollDyn $ \(w,h) lhrentries scroll ->
        map (makeLayerImage w) . L.take (max 0 (h - padBottom)) . L.drop (snd scroll) $ toList lhrentries
  tellImages layerImages

  layerInpEv_d3 <- makeLMouseDataInputEv scrollDyn True
  return layerInpEv_d3

holdLayerWidget :: forall t m. (MonadWidget t m, HasPotato t m)
  => LayerWidgetConfig t
  -> m (LayerWidget t)
holdLayerWidget lwc@LayerWidgetConfig {..} = do




  potatostylebeh <- fmap _potatoConfig_style askPotato
  PotatoStyle {..} <- sample potatostylebeh

  regionWidthDyn <- displayWidth
  --regionHeightDyn <- displayHeight

  (layerInpEv, newFolderEv) <- initLayout $ col $ mdo
    -- layer contents and scroll bar
    layerInpEv_d1 <- (grout . stretch) 1 $ row $ mdo

      -- the layer list itself
      (layerInpEv_d2, listRegionHeightDyn) <- (grout . stretch) 0 $ col $ do
        listRegionHeightDyn_d1 <- displayHeight
        layerInpEv_d3 <- layerContents lwc (fmap (\y -> (0,y)) vScrollDyn)
        return (layerInpEv_d3, listRegionHeightDyn_d1)

      -- the vertical scroll bar
      vScrollDyn <- (grout . fixed) 1 $ col $ do
        let
          contentSizeDyn = fmap ((+1) . Seq.length . _layersViewHandlerRenderOutput_entries) _layerWidgetConfig_layersView
          handleStyleBeh = undefined
        vScrollBar handleStyleBeh contentSizeDyn

      return layerInpEv_d2

    -- TODO horizontal scroll bar somedays

    -- buttons at the bottom
    (newFolderEv_d1, heightDyn) <- (grout . fixed) heightDyn $ row $ do
      (buttonsEv, heightDyn_d1) <- buttonList (constDyn ["new folder"]) (Just regionWidthDyn)
      -- TODO new layer/delete buttons how here
      -- TODO other folder options too maybe?
      return (ffilterButtonIndex 0 buttonsEv, heightDyn_d1)

    return (layerInpEv_d1, newFolderEv_d1)


  return $ LayerWidget {
      _layerWidget_mouse = layerInpEv
      , _layerWidget_newFolderEv = newFolderEv
    }
