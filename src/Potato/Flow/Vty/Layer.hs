
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

  (layerInpEv) <- initLayout $ col $ mdo
    -- the layer list itself
    (layerInpEv_d1) <- (grout . stretch) 5 $ row $ do
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
            label = isOwl_name sowl

            attr = case selected of
              LHRESS_Selected -> _potatoStyle_selected
              LHRESS_InheritSelected -> _potatoStyle_selected
              LHRESS_ChildSelected -> _potatoStyle_softSelected
              _ -> _potatoStyle_normal

            -- TODO correct styles so they aren't confused with selected styles (you should add colors)
            attrrenamingbg = _potatoStyle_softSelected
            attrrenamingcur = _potatoStyle_selected

            identn = case mdots of
              Nothing -> ident
              Just x -> x

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
          $ ffor2 listRegionDyn (fmap _layersViewHandlerRenderOutput_entries _layerWidgetConfig_layersView) $ \(w,h) lhrentries ->
            map (makeLayerImage w) . L.take (max 0 (h - padBottom)) $ toList lhrentries
      tellImages layerImages
      let
        -- TODO scrolling?
        offset = V2 0 0
      layerInpEv_d2 <- makeLMouseDataInputEv offset True
      return layerInpEv_d2


    -- buttons at the bottom
    (grout . fixed) 1 $ row $ do
      -- TODO new layer/delete buttons how here
      -- TODO other folder options too maybe?
      return ()

    return (layerInpEv_d1)


  return $ LayerWidget layerInpEv
