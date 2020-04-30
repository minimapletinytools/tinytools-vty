{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Layer (
  LayerWidgetConfig(..)
  , LayerWidget(..)
  , holdLayerWidget
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.CanvasPane
import           Potato.Flow.Reflex.Vty.Selection
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget
import           Reflex.Potato.Helpers

import           Control.Monad.Fix
import           Data.Dependent.Sum                (DSum ((:=>)))
import qualified Data.IntMap.Strict                as IM
import           Data.These

import           Reflex
import           Reflex.Network
import           Reflex.Vty


data LayerWidgetConfig t = LayerWidgetConfig {
  _layerWidgetConfig_temp_sEltTree      :: Dynamic t SEltTree
  , _layerWidgetConfig_selectionManager :: SelectionManager t
}

data LayerWidget t = LayerWidget {
  _layerWidget_select       :: Event t LayerPos
  , _layerWidget_changeName :: Event t ControllersWithId
}

holdLayerWidget :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
holdLayerWidget LayerWidgetConfig {..} = do
  pw <- displayWidth
  ph <- displayHeight
  addButton <- col $ do
    fixed 1 $ debugFocus
    fixed 1 $ text . current . fmap (show . length)$ _layerWidgetConfig_temp_sEltTree
    addButton <- fixed 3 $ textButtonStatic def "add"
    -- note this is only possible because you added PostBuild to Layout
    stretch $ col $ simpleList (fmap (zip [0..]) _layerWidgetConfig_temp_sEltTree) $ \ds -> do
      fixed 1 $ text $ current $ fmap (_sEltLabel_name . snd) ds

    return addButton
  return LayerWidget {
    _layerWidget_select = never
    , _layerWidget_changeName = never
  }
