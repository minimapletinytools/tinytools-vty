{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Selection (
  SelectionManagerConfig(..)
  , SelectionManager(..)
  , holdSelectionManager
) where
import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Reflex.Potato.Helpers


import           Control.Monad.Fix
import qualified Data.IntMap.Strict                 as IM
import           Data.These

import           Reflex



data SelectionManagerConfig t = SelectionManagerConfig {
  _selectionManagerConfig_pfctx             :: PFWidgetCtx t
  -- connect to _canvasWidget_addSEltLabel to auto select new elts
  , _selectionManagerConfig_newElt_layerPos :: Event t (LayerPos, SEltLabel)
  -- connect to _sEltLayerTree_changeView
  , _selectionManagerConfig_sEltLayerTree   :: SEltLayerTree t
  -- TODO multi-select
  , _selectionManagerConfig_select          :: Event t LayerPos
}

data SelectionManager t = SelectionManager {
  _selectionManager_selected :: Dynamic t Selected
}

holdSelectionManager :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => SelectionManagerConfig t
  -> m (SelectionManager t)
holdSelectionManager SelectionManagerConfig {..} = do
  let
    newSingle = fmapMaybe (\im -> if IM.size im == 1 then IM.lookupMin im else Nothing)
      $ _sEltLayerTree_changeView _selectionManagerConfig_sEltLayerTree
    alignfn = \case
      These (lp,sl) (rid,_) -> Just [(rid, lp, sl)]
      _ -> Nothing
    -- we make an unchecked assumption that these two events coincide and refer to the same new element
    selFromNew = alignEventWithMaybe alignfn _selectionManagerConfig_newElt_layerPos newSingle
    selFromLayers = fmap (:[]) $ sEltLayerTree_tagSuperSEltByPos _selectionManagerConfig_sEltLayerTree _selectionManagerConfig_select
  selected <- holdDyn [] $ leftmostwarn "SelectionManager" [selFromNew, selFromLayers]
  return
    SelectionManager {
      _selectionManager_selected = selected
    }
