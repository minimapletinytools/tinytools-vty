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
import           Data.Maybe
import           Data.These

import           Reflex



data SelectionManagerConfig t = SelectionManagerConfig {
  _selectionManagerConfig_pfctx             :: PFWidgetCtx t
  -- connect to _canvasWidget_addSEltLabel to auto select new elts
  , _selectionManagerConfig_newElt_layerPos :: Event t (LayerPos, SEltLabel)
  -- connect to _sEltLayerTree_changeView
  , _selectionManagerConfig_sEltLayerTree   :: SEltLayerTree t
  -- TODO use Dynamic Seq instead maybe
  , _selectionManagerConfig_select          :: Event t [LayerPos]
}

data SelectionManager t = SelectionManager {
  _selectionManager_selected :: Dynamic t (Bool, [SuperSEltLabel]) -- (selection via newly created (but not pasted), list of selected elements)
}

holdSelectionManager :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => SelectionManagerConfig t
  -> m (SelectionManager t)
holdSelectionManager SelectionManagerConfig {..} = do
  let
    newSingle = fmapMaybe (\im -> if IM.size im == 1 then IM.lookupMin im else Nothing)
      $ _sEltLayerTree_changeView _selectionManagerConfig_sEltLayerTree
    selFromVeryNew_alignfn = \case
      These (lp,sl) (rid,_) -> Just [Just (rid, lp, sl)]
      _ -> Nothing

    -- we make an unchecked assumption that these two events coincide and refer to the same new element
    -- we do this funny align so we can attach the layer position and ensure the element is actually new and not just modified
    selFromVeryNew :: Event t ([SuperSEltLabel])
    selFromVeryNew = fmap (fmap fromJust) $ alignEventWithMaybe selFromVeryNew_alignfn _selectionManagerConfig_newElt_layerPos newSingle
    selFromLayers :: Event t ([SuperSEltLabel])
    selFromLayers = fmap (fmap fromJust) $ sEltLayerTree_tagSuperSEltsByPos _selectionManagerConfig_sEltLayerTree _selectionManagerConfig_select
    -- PARTIAL the above should never fail if layers is working correctly so I would rather it crash for now
    -- non-partial version:
    --selFromLayers = fmapMaybe sequence $ sEltLayerTree_tagSuperSEltsByPos _selectionManagerConfig_sEltLayerTree _selectionManagerConfig_select

    selectedNew = leftmostwarn "SelectionManager - selectedNew"
      [fmap (\x -> (True,  x)) selFromVeryNew
      , fmap (\x -> (False, x)) selFromLayers]

  let
    selChangesFromModified :: Event t (REltIdMap (Maybe SEltLabel))
    selChangesFromModified = fmap (fmap snd) $ _sEltLayerTree_changeView _selectionManagerConfig_sEltLayerTree

    selectedInputEv :: Event t (These (Bool, [SuperSEltLabel]) (REltIdMap (Maybe SEltLabel)))
    selectedInputEv = alignEventWithMaybe Just selectedNew selChangesFromModified

    selectionFoldFn :: These (Bool, [SuperSEltLabel]) (REltIdMap (Maybe SEltLabel)) -> (Bool, [SuperSEltLabel]) -> (Bool, [SuperSEltLabel])
    selectionFoldFn (This x) _ = x
    -- this will happen if we create a new element, in which case we've already read the most updated value in 'selFromVeryNew' and can safely ignore than changes
    selectionFoldFn (These x _) _ = x
    selectionFoldFn (That slm) (vn, ssls) = (vn, foldr innerfoldfn [] ssls) where
      innerfoldfn sl@(rid, lp, _) acc = case IM.lookup rid slm of
        Nothing -> sl : acc
        Just mseltl -> case mseltl of
          Nothing    -> acc -- this means item got deleted
          Just seltl -> (rid, lp, seltl) : acc

  selected :: Dynamic t (Bool, [SuperSEltLabel])
    <- foldDyn selectionFoldFn (False, []) selectedInputEv
  return
    SelectionManager {
      _selectionManager_selected = selected
    }



  {-
  let
    newSingle = fmapMaybe (\im -> if IM.size im == 1 then IM.lookupMin im else Nothing)
      $ _sEltLayerTree_changeView _selectionManagerConfig_sEltLayerTree
    alignfn = \case
      These (lp,sl) (rid,_) -> Just [(rid, lp, sl)]
      _ -> Nothing
    -- we make an unchecked assumption that these two events coincide and refer to the same new element
    selFromVeryNew = alignEventWithMaybe alignfn _selectionManagerConfig_newElt_layerPos newSingle
    selFromLayers = fmap (:[]) $ sEltLayerTree_tagSuperSEltByPos _selectionManagerConfig_sEltLayerTree _selectionManagerConfig_select
    selFromModified =
  selected <- holdDyn (False, []) $ leftmostwarn "SelectionManager"
    [fmap (\x -> (True, x)) selFromVeryNew
    , fmap (\x -> (False, x)) selFromLayers]
  return
    SelectionManager {
      _selectionManager_selected = selected
    }
  -}
