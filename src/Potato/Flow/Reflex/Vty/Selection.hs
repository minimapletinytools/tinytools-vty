{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Selection (
  SelectionManagerConfig(..)
  , SelectionManager(..)
  , holdSelectionManager
) where
import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.Manipulator.Types
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Reflex.Potato.Helpers

import           Control.Monad.Fix
import qualified Data.IntMap.Strict                       as IM
import qualified Data.List                                as L
import           Data.Maybe
import           Data.These
import           Data.Tuple.Extra

import           Reflex

disjointUnion :: (Eq a) => [a] -> [a] -> [a]
disjointUnion a b = L.union a b L.\\ L.intersect a b

computeSelectionType :: [SuperSEltLabel] -> ManipSelectionType
computeSelectionType = foldl' foldfn MSTNone where
  foldfn accType (_,_,SEltLabel _ selt) = case accType of
    MSTNone -> case selt of
      SEltBox _  -> MSTBox
      SEltLine _ -> MSTLine
      SEltText _ -> MSTText
      _          -> MSTNone
    _ -> MSTBoundingBox


data SelectionManagerConfig t = SelectionManagerConfig {
  _selectionManagerConfig_pfctx             :: PFWidgetCtx t

  -- connect to _canvasWidget_addSEltLabel to auto select new elts
  , _selectionManagerConfig_newElt_layerPos :: Event t (LayerPos, SEltLabel)

  -- connect to _layerWidget_select
  , _selectionManagerConfig_select          :: Event t (Bool, LayerPos)

  -- connect to _canvasWidget_select
  -- left is select just one, right is select many (canvas is unaware of ordering)
  , _selectionManagerConfig_selectByREltId  :: Event t (Bool, Either [REltId] [REltId])
}

data SelectionManager t = SelectionManager {
  _selectionManager_selected :: Dynamic t (ManipSelectionType, [SuperSEltLabel]) -- (selection via newly created (but not pasted) only true for one frame where element was created, list of selected elements)
}

holdSelectionManager :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
  => SelectionManagerConfig t
  -> m (SelectionManager t)
holdSelectionManager SelectionManagerConfig {..} = mdo
  let

    sEltLabelChangesEv = _pfo_potato_changed . _pFWidgetCtx_pfo $ _selectionManagerConfig_pfctx
    pFStateDyn = _pfo_pFState . _pFWidgetCtx_pfo $ _selectionManagerConfig_pfctx

    -- ::selection from newly created element::
    newSingle = fmapMaybe (\im -> if IM.size im == 1 then IM.lookupMin im else Nothing)
      $ sEltLabelChangesEv
    selFromVeryNew_alignfn = \case
      -- we make an unchecked assumption that these two events coincide and refer to the same new element
      -- TODO add an assert that they are indeed the same elt
      These (lp,sl) (rid,_) -> Just [Just (rid, lp, sl)]
      _ -> Nothing
    -- we do this funny align so we can attach the layer position and ensure the element is actually new and not just modified
    selFromVeryNew :: Event t ([SuperSEltLabel])
    selFromVeryNew = fmap (fmap fromJust) $ alignEventWithMaybe selFromVeryNew_alignfn _selectionManagerConfig_newElt_layerPos newSingle

    -- ::selection by layer position or REltId helpers::
    selection_pushfn :: (Bool, [LayerPos]) -> PushM t [LayerPos]
    selection_pushfn (addToSelection,lps) = if not addToSelection
      then return lps
      else do
        currentSelection <- sample . current $ (fmap (fmap snd3)) selected
        return $ disjointUnion lps currentSelection

    -- TODO wrap this into a helper
    layerPosEvToSuperSEltLabelEv :: Event t [LayerPos] -> Event t [SuperSEltLabel]
    -- PARTIAL the above should never fail if layers is working correctly so I would rather it crash for now
    layerPosEvToSuperSEltLabelEv = pushAlways (\lps -> do
      pFState <- sample . current $ pFStateDyn
      return $ fmap (fromJust . flip pFState_getSuperSEltByPos pFState) lps)



    -- ::selection from LayersWidget::
    selFromLayers :: Event t [SuperSEltLabel]
    selFromLayers = layerPosEvToSuperSEltLabelEv $ pushAlways selection_pushfn $ fmap (\(b,lp) -> (b, [lp])) _selectionManagerConfig_select

    -- ::selection from CanvasWidget::
    potatoTotalDyn = _pfo_potato_potatoTotal $ _pFWidgetCtx_pfo _selectionManagerConfig_pfctx
    pushSelFromCanvas :: (Bool, Either [REltId] [REltId]) -> PushM t [LayerPos]
    pushSelFromCanvas (addToSelection, erids) = do
      pt <- sample . current $ potatoTotalDyn
      let
        -- PARTIAL
        mapToLp = map (\rid -> (fromJust . _potatoTotal_layerPosMap pt $ rid))
        lps = case erids of
          Left rids  -> case mapToLp rids of
            [] -> []
            xs -> [L.maximumBy (\lp1 lp2 -> compare lp2 lp1) xs]
          Right rids -> mapToLp rids
      selection_pushfn (addToSelection, lps)
    selFromCanvas :: Event t ([SuperSEltLabel])
    selFromCanvas = layerPosEvToSuperSEltLabelEv $ pushAlways pushSelFromCanvas _selectionManagerConfig_selectByREltId

    -- ::combine everything togethr
    selectedNew = leftmostWarn "SelectionManager - selectedNew"
      [selFromVeryNew
      , selFromLayers
      , selFromCanvas]

    selectedInputEv :: Event t (These [SuperSEltLabel] (REltIdMap (Maybe SEltLabel)))
    selectedInputEv = alignEventWithMaybe Just selectedNew sEltLabelChangesEv

    selectionFoldFn ::
      These [SuperSEltLabel] (REltIdMap (Maybe SEltLabel))
      -> [SuperSEltLabel]
      -> [SuperSEltLabel]
    selectionFoldFn (This x) _ = x
    -- this will happen if we create a new element, in which case we've already read the most updated value in 'selFromVeryNew' and can safely ignore than changes
    selectionFoldFn (These x _) _ = x
    selectionFoldFn (That slm) ssls = foldr innerfoldfn [] ssls where
      innerfoldfn sl@(rid, lp, _) acc = case IM.lookup rid slm of
        Nothing -> sl : acc
        Just mseltl -> case mseltl of
          Nothing    -> acc -- this means item got deleted
          Just seltl -> (rid, lp, seltl) : acc

  selected :: Dynamic t [SuperSEltLabel]
    <- foldDyn selectionFoldFn [] selectedInputEv
  return
    SelectionManager {
      _selectionManager_selected = fmap (\sseltls ->  (computeSelectionType sseltls, sseltls)) selected
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
  selected <- holdDyn (False, []) $ leftmostWarn "SelectionManager"
    [fmap (\x -> (True, x)) selFromVeryNew
    , fmap (\x -> (False, x)) selFromLayers]
  return
    SelectionManager {
      _selectionManager_selected = selected
    }
  -}
