{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Flow (
  flowMain
) where
import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.Canvas
import           Potato.Flow.Reflex.Vty.Layer
import           Potato.Flow.Reflex.Vty.Manipulator
import           Potato.Flow.Reflex.Vty.Selection
import           Potato.Flow.Reflex.Vty.Tools
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget


import           Control.Monad.Fix
import           Control.Monad.NodeId
import qualified Data.IntMap.Strict                 as IM
import           Data.These
import           Data.Time.Clock
import           Data.Tuple.Extra

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Potato.Helpers
import           Reflex.Vty






data ParamWidget t = ParamWidget {

}

paramWidget :: forall t m. (Reflex t, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
 => VtyWidget t m (ParamWidget t)
paramWidget = return ParamWidget {}



flowMain :: IO ()
flowMain = mainWidget $ mdo
  -- external inputs
  currentTime <- liftIO $ getCurrentTime
  tickEv <- tickLossy 1 currentTime
  ticks <- foldDyn (+) (0 :: Int) (fmap (const 1) tickEv)
  inp <- input

  -- potato flow stuff
  let
    -- TODO disable when manipulating _canvasWidget_isManipulating
    undoEv = fforMaybe inp $ \case
      V.EvKey (V.KChar 'z') [V.MCtrl] -> Just ()
      _ -> Nothing
    redoEv = fforMaybe inp $ \case
      V.EvKey (V.KChar 'y') [V.MCtrl] -> Just ()
      _ -> Nothing

    pfc = PFConfig {
        _pfc_addElt     = _canvasWidget_addSEltLabel canvasW
        , _pfc_removeElt  = never
        , _pfc_manipulate = doManipulate
        , _pfc_undo       = leftmost [undoEv, undoBeforeManipulate]
        , _pfc_redo       = redoEv
        , _pfc_save = never
        , _pfc_load = never
        , _pfc_resizeCanvas = never
        , _pfc_addFolder = never
      }
  pfo <- holdPF pfc


  -- ::prep broadphase/canvas::
  -- TODO move into Canvas.hs I guess
  let
    bpc = BroadPhaseConfig $ fmap (fmap snd) $ _sEltLayerTree_changeView (_pfo_layers pfo)
    --renderfn :: ([LBox], BPTree, REltIdMap (Maybe SEltLabel)) -> RenderedCanvas -> PushM t RenderedCanvas
    renderfn (boxes, bpt, cslmap) rc = case boxes of
      [] -> return rc
      (b:bs) -> case intersect_LBox (renderedCanvas_box rc) (foldl' union_LBox b bs) of
        Nothing -> return rc
        Just aabb -> do
          slmap <- sample . current . _directory_contents . _sEltLayerTree_directory . _pfo_layers $ pfo
          let
            rids = broadPhase_cull aabb bpt
            seltls = flip fmap rids $ \rid -> case IM.lookup rid cslmap of
              Nothing -> case IM.lookup rid slmap of
                Nothing -> error "this should never happen, because broadPhase_cull should only give existing seltls"
                Just seltl -> seltl
              Just mseltl -> case mseltl of
                Nothing -> error "this should never happen, because deleted seltl would have been culled in broadPhase_cull"
                Just seltl -> seltl
            newrc = render aabb (map _sEltLabel_sElt seltls) rc
          return $ newrc
    --foldCanvasFn :: (These ([LBox], BPTree, REltIdMap (Maybe SEltLabel)) LBox) -> RenderedCanvas -> PushM t RenderedCanvas
    foldCanvasFn (This x) rc = renderfn x rc
    foldCanvasFn (That lbx) _ = do
      bpt <- sample . current $ _broadPhase_bPTree broadPhase
      -- TODO only redo what's needed
      let renderBoxes = [lbx]
      renderfn (renderBoxes, bpt, IM.empty) (emptyRenderedCanvas lbx)
    foldCanvasFn (These _ _) _ = error "resize and change events should never occur simultaneously"
  broadPhase <- holdBroadPhase bpc
  let
    defaultCanvasLBox = LBox (V2 0 0) (V2 100 50)
  canvas <- foldDynM foldCanvasFn (emptyRenderedCanvas defaultCanvasLBox)
    $ alignEventWithMaybe Just (_broadPhase_render broadPhase) (updated . _canvas_box $ _pfo_canvas pfo)




  -- ::selection stuff::
  selectionManager <- holdSelectionManager
    SelectionManagerConfig {
      _selectionManagerConfig_newElt_layerPos = _canvasWidget_addSEltLabel canvasW
      , _selectionManagerConfig_sEltLayerTree = _pfo_layers pfo
      , _selectionManagerConfig_select = never
    }


  -- main panels
  let
    leftPanel = col $ do
      fixed 5 $ debugStream [
        never
        --, fmapLabelShow "tool" (_toolWidget_tool tools)
        --, fmapLabelShow "canvas size" $ updated . _canvas_box $ _pfo_canvas pfo
        --, fmapLabelShow "render" $ fmap fst3 (_broadPhase_render broadPhase)
        --, fmapLabelShow "change" $ fmap (fmap snd) $ _sEltLayerTree_changeView (_pfo_layers pfo)
        ]
      tools' <- fixed 3 $ holdToolsWidget
      layers' <- stretch $ holdLayerWidget $ LayerWidgetConfig (constDyn []) selectionManager
      params' <- fixed 5 $ paramWidget
      return (layers', tools', params')

    rightPanel = holdCanvasWidget $ CanvasWidgetConfig
      (_toolWidget_tool tools)
      canvas
      selectionManager

  ((layersW, tools, _), canvasW) <- splitHDrag 35 (fill '*') leftPanel rightPanel

  -- prep manipulate event
  -- MANY FRAMES via ManipulatorWidget (ok, as undo manipulation currently is 1 frame in potato-flow, and the previous operation to undo is always a manipulate operation)
  let
    manipulatorW = _canvasWidget_manipulatorWidget canvasW
    undoBeforeManipulate = fmapMaybe (\x -> if fst x then Just () else Nothing) $ _manipulatorWidget_modify manipulatorW
    doManipulate' = fmap snd $ _manipulatorWidget_modify manipulatorW
  doManipulate <- sequenceEvents undoBeforeManipulate doManipulate'

  -- handle escape events
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
