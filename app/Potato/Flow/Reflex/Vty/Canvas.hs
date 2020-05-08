{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Canvas (
  CanvasWidgetConfig(..)
  , CanvasWidget(..)
  , holdCanvasWidget
) where


import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.CanvasPane
import           Potato.Flow.Reflex.Vty.Manipulator
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Flow.Reflex.Vty.Selection
import           Potato.Flow.Reflex.Vty.Tools
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget
import           Reflex.Potato.Helpers

import           Control.Monad.Fix
import qualified Data.IntMap.Strict                 as IM
import           Data.These

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Vty




data CanvasWidgetConfig t = CanvasWidgetConfig {
  _canvasWidgetConfig_pfctx              :: PFWidgetCtx t
  , _canvasWidgetConfig_tool             :: Event t Tool
  , _canvasWidgetConfig_selectionManager :: SelectionManager t
  , _canvasWidgetConfig_pfo              :: PFOutput t
}

data CanvasWidget t = CanvasWidget {
  _canvasWidget_isManipulating :: Dynamic t Bool

  , _canvasWidget_addSEltLabel :: Event t (Bool, (LayerPos, SEltLabel))
  , _canvasWidget_modify       :: Event t (Bool, ControllersWithId)
}

holdCanvasWidget :: forall t m. (MonadWidget t m)
  => CanvasWidgetConfig t
  -> VtyWidget t m (CanvasWidget t)
holdCanvasWidget CanvasWidgetConfig {..} = mdo
  inp <- input

  -- ::prep broadphase/canvas::
  let
    bpc = BroadPhaseConfig $ fmap (fmap snd) $ _sEltLayerTree_changeView (_pfo_layers _canvasWidgetConfig_pfo)
    --renderfn :: ([LBox], BPTree, REltIdMap (Maybe SEltLabel)) -> RenderedCanvas -> PushM t RenderedCanvas
    renderfn (boxes, bpt, cslmap) rc = case boxes of
      [] -> return rc
      (b:bs) -> case intersect_LBox (renderedCanvas_box rc) (foldl' union_LBox b bs) of
        Nothing -> return rc
        Just aabb -> do
          slmap <- sample . current . _directory_contents . _sEltLayerTree_directory . _pfo_layers $ _canvasWidgetConfig_pfo
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

  -- :: prepare rendered canvas ::
  renderedCanvas <- foldDynM foldCanvasFn (emptyRenderedCanvas defaultCanvasLBox)
    $ alignEventWithMaybe Just (_broadPhase_render broadPhase) (updated . _canvas_box $ _pfo_canvas _canvasWidgetConfig_pfo)


  -- ::cursor::
  let
    escEv = fforMaybe inp $ \case
      V.EvKey (V.KEsc) [] -> Just ()
      _ -> Nothing
  cursor <- holdDyn CSSelecting $ leftmost [fmap tool_cursorState _canvasWidgetConfig_tool, CSSelecting <$ escEv]
  dragOrigEv :: Event t ((CursorState, (Int,Int)), Drag2) <- drag2AttachOnStart V.BLeft (ffor2 (current cursor)  (current panPos) (,))
  let
    -- ignore inputs captured by manipulator
    dragEv = difference dragOrigEv (_manipulatorWidget_didCaptureMouse manipulatorW)
    cursorDragEv c' = cursorDragStateEv (Just c') Nothing dragEv
    --cursorDraggingEv c' = cursorDragStateEv (Just c') (Just Dragging) dragEv
    cursorStartEv c' = cursorDragStateEv (Just c') (Just DragStart) dragEv
    --cursorEndEv c' = cursorDragStateEv (Just c') (Just DragEnd) dragEv

  -- ::panning::
  -- TODO make this so it doesn't trigger when you start drag off of this panel
  -- you could do this by checking if dragFrom is on the edges
  LBox (V2 cx0 cy0) (V2 cw0 ch0) <- sample $ current (fmap renderedCanvas_box renderedCanvas)
  pw0 <- displayWidth >>= sample . current
  ph0 <- displayHeight >>= sample . current
  let
    panFoldFn ((sx,sy), Drag2 (fromX, fromY) (toX, toY) _ _ _) _ = (sx + toX-fromX, sy + toY-fromY)
  -- panPos is position of upper left corner of canvas relative to screen
  panPos <- foldDyn panFoldFn (cx0 - (cw0-pw0)`div`2, cy0 - (ch0-ph0)`div`2) $ cursorDragEv CSPan

  -- ::new elts::
  -- TODO move this inside of Manipulator
  let
    boxPushFn ((px,py), Drag2 (fromX, fromY) _ _ _ _) = do
      pos <- return 0
      --return $ (pos, SEltLabel "<box>" $ SEltBox $ SBox (LBox (V2 (fromX-px) (fromY-py)) (V2 1 1)) def)
      -- 0,0 initial size is more correct for immediate manipulation, but kind of annoying as you can end up with 0x0 boxes very easily...
      return $ (pos, SEltLabel "<box>" $ SEltBox $ SBox (LBox (V2 (fromX-px) (fromY-py)) (V2 0 0)) def)
    newBoxEv = pushAlways boxPushFn $ cursorStartEv CSBox

  -- ::draw the canvas::
  -- TODO make this efficient -_-
  let
    canvasRegion = translate_dynRegion panPos $ dynLBox_to_dynRegion (fmap renderedCanvas_box renderedCanvas)
  fill '░'
  pane canvasRegion (constDyn True) $ do
    text $ current (fmap renderedCanvasToText renderedCanvas)

  -- ::info pane::
  col $ do
    fixed 2 $ debugStream
      [
      never
      --, fmapLabelShow "drag" dragEv
      --, fmapLabelShow "input" inp
      --, fmapLabelShow "cursor" (updated cursor)
      --, fmapLabelShow "selection" (updated $ _selectionManager_selected _canvasWidgetConfig_selectionManager)
      --, fmapLabelShow "manip" $ _manipulatorWidget_modify manipulatorW
      ]
    --fixed 1 $ row $ do
    --  fixed 15 $ text $ fmap (\x -> "cursor: " <> show x) $ current cursor


  -- ::manipulators::
  let
    manipCfg = ManipulatorWidgetConfig {
        _manipulatorWigetConfig_pfctx = _canvasWidgetConfig_pfctx
        , _manipulatorWigetConfig_selected = _selectionManager_selected _canvasWidgetConfig_selectionManager
        , _manipulatorWidgetConfig_panPos = current panPos
        -- TODO this is not correct
        , _manipulatorWidgetConfig_drag = dragOrigEv
      }
  manipulatorW <- holdManipulatorWidget manipCfg

  return CanvasWidget {
      -- TODO
      _canvasWidget_isManipulating = constDyn False
      , _canvasWidget_addSEltLabel = leftmostwarn "canvas add"
        [fmap (\x -> (False, x)) newBoxEv
        , _manipulatorWidget_add manipulatorW]
      , _canvasWidget_modify = _manipulatorWidget_modify manipulatorW
    }
