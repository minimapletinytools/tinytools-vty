{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Canvas (
  CanvasWidgetConfig(..)
  , CanvasWidget(..)
  , holdCanvasWidget
) where


import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.Manipulator
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Flow.Reflex.Vty.Selection
import           Potato.Flow.Reflex.Vty.Tools
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget
import           Reflex.Potato.Helpers

import           Control.Lens
import qualified Data.IntMap.Strict                 as IM
import           Data.These

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Vty



-- returns pan position at start of drag and dragging info filtered for tool/drag state
toolDragStateEv :: (Reflex t)
  => Maybe Tool -- ^ tool state to select for
  -> Maybe DragState
  -> Event t ((Tool, (Int,Int)), Drag2) -- ^ ((tool, panPos), drag)
  -> Event t ((Int,Int), Drag2)
toolDragStateEv c' d' dragEv = r where
  fmapMaybeFn ((c,p),d) = if maybe True (_drag2_state d  ==) d' && maybe True (c ==) c'
    then Just (p,d)
    else Nothing
  r = fmapMaybe fmapMaybeFn dragEv

dynLBox_to_dynRegion :: (Reflex t) => Dynamic t LBox -> DynRegion t
dynLBox_to_dynRegion dlb = r where
  x' = flip fmap dlb $ \(LBox (V2 x _) _) -> x
  y' = flip fmap dlb $ \(LBox (V2 _ y) _) -> y
  w' = flip fmap dlb $ \(LBox _ (V2 w _)) -> w
  h' = flip fmap dlb $ \(LBox _ (V2 _ h)) -> h
  r = DynRegion x' y' w' h'

translate_dynRegion :: (Reflex t) => Dynamic t (Int, Int) -> DynRegion t -> DynRegion t
translate_dynRegion pos dr = dr {
    _dynRegion_left = liftA2 (+) (_dynRegion_left dr) (fmap fst pos)
    , _dynRegion_top = liftA2 (+) (_dynRegion_top dr) (fmap snd pos)
  }


data CanvasWidgetConfig t = CanvasWidgetConfig {
  _canvasWidgetConfig_pfctx              :: PFWidgetCtx t
  , _canvasWidgetConfig_tool             :: Dynamic t Tool
  , _canvasWidgetConfig_selectionManager :: SelectionManager t
  , _canvasWidgetConfig_pfo              :: PFOutput t
}

data CanvasWidget t = CanvasWidget {
  _canvasWidget_isManipulating      :: Dynamic t Bool

  , _canvasWidget_addSEltLabel      :: Event t (Bool, (LayerPos, SEltLabel))
  , _canvasWidget_modify            :: Event t (Bool, ControllersWithId)

  , _canvasWidget_consumingKeyboard :: Behavior t Bool
  , _canvasWidget_select            :: Event t (Bool, Either [REltId] [REltId]) -- ^ (left is select single, right is select many)
  , _canvasWidget_undo              :: Event t ()
}

holdCanvasWidget :: forall t m. (MonadWidget t m)
  => CanvasWidgetConfig t
  -> VtyWidget t m (CanvasWidget t)
holdCanvasWidget CanvasWidgetConfig {..} = mdo

  -- ::prepare broadphase/canvas::
  let
    bpc = BroadPhaseConfig $ fmap (fmap snd) $ _sEltLayerTree_changeView (_pfo_layers _canvasWidgetConfig_pfo)
    --renderfn :: ([LBox], BPTree, REltIdMap (Maybe SEltLabel)) -> RenderedCanvas -> PushM t RenderedCanvas
    renderfn (boxes, bpt, cslmap) rc = case boxes of
      [] -> return rc
      (b:bs) -> case intersect_LBox (renderedCanvas_box rc) (foldl' union_LBox b bs) of
        Nothing -> return rc
        Just aabb -> do
          -- TODO use PotatoTotal
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
            -- TODO need to order seltls by layer position oops
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

  -- ::prepare rendered canvas ::
  renderedCanvas <- foldDynM foldCanvasFn (emptyRenderedCanvas defaultCanvasLBox)
    $ alignEventWithMaybe Just (_broadPhase_render broadPhase) (updated . _canvas_box $ _pfo_canvas _canvasWidgetConfig_pfo)

  -- ::cursor::
  -- NOTE the way we check if drag events go to canvas vs handle is a little bad TODO please fix
  dragOrigEv :: Event t ((Tool, (Int,Int)), Drag2)
    <- drag2AttachOnStart V.BLeft (ffor2 (current _canvasWidgetConfig_tool) (current panPos) (,))
  canvasIsDraggingDyn <- foldDynMaybe canvasIsDraggingDyn_foldfn False $ fmap snd $ dragEv'
  let
    dragEv' = difference dragOrigEv (_manipulatorWidget_didCaptureMouse manipulatorW)
    toolStartEv c' = toolDragStateEv (Just c') (Just DragStart) dragEv'
    toolDragEv c' = gate (current canvasIsDraggingDyn) $ toolDragStateEv (Just c') Nothing dragEv'
    toolEndEv c' = gate (current canvasIsDraggingDyn) $ toolDragStateEv (Just c') (Just DragEnd) dragEv'
    dragEv c' = leftmost [toolStartEv c', toolDragEv c', toolEndEv c']
    -- I'm still not totally sure if DragEnd is guarantee to trigger after a DragStart, but this is harmless-ish if it doesn't happen in this case
    canvasIsDraggingDyn_foldfn :: Drag2 -> Bool -> Maybe Bool
    canvasIsDraggingDyn_foldfn (Drag2 _ _ _ _ DragStart) _ = Just True
    canvasIsDraggingDyn_foldfn (Drag2 _ _ _ _ DragEnd) _   = Just False
    canvasIsDraggingDyn_foldfn _ _                         = Nothing

  -- ::panning::
  LBox (V2 cx0 cy0) (V2 cw0 ch0) <- sample $ current (fmap renderedCanvas_box renderedCanvas)
  pw0 <- displayWidth >>= sample . current
  ph0 <- displayHeight >>= sample . current
  let
    panFoldFn ((sx,sy), Drag2 (fromX, fromY) (toX, toY) _ _ _) _ = (sx + toX-fromX, sy + toY-fromY)
  -- panPos is position of upper left corner of canvas relative to screen
  panPos <- foldDyn panFoldFn (cx0 - (cw0-pw0)`div`2, cy0 - (ch0-ph0)`div`2) $ toolDragEv TPan

  -- ::selecting::
  let
    selectPushFn :: ((Int,Int),Drag2) -> PushM t (Bool, Either [REltId] [REltId])
    selectPushFn ((sx,sy), drag) = case drag of
      Drag2 (fromX, fromY) (toX, toY) _ mods _ -> do
        let
          shiftClick = isJust $ find (==V.MShift) mods
          boxSize = V2 (toX-fromX) (toY-fromY)
          selectBox = LBox (V2 (fromX-sx) (fromY-sy)) boxSize
          selectType = if boxSize == 0 then Left else Right
        bpt <- sample . current $ _broadPhase_bPTree broadPhase
        return $ (shiftClick, selectType $ broadPhase_cull selectBox bpt)
    selectEv = pushAlways selectPushFn (toolEndEv TSelect)
{-
  -- TODO go straight into CBoundingBox move on single select
    --so listen to dragstart event and if it clicked on something pass through selection event and ignore dragend event
    -- unless <some modifier> is held, in which case do normal selecting

    TODO finish this
    selectingBoxDyn_foldfn :: ((Int,Int),Drag2) -> (Bool, LBox) -> (Bool, LBox)
    selectingBoxDyn_foldfn ((sx,sy), drag) _ = case drag of
  selectingBoxDyn <- foldDyn selectingBoxDyn_foldfn (False, LBox 0 0) (dragEv TSelect)
-}

  -- ::draw the canvas::
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
      --, fmapLabelShow "select" selectEv
      --, fmapLabelShow "drag" dragEv
      --, fmapLabelShow "input" inp
      --, fmapLabelShow "_canvasWidgetConfig_tool" (updated _canvasWidgetConfig_tool)
      --, fmapLabelShow "selection" (updated $ _selectionManager_selected _canvasWidgetConfig_selectionManager)
      --, fmapLabelShow "manip" $ _manipulatorWidget_modify manipulatorW
      ]
    --fixed 1 $ row $ do
    --  fixed 15 $ text $ fmap (\x -> "_canvasWidgetConfig_tool: " <> show x) $ current _canvasWidgetConfig_tool


  -- ::manipulators::
  let
    manipCfg = ManipulatorWidgetConfig {
        _manipulatorWigetConfig_pfctx = _canvasWidgetConfig_pfctx
        , _manipulatorWigetConfig_selected = _selectionManager_selected _canvasWidgetConfig_selectionManager
        , _manipulatorWidgetConfig_panPos = current panPos
        -- TODO this is not correct
        , _manipulatorWidgetConfig_drag = fmap (over _1 fst) dragOrigEv
        , _manipulatorWidgetConfig_tool = _canvasWidgetConfig_tool
      }
  manipulatorW <- holdManipulatorWidget manipCfg

  return CanvasWidget {
      -- TODO
      _canvasWidget_isManipulating = constDyn False
      , _canvasWidget_addSEltLabel = _manipulatorWidget_add manipulatorW
      , _canvasWidget_modify = _manipulatorWidget_modify manipulatorW
      , _canvasWidget_select = selectEv
      -- TODO
      , _canvasWidget_consumingKeyboard = constant False
      , _canvasWidget_undo = _manipulatorWidget_undo manipulatorW

    }
