{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Canvas (
  CanvasWidgetConfig(..)
  , CanvasWidget(..)
  , holdCanvasWidget
) where


import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.Canvas.Types
import           Potato.Flow.Reflex.Vty.Manipulator
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Flow.Reflex.Vty.Selection
import           Potato.Flow.Reflex.Vty.Tools
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget

import qualified Data.IntMap.Strict                  as IM
import qualified Data.Text                           as T
import           Data.These

import qualified Graphics.Vty                        as V
import           Reflex
import           Reflex.Vty


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

  -- ::draw the canvas ::
  renderedCanvas <- foldDynM foldCanvasFn (emptyRenderedCanvas defaultCanvasLBox)
    $ alignEventWithMaybe Just (_broadPhase_render broadPhase) (updated . _canvas_box $ _pfo_canvas _canvasWidgetConfig_pfo)
  let
    canvasRegion = translate_dynRegion panPos $ dynLBox_to_dynRegion (fmap renderedCanvas_box renderedCanvas)
  fill '░'
  pane canvasRegion (constDyn True) $ do
    text $ current (fmap renderedCanvasToText renderedCanvas)


  -- ::drag events::
  dragOrigEv :: Event t CanvasDrag
    <- drag2AttachOnStart V.BLeft (ffor2 (current _canvasWidgetConfig_tool) (current panPos) (,))
  trackedDrag0 <- trackDrag dragOrigEv (_pFWidgetCtx_ev_cancel _canvasWidgetConfig_pfctx)

  -- ::create manipulator::
  -- since trackDrag is monadic, drag consumers must be created in order
  let
    manipCfg = ManipulatorWidgetConfig {
        _manipulatorWigetConfig_pfctx = _canvasWidgetConfig_pfctx
        , _manipulatorWigetConfig_selected = _selectionManager_selected _canvasWidgetConfig_selectionManager
        , _manipulatorWidgetConfig_panPos = current panPos
        , _manipulatorWidgetConfig_trackedDrag = trackedDrag0
        , _manipulatorWidgetConfig_tool = _canvasWidgetConfig_tool
      }
  manipulatorW <- holdManipulatorWidget manipCfg
  trackedDrag1 <- return $ _manipulatorWidget_trackedDrag manipulatorW

  -- ::panning::
  (trackedDrag2, panDrag) <- filterCanvasTrackedDrag (Just TPan) trackedDrag1
  let
    panFoldFn :: Either ((Int,Int), Drag2) () -> (Int,Int) -> (Int, Int)
    -- TODO needs to sample last finalized pan position from somewhere..
    panFoldFn (Right _) p = p
    panFoldFn (Left ((sx,sy), Drag2 (fromX, fromY) (toX, toY) _ _ _)) _ = (sx + toX-fromX, sy + toY-fromY)
  LBox (V2 cx0 cy0) (V2 cw0 ch0) <- sample $ current (fmap renderedCanvas_box renderedCanvas)
  pw0 <- displayWidth >>= sample . current
  ph0 <- displayHeight >>= sample . current
  panPos <- foldDyn panFoldFn (cx0 - (cw0-pw0)`div`2, cy0 - (ch0-ph0)`div`2) $ alignTrackedDrag panDrag


  -- ::selecting::
  -- TODO canceling drags is broken (you can fix easily by not having dynamic selecting, i.e. only select on DragEnd)
  -- TODO go straight into CBoundingBox move on single select
  -- this requires manipulator to be capturing the drag after selection (which happens one frame later)
  -- so to do this, have a separate routine that checks for click on a single element, manipulator captures drag in this case
  -- code below can stay as before
  (_, selectDrag) <- filterCanvasTrackedDrag (Just TSelect) trackedDrag2
  let
    selectLBox_foldfn :: ((Int,Int), Drag2) -> Maybe LBox -> Maybe LBox
    selectLBox_foldfn (_, d) _ = case d of
      Drag2 _ _ _ _ DragEnd -> Nothing
      Drag2 (fromX, fromY) (toX, toY) _ _ _ -> Just $ LBox (V2 fromX fromY) (V2 (toX-fromX) (toY-fromY))
    selectEv_pushfn :: ((Int,Int), Drag2) -> PushM t (Bool, Either [REltId] [REltId])
    selectEv_pushfn ((sx,sy), d) = case d of
      Drag2 (fromX, fromY) (toX, toY) _ mods _ -> do
        let
          shiftClick = isJust $ find (==V.MShift) mods
          boxSize = V2 (toX-fromX) (toY-fromY)
          selectBox = LBox (V2 (fromX-sx) (fromY-sy)) boxSize
          selectType = if boxSize == 0 then Left else Right
        bpt <- sample . current $ _broadPhase_bPTree broadPhase
        return $ (shiftClick, selectType $ broadPhase_cull selectBox bpt)
    selectEv = pushAlways selectEv_pushfn $ _trackedDrag_drag selectDrag
  selectLBox <- foldDyn selectLBox_foldfn Nothing $ _trackedDrag_drag selectDrag
  selectBoxWidget $ fmap (maybe (LBox 0 0) id) selectLBox

  trackDrag2Dyn <- holdDyn Nothing $ (fmap Just $ _trackedDrag_drag trackedDrag2)

  -- ::info pane::
  col $ do
    fixed 2 $ debugStreamBeh $ [ fmapLabelShow "trackedDrag2" (current $ trackDrag2Dyn) ]
    fixed 2 $ debugStream
      [
      never
      --, fmapLabelShow "manipTracking" (updated $ _trackedDrag_dragging trackedDrag0)
      --, fmapLabelShow "selectDrag" $ _trackedDrag_drag selectDrag
      --, fmapLabelShow "trackedDrag2" $ _trackedDrag_drag trackedDrag2
      --, fmapLabelShow "pan" (_trackedDrag_drag panDrag)
      --, fmapLabelShow "drag" dragEv
      --, fmapLabelShow "input" inp
      --, fmapLabelShow "_canvasWidgetConfig_tool" (updated _canvasWidgetConfig_tool)
      --, fmapLabelShow "selection" (updated $ _selectionManager_selected _canvasWidgetConfig_selectionManager)
      --, fmapLabelShow "manip" $ _manipulatorWidget_modify manipulatorW
      ]
    --fixed 1 $ row $ do
    --  fixed 15 $ text $ fmap (\x -> "_canvasWidgetConfig_tool: " <> show x) $ current _canvasWidgetConfig_tool

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







-- box drawing code for selection box
-- pretty much copy pasted from boxTitle in 'Reflex.Vty.Widget'
withinImage :: Region -> V.Image -> V.Image
withinImage (Region left top width height)
  | width < 0 || height < 0 = withinImage (Region left top 0 0)
  | otherwise = V.translate left top . V.crop width height
selectBoxWidget' :: (Monad m, Reflex t, MonadNodeId m)
    => Behavior t BoxStyle
    -> Text
    -> VtyWidget t m a
    -> VtyWidget t m a
selectBoxWidget' boxStyle title child = do
  dh <- displayHeight
  dw <- displayWidth
  let boxReg = DynRegion (pure 0) (pure 0) dw dh
      innerReg = DynRegion (pure 1) (pure 1) (subtract 2 <$> dw) (subtract 2 <$> dh)
  tellImages (boxImages <$> boxStyle <*> currentRegion boxReg)
  --tellImages (fmap (\r -> [regionBlankImage r]) (currentRegion innerReg))
  pane innerReg (pure True) child
  where
    boxImages :: BoxStyle -> Region -> [V.Image]
    boxImages style (Region left top width height) =
      let right = left + width - 1
          bottom = top + height - 1
          sides =
            [ withinImage (Region (left + 1) top (width - 2) 1) $
                V.text' V.defAttr $
                  hPadText title (_boxStyle_n style) (width - 2)
            , withinImage (Region right (top + 1) 1 (height - 2)) $
                V.charFill V.defAttr (_boxStyle_e style) 1 (height - 2)
            , withinImage (Region (left + 1) bottom (width - 2) 1) $
                V.charFill V.defAttr (_boxStyle_s style) (width - 2) 1
            , withinImage (Region left (top + 1) 1 (height - 2)) $
                V.charFill V.defAttr (_boxStyle_w style) 1 (height - 2)
            ]
          corners =
            [ withinImage (Region left top 1 1) $
                V.char V.defAttr (_boxStyle_nw style)
            , withinImage (Region right top 1 1) $
                V.char V.defAttr (_boxStyle_ne style)
            , withinImage (Region right bottom 1 1) $
                V.char V.defAttr (_boxStyle_se style)
            , withinImage (Region left bottom 1 1) $
                V.char V.defAttr (_boxStyle_sw style)
            ]
      in sides ++ if width > 1 && height > 1 then corners else []
    hPadText :: T.Text -> Char -> Int -> T.Text
    hPadText t c l = if lt >= l
                     then t
                     else left <> t <> right
      where
        lt = T.length t
        delta = l - lt
        mkHalf n = T.replicate (n `div` 2) (T.singleton c)
        left = mkHalf $ delta + 1
        right = mkHalf delta
selectBoxWidget :: (Reflex t, MonadNodeId m)
  => Dynamic t LBox
  -> VtyWidget t m ()
selectBoxWidget db = pane (dynLBox_to_dynRegion db) (constDyn False) $ selectBoxWidget' (constant singleBoxStyle) mempty (return ())
