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

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Vty




data CanvasWidgetConfig t = CanvasWidgetConfig {
  _canvasWidgetConfig_pfctx                 :: PFWidgetCtx t
  , _canvasWidgetConfig_tool                :: Event t Tool
  , _canvasWidgetConfig_renderedCanvas_temp :: Dynamic t RenderedCanvas
  , _canvasWidgetConfig_selectionManager    :: SelectionManager t
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
  LBox (V2 cx0 cy0) (V2 cw0 ch0) <- sample $ current (fmap renderedCanvas_box _canvasWidgetConfig_renderedCanvas_temp)
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
    canvasRegion = translate_dynRegion panPos $ dynLBox_to_dynRegion (fmap renderedCanvas_box _canvasWidgetConfig_renderedCanvas_temp)
  fill '░'
  pane canvasRegion (constDyn True) $ do
    text $ current (fmap renderedCanvasToText _canvasWidgetConfig_renderedCanvas_temp)

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
