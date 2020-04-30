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
import           Potato.Flow.Reflex.Vty.Selection
import           Potato.Flow.Reflex.Vty.Tools
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget
import           Reflex.Potato.Helpers

import           Control.Monad.Fix
import           Data.Dependent.Sum                 (DSum ((:=>)))
import qualified Data.IntMap.Strict                 as IM
import           Data.These

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Network
import           Reflex.Vty




data CanvasWidgetConfig t = CanvasWidgetConfig {
  _canvasWidgetConfig_tool                  :: Event t Tool
  , _canvasWidgetConfig_renderedCanvas_temp :: Dynamic t RenderedCanvas
  , _canvasWidgetConfig_selectionManager    :: SelectionManager t
}

data CanvasWidget t = CanvasWidget {
  _canvasWidget_isManipulating      :: Dynamic t Bool
  , _canvasWidget_addSEltLabel      :: Event t (LayerPos, SEltLabel)
  , _canvasWidget_manipulatorWidget :: ManipulatorWidget t
}

holdCanvasWidget :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, NotReady t m, MonadHold t m, MonadFix m, MonadNodeId m)
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
  dragEv :: Event t ((CursorState, (Int,Int)), Drag2) <- drag2AttachOnStart V.BLeft (ffor2 (current cursor)  (current panPos) (,))
  let
    cursorDragEv c' = cursorDragStateEv (Just c') Nothing dragEv
    cursorDraggingEv c' = cursorDragStateEv (Just c') (Just Dragging) dragEv
    cursorStartEv c' = cursorDragStateEv (Just c') (Just DragStart) dragEv
    cursorEndEv c' = cursorDragStateEv (Just c') (Just DragEnd) dragEv

  -- ::panning::
  -- TODO make this so it doesn't trigger when you start drag off of this panel
  -- you could do this by checking if dragFrom is on the edges
  LBox (LPoint (V2 cx0 cy0)) (LSize (V2 cw0 ch0)) <- sample $ current (fmap renderedCanvas_box _canvasWidgetConfig_renderedCanvas_temp)
  pw0 <- displayWidth >>= sample . current
  ph0 <- displayHeight >>= sample . current
  let
    panFoldFn ((sx,sy), Drag2 (fromX, fromY) (toX, toY) _ _ _) _ = (sx + toX-fromX, sy + toY-fromY)

  -- panPos is position of upper left corner of canvas relative to screen
  panPos <- foldDyn panFoldFn (cx0 - (cw0-pw0)`div`2, cy0 - (ch0-ph0)`div`2) $ cursorDragEv CSPan

  -- ::tools::
  let
    boxPushFn ((px,py), Drag2 (fromX, fromY) _ _ _ _) = do
      pos <- return 0
      return $ (pos, SEltLabel "<box>" $ SEltBox $ SBox (LBox (LPoint (V2 (fromX-px) (fromY-py))) (LSize (V2 1 1))) def)
    newBoxEv = pushAlways boxPushFn $ cursorStartEv CSBox

  -- ::manipulators::
  let
    manipCfg = ManipulatorWidgetConfig {
        _manipulatorWigetConfig_selected = _selectionManager_selected _canvasWidgetConfig_selectionManager
        , _manipulatorWidgetConfig_panPos = current panPos
        -- TODO this is not correct
        , _manipulatorWidgetConfig_drag = dragEv
      }
  manipulatorW <- holdManipulatorWidget manipCfg

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
      , fmapLabelShow "manip" $ _manipulatorWidget_modify manipulatorW
      ]
    fixed 1 $ row $ do
      fixed 15 $ text $ fmap (\x -> "cursor: " <> show x) $ current cursor


  return CanvasWidget {
      -- TODO
      _canvasWidget_isManipulating = constDyn False
      , _canvasWidget_addSEltLabel = leftmostwarn "canvas add" [newBoxEv]
      , _canvasWidget_manipulatorWidget = manipulatorW
    }
