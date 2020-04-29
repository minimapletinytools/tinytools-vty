{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo   #-}

{-# OPTIONS_GHC -threaded #-}

module Flow (
  flowMain
) where
import           Relude

import           Potato.Flow
import           Potato.Flow.Testing
import           Reflex.Potato.Helpers
import Potato.Reflex.Vty.Helpers
import Potato.Reflex.Vty.Widget


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Data.Functor.Misc
import qualified Data.Map                as Map
import           Data.Maybe
import qualified Data.Text               as T
import qualified Data.Text.Zipper        as TZ
import           Data.Time.Clock
import qualified Graphics.Vty            as V
import           Reflex
import           Reflex.Class.Switchable
import           Reflex.Network
import           Reflex.Vty
import qualified Text.Show

data CursorState = CSPan | CSSelecting | CSBox deriving (Eq)

instance Show CursorState where
  show CSPan = "PAN"
  show CSSelecting = "SELECT"
  show CSBox = "BOX"

dynLBox_to_dynRegion :: (Reflex t) => Dynamic t LBox -> DynRegion t
dynLBox_to_dynRegion dlb = r where
  x' = flip fmap dlb $ \(LBox (LPoint (V2 x y)) (LSize (V2 w h))) -> x
  y' = flip fmap dlb $ \(LBox (LPoint (V2 x y)) (LSize (V2 w h))) -> y
  w' = flip fmap dlb $ \(LBox (LPoint (V2 x y)) (LSize (V2 w h))) -> w
  h' = flip fmap dlb $ \(LBox (LPoint (V2 x y)) (LSize (V2 w h))) -> h
  r = DynRegion x' y' w' h'

translate_dynRegion :: (Reflex t) => Dynamic t (Int, Int) -> DynRegion t -> DynRegion t
translate_dynRegion pos dr = dr {
    _dynRegion_left = liftA2 (+) (_dynRegion_left dr) (fmap fst pos)
    , _dynRegion_top = liftA2 (+) (_dynRegion_top dr) (fmap snd pos)
  }


data CanvasWidgetConfig t = CanvasWidgetConfig {
  _canvasWidgetConfig_tool :: Event t Tool
  , _canvasWidgetConfig_canvas_temp :: Dynamic t Canvas
}

data CanvasWidget t = CanvasWidget {
  _canvasWidget_isManipulating :: Dynamic t Bool
}
canvasWidget :: forall t m. (Reflex t, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => CanvasWidgetConfig t
  -> VtyWidget t m (CanvasWidget t)
canvasWidget CanvasWidgetConfig {..} = mdo
  inp <- input

  -- ::cursor::
  let
    escEv = fforMaybe inp $ \case
      V.EvKey (V.KEsc) [] -> Just ()
      _ -> Nothing
  cursor <- holdDyn CSSelecting $ leftmost [fmap tool_cursorState _canvasWidgetConfig_tool, CSSelecting <$ escEv]

  -- ::panning::
  -- TODO make this so it doesn't trigger when you start drag off of this panel
  -- you could do this by checking if dragFrom is on the edges
  dragEv :: Event t ((Int,Int), Drag2) <- drag2AttachOnStart V.BLeft (current panePos)
  LBox (LPoint (V2 cx0 cy0)) (LSize (V2 cw0 ch0)) <- sample $ current (fmap canvas_box _canvasWidgetConfig_canvas_temp)
  pw0 <- displayWidth >>= sample . current
  ph0 <- displayHeight >>= sample . current
  let
    panEv = fmapMaybe (\(c,d) -> if c == CSPan then Just d else Nothing) $  attach (current cursor) dragEv
    panFoldFn ((sx,sy), Drag2 (fromX, fromY) (toX, toY) _ _ _) _ = (sx + toX-fromX, sy + toY-fromY)
  panePos <- foldDyn panFoldFn (cx0 - (cw0-pw0)`div`2, cy0 - (ch0-ph0)`div`2) panEv

  -- ::tools::


  -- ::draw the canvas::
  -- TODO make this efficient -_-
  let
    canvasRegion = translate_dynRegion panePos $ dynLBox_to_dynRegion (fmap canvas_box _canvasWidgetConfig_canvas_temp)
  fill '░'
  pane canvasRegion (constDyn True) $ do
    text $ current (fmap canvasToText _canvasWidgetConfig_canvas_temp)

  -- ::info pane::
  col $ do
    fixed 2 $ debugStream [fmapLabelShow "drag" dragEv, fmapLabelShow "input" inp, fmapLabelShow "cursor" (updated cursor)]
    fixed 1 $ row $ do
      fixed 15 $ text $ fmap (\x -> "cursor: " <> show x) $ current cursor


  return CanvasWidget {
      _canvasWidget_isManipulating = constDyn False
    }


data LayerWidgetConfig t = LayerWidgetConfig {
  _layerWidgetConfig_temp_sEltTree :: Dynamic t SEltTree
}

data LayerWidget t = LayerWidget {
  _layerWidget_potatoAdd :: Event t (LayerPos, SEltLabel)
  , _layerWidget_select  :: Event t LayerPos
}

layerWidget :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
layerWidget LayerWidgetConfig {..} = do
  pw <- displayWidth
  ph <- displayHeight
  addButton <- col $ do
    fixed 1 $ debugFocus
    fixed 1 $ text . current . fmap (show . length)$ _layerWidgetConfig_temp_sEltTree
    addButton <- fixed 3 $ textButtonStatic def "add"
    stretch $ col $ simpleList (fmap (zip [0..]) _layerWidgetConfig_temp_sEltTree) $ \ds -> do
      fixed 1 $ text $ current $ fmap (_sEltLabel_name . snd) ds
    return addButton
  return LayerWidget {
    _layerWidget_potatoAdd = fmap (const (0, SEltLabel "meow" (SEltBox simpleSBox))) addButton
    , _layerWidget_select = never
  }

data Tool = TPan | TBox | TNothing deriving (Eq, Show)

tool_cursorState :: Tool -> CursorState
tool_cursorState TPan = CSPan
tool_cursorState TBox = CSBox
tool_cursorState _ = CSSelecting

data ToolWidget t = ToolWidget {
  _toolWidget_tool :: Event t Tool
}

toolsWidget :: forall t m. (Reflex t, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => VtyWidget t m (ToolWidget t)
toolsWidget = row $ do
  pan <- fixed 5 $ textButton def "PAN"
  box <- fixed 5 $ textButton def "BOX"
  return ToolWidget {
    _toolWidget_tool = leftmost [TPan <$ pan, TBox <$ box]
  }

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
  ticks <- foldDyn (+) 0 (fmap (const 1) tickEv)
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
        _pfc_addElt     = _layerWidget_potatoAdd layers
        , _pfc_removeElt  = never
        , _pfc_manipulate = never
        , _pfc_undo       = undoEv
        , _pfc_redo       = redoEv
        , _pfc_save = never
      }
  pfo <- holdPF pfc
  potatoUpdated <- delayEvent $ _pfo_potato_changed pfo
  let
    layerTree = _pfo_layers pfo
    stateUpdated = tag (_pfo_potato_state pfo) potatoUpdated
    selts = fmap (fmap (_sEltLabel_sElt)) $ _pfo_potato_state pfo
  treeDyn <- holdDyn [] stateUpdated
  canvas <- foldDyn potatoRender (emptyCanvas (LBox (LPoint (V2 0 0)) (LSize (V2 40 30))))
    $ tag selts potatoUpdated

  -- main panels
  let
    leftPanel = col $ do
      fixed 2 $ debugStream [fmapLabelShow "tool" (_toolWidget_tool tools)]
      tools' <- fixed 3 $ toolsWidget
      layers' <- stretch $ layerWidget $ LayerWidgetConfig treeDyn
      params' <- fixed 5 $ paramWidget
      return (layers', tools', params')

    rightPanel = canvasWidget $ CanvasWidgetConfig
      (_toolWidget_tool tools)
      canvas

  ((layers, tools, _), _) <- splitHDrag 35 (fill '*') leftPanel rightPanel

  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
