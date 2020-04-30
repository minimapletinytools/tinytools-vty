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
import           Potato.Flow.Testing
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget


import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Data.Dependent.Sum                 (DSum ((:=>)))
import qualified Data.IntMap.Strict                 as IM
import qualified Data.List                          as L
import           Data.Time.Clock
import           Data.Tuple.Extra
import qualified Text.Show

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Network
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
        _pfc_addElt     = _canvasWidget_addSEltLabel canvasW
        , _pfc_removeElt  = never
        , _pfc_manipulate = doManipulate
        , _pfc_undo       = leftmost [undoEv, undoBeforeManipulate]
        , _pfc_redo       = redoEv
        , _pfc_save = never
      }
  pfo <- holdPF pfc

  -- ::prep PFO data::
  -- delaying one frame ensures we can sample most recent behaviors
  potatoUpdated <- delayEvent $ _pfo_potato_changed pfo
  let
    layerTree = _pfo_layers pfo
    stateUpdated = tag (_pfo_potato_state pfo) potatoUpdated
    selts = fmap (fmap (_sEltLabel_sElt . thd3)) $ _pfo_potato_state pfo
  superTreeDyn <- holdDyn [] stateUpdated
  canvas <- foldDyn potatoRender (emptyCanvas (LBox (LPoint (V2 0 0)) (LSize (V2 100 40))))
    $ tag selts potatoUpdated

  -- ::selection stuff::
  selectionManager <- holdSelectionManager
    SelectionManagerConfig {
      _selectionManagerConfig_newElt_layerPos = _canvasWidget_addSEltLabel canvasW
      , _selectionManagerConfig_sEltLayerTree = layerTree
      , _selectionManagerConfig_select = never
    }


  -- main panels
  let
    leftPanel = col $ do
      fixed 2 $ debugStream [fmapLabelShow "tool" (_toolWidget_tool tools)]
      tools' <- fixed 3 $ holdToolsWidget
      layers' <- stretch $ holdLayerWidget $ LayerWidgetConfig superTreeDyn selectionManager
      params' <- fixed 5 $ paramWidget
      return (layers', tools', params')

    rightPanel = holdCanvasWidget $ CanvasWidgetConfig
      (_toolWidget_tool tools)
      canvas
      selectionManager

  ((layersW, tools, _), canvasW) <- splitHDrag 35 (fill '*') leftPanel rightPanel

  -- prep manipulate event
  let
    manipulatorW = _canvasWidget_manipulatorWidget canvasW
    undoBeforeManipulate = fmapMaybe (\x -> if fst x then Just () else Nothing) $ _manipulatorWidget_modify manipulatorW
    doManipulate' = fmap snd $ _manipulatorWidget_modify manipulatorW
  doManipulate <- sequenceEvents undoBeforeManipulate doManipulate'

  -- handle escape events
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
