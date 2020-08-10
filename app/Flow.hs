{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Flow (
  flowMain
) where
import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.Attrs
import           Potato.Flow.Reflex.Vty.Canvas
import           Potato.Flow.Reflex.Vty.Layer
import           Potato.Flow.Reflex.Vty.Params
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Flow.Reflex.Vty.Selection
import           Potato.Flow.Reflex.Vty.Tools
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget


import           Control.Monad.Fix
import           Control.Monad.NodeId
import qualified Data.Aeson                         as Aeson
import           Data.Time.Clock

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Potato.Helpers
import           Reflex.Vty


flowMain :: IO ()
flowMain = do
  mainWidget mainPFWidget

-- TODO
data MainPFTestOutput t = MainPFTestOutput {
  _mainPFTestOutput_leftPane    :: DynRegion t
  , _mainPFTestOutput_rightPane :: DynRegion t
}


mainPFWidget :: forall t m. (Reflex t, MonadHold t m, MonadFix m, NotReady t m, Adjustable t m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadNodeId m, MonadIO (Performable m), MonadIO m)
  => VtyWidget t m (Event t ())
mainPFWidget = mdo
  -- external inputs
  currentTime <- liftIO $ getCurrentTime
  tickEv <- tickLossy 1 currentTime
  ticks <- foldDyn (+) (0 :: Int) (fmap (const 1) tickEv)
  inp <- input
  postBuildEv <- getPostBuild

  let
    pfctx = PFWidgetCtx {
        _pFWidgetCtx_attr_default = constDyn lg_default
        , _pFWidgetCtx_attr_manipulator = constDyn lg_manip
        , _pFWidgetCtx_ev_cancel        = fforMaybe inp $ \case
          V.EvKey (V.KEsc) [] -> Just ()
          _ -> Nothing
        , _pFWidgetCtx_ev_input = inp
        , _pFWidgetCtx_pfo = pfo
      }

  -- potato flow stuff
  let
    -- TODO disable when manipulating _canvasWidget_isManipulating
    undoEv = fforMaybe inp $ \case
      V.EvKey (V.KChar 'z') [V.MCtrl] -> Just ()
      _ -> Nothing
    redoEv = fforMaybe inp $ \case
      V.EvKey (V.KChar 'y') [V.MCtrl] -> Just ()
      _ -> Nothing
    saveEv = fforMaybe inp $ \case
      V.EvKey (V.KChar 's') [V.MCtrl] -> Just ()
      _ -> Nothing

    pfc = PFConfig {
        _pfc_addElt     = doNewElt
        , _pfc_deleteElts  = never
        , _pfc_manipulate = leftmostAssert "manipulate" [doManipulate, _layerWidget_changeName layersW]
        , _pfc_undo       = leftmostAssert "undo" [undoEv, undoBeforeManipulate, undoBeforeNewAdd, _canvasWidget_undo canvasW]
        , _pfc_redo       = redoEv
        , _pfc_save = saveEv
        , _pfc_load = never --fmapMaybe id loadFileEv
        , _pfc_resizeCanvas = never
        , _pfc_addFolder = never
        , _pfc_moveElt = never
        , _pfc_copy = never
        , _pfc_paste = never
      }
  pfo <- holdPF pfc

  -- ::save/load file potato::
  performEvent_ $ ffor (_pfo_saved pfo) $ \spf -> do
    liftIO $ Aeson.encodeFile "potato.flow" spf

  --loadFileEv <- performEvent $ ffor postBuildEv $ \_ -> do
  --  liftIO $ Aeson.decodeFileStrict "potato.flow"

  -- ::selection stuff::
  selectionManager <- holdSelectionManager
    SelectionManagerConfig {
      _selectionManagerConfig_pfctx = pfctx
      , _selectionManagerConfig_newElt_layerPos = doNewElt
      , _selectionManagerConfig_select = _layerWidget_select layersW
      , _selectionManagerConfig_selectByREltId = _canvasWidget_select canvasW
    }

  -- main panels
  let
    leftPanel = col $ do
      fixed 5 $ debugStream [
        never
        , fmapLabelShow "doManipulate" $ doManipulate
        , fmapLabelShow "doNewElt" $ doNewElt
        --, fmapLabelShow "undo" $ _canvasWidget_addSEltLabel canvasW
        --, fmapLabelShow "input" inp
        ]
      tools' <- fixed 10 $ holdToolsWidget $  ToolWidgetConfig {
          _toolWidgetConfig_pfctx = pfctx
          , _toolWidgetConfig_consumingKeyboard = consumingKeyboard
          , _toolWidgetConfig_setDefault = never --void $ _canvasWidget_addSEltLabel canvasW
        }

      layers' <- stretch $ holdLayerWidget $ LayerWidgetConfig {
            _layerWidgetConfig_pfctx              = pfctx
            , _layerWidgetConfig_selectionManager = selectionManager
          }
      params' <- fixed 5 $ holdParamsWidget $ ParamsWidgetConfig {
          _paramsWidgetConfig_pfctx = pfctx
        }
      return (layers', tools', params')

    rightPanel = holdCanvasWidget $ CanvasWidgetConfig {
        _canvasWidgetConfig_pfctx = pfctx
        , _canvasWidgetConfig_tool = (_toolWidget_tool toolsW)
        , _canvasWidgetConfig_pfo = pfo
        , _canvasWidgetConfig_selectionManager = selectionManager
      }

  ((layersW, toolsW, paramsW), canvasW) <- splitHDrag 35 (fill '*') leftPanel rightPanel

  -- prep consuming keyboard behavior
  let
    consumingKeyboard = ffor2 (_canvasWidget_consumingKeyboard canvasW) (_paramsWidget_consumingKeyboard paramsW) (||)

  -- TODO place new elt at end of current selection
  -- prep newAdd event
  -- MANY FRAMES
  let
    undoBeforeNewAdd = fmapMaybe (\x -> if fst x then Just () else Nothing) $ _canvasWidget_addSEltLabel canvasW
    doNewElt' = fmap snd $ _canvasWidget_addSEltLabel canvasW
  doNewElt <- sequenceEvents undoBeforeNewAdd doNewElt'


  -- prep manipulate event
  -- MANY FRAMES via ManipulatorWidget (ok, as undo manipulation currently is 1 frame in potato-flow, and the previous operation to undo is always a manipulate operation)
  let
    undoBeforeManipulate = fmapMaybe (\x -> if fst x then Just () else Nothing) $ _canvasWidget_modify canvasW
    doManipulate' = fmap snd $ _canvasWidget_modify canvasW
  doManipulate <- sequenceEvents undoBeforeManipulate doManipulate'

  -- handle escape events
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
