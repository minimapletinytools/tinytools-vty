{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Flow (
  flowMain
) where
import           Relude

import           Potato.Flow
import           Potato.Flow.Controller
import           Potato.Flow.Reflex.Vty.Attrs
import           Potato.Flow.Reflex.Vty.Canvas
import           Potato.Flow.Reflex.Vty.Layer
import           Potato.Flow.Reflex.Vty.Params
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Flow.Reflex.Vty.Tools
import           Potato.Flow.TestStates
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget


import           Control.Monad.Fix
import           Control.Monad.NodeId
import qualified Data.Aeson                         as Aeson
import qualified Data.Text.Encoding                 as T
import           Data.Time.Clock

import qualified Graphics.Vty                       as V
import qualified Graphics.Vty.Input.Events          as V
import           Reflex
import           Reflex.Potato.Helpers
import           Reflex.Vty





convertModifiers :: [V.Modifier] -> [KeyModifier]
convertModifiers = fmap $ \case
  V.MShift -> KeyModifier_Shift
  V.MCtrl  -> KeyModifier_Ctrl
  V.MMeta  -> KeyModifier_Ctrl
  V.MAlt ->  KeyModifier_Alt

convertKey :: V.Key -> Maybe KeyboardKey
convertKey = \case
  V.KEsc   -> Just KeyboardKey_Esc
  V.KChar c -> Just $   KeyboardKey_Char c
  V.KBS   -> Just KeyboardKey_Space
  V.KEnter   -> Just KeyboardKey_Return
  V.KLeft   -> Just KeyboardKey_Left
  V.KRight   -> Just KeyboardKey_Right
  V.KUp   -> Just KeyboardKey_Up
  V.KDown   -> Just KeyboardKey_Down
  V.KUpLeft   -> Nothing
  V.KUpRight -> Nothing
  V.KDownLeft -> Nothing
  V.KDownRight -> Nothing
  V.KCenter -> Nothing
  V.KFun _ -> Nothing
  V.KBackTab -> Nothing
  V.KPrtScr -> Nothing
  V.KPause -> Nothing
  V.KIns -> Nothing
  V.KHome -> Just KeyboardKey_Home
  V.KPageUp -> Nothing
  V.KDel -> Just KeyboardKey_Delete
  V.KEnd -> Just KeyboardKey_End
  V.KPageDown -> Nothing
  V.KBegin -> Nothing
  V.KMenu -> Nothing


convertButton :: V.Button -> Maybe MouseButton
convertButton = \case
  V.BLeft -> Just MouseButton_Left
  V.BMiddle -> Just MouseButton_Middle
  V.BRight -> Just MouseButton_Right
  V.BScrollUp -> Nothing
  V.BScrollDown -> Nothing

makeLMouseDataInputEv
  :: (Reflex t, MonadFix m, MonadHold t m)
  => XY
  -> Bool
  -> VtyWidget t m (Event t LMouseData)
makeLMouseDataInputEv offset isLayerMouse = mdo
  -- NOTE, must report both mouse down and up for any given drag or things will break
  -- button/mods is always the same button as mouse down, even if it changes during a drag
  inp <- input

  let
    mouseDownEv = fforMaybe inp $ \case
      V.EvMouseDown _ _ b mods -> Just (b, mods)
      _ -> Nothing
    -- tracks if last event was a mouse up
    mouseUpEv = fforMaybe inp $ \case
      V.EvMouseUp _ _ _ -> Just True
      V.EvMouseDown _ _ _ _ -> Just False
      _ -> Nothing
    mouseDownFoldFn (True, x) _  = x -- only updated button/mods just after a mouse up
    mouseDownFoldFn (False, _) x = x
  mouseUpDyn <- holdDyn True mouseUpEv
  mouseDownDyn <- foldDyn mouseDownFoldFn (V.BLeft,[]) (attach (current mouseUpDyn) mouseDownEv)

  return $ fforMaybe (attach (current mouseDownDyn) inp) $ \case
    (_, V.EvMouseDown _ _ V.BScrollUp _) -> Nothing
    (_, V.EvMouseDown _ _ V.BScrollDown _) -> Nothing
    (_, V.EvMouseDown x y b mods) -> case convertButton b of
      Nothing -> Nothing
      Just b' -> Just $ LMouseData {
        _lMouseData_position       = (V2 x y) + offset
        , _lMouseData_isRelease    = False
        , _lMouseData_button       = b'
        , _lMouseData_modifiers    = convertModifiers mods
        , _lMouseData_isLayerMouse = isLayerMouse
      }
    ((b,mods), V.EvMouseUp x y _) -> case convertButton b of
      Nothing -> Nothing
      Just b' -> Just $ LMouseData {
        _lMouseData_position       = (V2 x y) + offset
        , _lMouseData_isRelease    = True
        , _lMouseData_button       = b'
        , _lMouseData_modifiers    = convertModifiers mods
        , _lMouseData_isLayerMouse = isLayerMouse
      }


flowMain :: IO ()
flowMain = do
  mainWidget mainPFWidget

-- TODO
data MainPFTestOutput t = MainPFTestOutput {
  _mainPFTestOutput_leftPane    :: DynRegion t
  , _mainPFTestOutput_rightPane :: DynRegion t
}

{-
verifyInput :: (Reflex t, MonadHold t m) => Event t VtyEvent -> m (Event t VtyEvent)
verifyInput ev = do
  let
    foldDynMaybeFn = \case
      EvMouseDown _ _ _ _ -> Just True
      EvMouseUp _ _ _ -> Just False
      _ -> Nothing
  isMouseDownDyn <- foldDynMaybe foldDynMaybeFn False ev
  -- TODO check for invalid key presses based on mouse state
-}


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
        -- TODO don't do this, we need to break out individual changes instead so we can take advantage of holdUniqDyn
        , _pFWidgetCtx_pFState = fmap goatState_pFState $ _goatWidget_DEBUG_goatState everythingW
        , _pFWidgetCtx_initialPFState = pfstate_basic1
      }

    keyboardEv = fforMaybe inp $ \case
      V.EvKey k mods -> convertKey k >>= (\kbd -> return $ KeyboardData kbd (convertModifiers mods))
      V.EvPaste bs -> Just $ KeyboardData (KeyboardKey_Paste (T.decodeUtf8 bs)) []
      _ -> Nothing



  -- ::save/load file potato::
  -- TODO
  --performEvent_ $ ffor (_pfo_saved pfo) $ \spf -> do
  --  liftIO $ Aeson.encodeFile "potato.flow" spf

  --loadFileEv <- performEvent $ ffor postBuildEv $ \_ -> do
  --  liftIO $ Aeson.decodeFileStrict "potato.flow"

  let
    goatWidgetConfig = GoatWidgetConfig {
        _goatWidgetConfig_initialState = _pFWidgetCtx_initialPFState pfctx
        , _goatWidgetConfig_load = never

        -- canvas direct input
        , _goatWidgetConfig_mouse = never
        , _goatWidgetConfig_keyboard = never

        , _goatWidgetConfig_selectTool = _toolWidget_setTool toolsW

        -- debugging/deprecated stuff
        , _goatWidgetConfig_setDebugLabel = never
      }

  everythingW <- holdGoatWidget goatWidgetConfig


  -- main panels
  let
    leftPanel = col $ do
      fixed 5 $ debugStream [
        never
        ]
      tools' <- fixed 10 $ holdToolsWidget $  ToolWidgetConfig {
          _toolWidgetConfig_pfctx = pfctx
          , _toolWidgetConfig_tool =  _goatWidget_tool everythingW
        }

      -- TODO pass out layer mouse from here
      layers' <- stretch $ holdLayerWidget $ LayerWidgetConfig {
            _layerWidgetConfig_pfctx              = pfctx
            , _layerWidgetConfig_layers = _goatWidget_layers everythingW

            , _layerWidgetConfig_selection = _goatWidget_selection  everythingW
          }
      params' <- fixed 5 $ holdParamsWidget $ ParamsWidgetConfig {
          _paramsWidgetConfig_pfctx = pfctx
        }
      return (layers', tools', params')

    -- TODO pass out canvas mouse from here
    rightPanel = holdCanvasWidget $ CanvasWidgetConfig {
        _canvasWidgetConfig_pfctx = pfctx
        , _canvasWidgetConfig_pan = _goatWidget_pan everythingW
        , _canvasWidgetConfig_broadPhase = _goatWidget_broadPhase everythingW
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
