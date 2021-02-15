{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Flow (
  flowMain
) where
import           Relude

import           Potato.Flow
import           Potato.Flow.Controller
import           Potato.Flow.TestStates
import           Potato.Flow.Vty.Attrs
import           Potato.Flow.Vty.Canvas
import           Potato.Flow.Vty.Info
import           Potato.Flow.Vty.Input
import           Potato.Flow.Vty.Layer
import           Potato.Flow.Vty.Params
import           Potato.Flow.Vty.PFWidgetCtx
import           Potato.Flow.Vty.Tools
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget.Popup
import           Potato.Reflex.Vty.Widget
import qualified Potato.Reflex.Vty.Host

import           Control.Concurrent
import           Control.Monad.Fix
import           Control.Monad.NodeId
import qualified Data.Aeson                        as Aeson
import qualified Data.Aeson.Encode.Pretty as PrettyAeson
import           Data.Maybe
import           Data.Monoid                       (Any)
import qualified Data.Text.Encoding                as T
import qualified Data.Text.Lazy                    as LT
import qualified Data.Text.Lazy.Encoding           as LT
import qualified Data.Text.IO as T
import           Data.Time.Clock
import qualified Data.ByteString.Lazy as LBS
import Data.These

import           Network.HTTP.Simple

import qualified Graphics.Vty                      as V
import qualified Graphics.Vty.Input.Events         as V
import qualified Graphics.Vty.UnicodeWidthTable.IO as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Potato.Helpers
import           Reflex.Vty


-- TODO move all this into Potato.Reflex.Vty.Host or something whatever
-- | Sets up the top-level context for a 'VtyWidget' and runs it with that context
potatoMainWidgetWithHandle
  :: V.Vty
  -> (forall t m. (MonadVtyApp t m, MonadNodeId m) => VtyWidget t m (Event t ()))
  -> IO ()
potatoMainWidgetWithHandle vty child =
  Potato.Reflex.Vty.Host.runVtyAppWithHandle vty $ \dr0 inp -> do
    size <- holdDyn dr0 $ fforMaybe inp $ \case
      V.EvResize w h -> Just (w, h)
      _ -> Nothing
    let inp' = fforMaybe inp $ \case
          V.EvResize {} -> Nothing
          x -> Just x
    let ctx = VtyWidgetCtx
          { _vtyWidgetCtx_width = fmap fst size
          , _vtyWidgetCtx_height = fmap snd size
          , _vtyWidgetCtx_input = inp'
          , _vtyWidgetCtx_focus = constDyn True
          }
    (shutdown, images) <- runNodeIdT $ runVtyWidget ctx $ do
      tellImages . ffor (current size) $ \(w, h) -> [V.charFill V.defAttr ' ' w h]
      child
    return $ Potato.Reflex.Vty.Host.VtyResult
      { _vtyResult_picture = fmap (V.picForLayers . reverse) images
      , _vtyResult_shutdown = shutdown
      }

-- | run a VtyWidget using term width map written to disk with write-term-width for the current terminal
-- uses default if the file does not exist
potatoMainWidget
  :: (forall t m. (MonadVtyApp t m, MonadNodeId m) => VtyWidget t m (Event t ()))
  -> IO ()
potatoMainWidget child = do
  cfg'' <- V.standardIOConfig
  let
    mTermName = V.termName cfg''
    widthMapFile = fromJust mTermName <> "_termwidthfile"
  putStrLn $ "setting up vty for term: " <> show mTermName
  let
    cfg' = cfg'' { V.mouseMode = Just True }
    cfg = case mTermName of
      Nothing -> cfg'
      Just termName -> cfg' {
          V.allowCustomUnicodeWidthTables = Just True
          , V.termWidthMaps = [(fromJust mTermName, widthMapFile)]
        }
  vty <- V.mkVty cfg
  potatoMainWidgetWithHandle vty child


-- | tick once (redraw widgets) upon event firing
tickOnEvent :: (Reflex t, Adjustable t m) => Event t a -> m ()
tickOnEvent ev = void $ runWithReplace (return ()) (ev $> return ())


flowMain :: IO ()
flowMain = do
  --mainWidget mainPFWidget
  potatoMainWidget mainPFWidget

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

fetchMOTD :: IO Text
fetchMOTD = do
  resp <- httpLBS "https://raw.githubusercontent.com/pdlla/potato-flow-vty/potato/MOTD.txt"
  return $ LT.toStrict $ LT.decodeUtf8 (getResponseBody resp)

fetchMOTDAsync :: forall t m. (MonadWidget t m) => Event t () -> VtyWidget t m (Event t Text)
fetchMOTDAsync ev = performEventAsync $ ffor ev $ const $ \f -> liftIO $ do
    forkIO $ do
      motd <- fetchMOTD
      f motd
    return ()

-- NOTE, this will query welcome message each time you recreate this
welcomeWidget :: forall t m. (MonadWidget t m)
  => VtyWidget t m (Event t ())
welcomeWidget = do
  postBuildEv <- getPostBuild
  welcomeMessageEv <- fetchMOTDAsync postBuildEv
  welcomeMessageDyn <- holdDyn "loading..." welcomeMessageEv
  boxTitle (constant def) "😱😱😱" $ do
    col $ do
      stretch $ text (current welcomeMessageDyn)
      fixed 3 $ textButton def (constant "bye")


-- | toggle the focus of a widget
-- also forces unfocused widget to ignore mouse inputs
focusWidgetNoMouse :: forall t m a. (MonadWidget t m)
  => Dynamic t Bool -- ^ whether widget should be focused or not, note events that change focus are not captured!
  -> VtyWidget t m a
  -> VtyWidget t m a
focusWidgetNoMouse focus child = VtyWidget $ do
  ctx <- lift ask
  let ctx' = VtyWidgetCtx {
      _vtyWidgetCtx_input = gate (current focus) (_vtyWidgetCtx_input ctx)
      , _vtyWidgetCtx_focus = liftA2 (&&) (_vtyWidgetCtx_focus ctx) focus
      , _vtyWidgetCtx_width = _vtyWidgetCtx_width ctx
      , _vtyWidgetCtx_height = _vtyWidgetCtx_height ctx
    }
  (result, images) <- lift . lift $ runVtyWidget ctx' child
  tellImages images
  return result

-- | block all or some input events, always focused if parent is focused
captureInputEvents :: forall t m a. (MonadWidget t m)
  => These (Event t ()) (Behavior t Bool) -- ^ Left ev is event indicating input should be capture. Right beh is behavior gating input (true means captured)
  -> VtyWidget t m a
  -> VtyWidget t m a
captureInputEvents capture child = VtyWidget $ do
  ctx <- lift ask
  let
    (ev, beh) = fromThese never (constant False) capture
    ctx' = VtyWidgetCtx {
        _vtyWidgetCtx_input = difference (gate (fmap not beh) (_vtyWidgetCtx_input ctx)) ev
        , _vtyWidgetCtx_focus = liftA2 (&&) (_vtyWidgetCtx_focus ctx) $ constDyn True
        , _vtyWidgetCtx_width = _vtyWidgetCtx_width ctx
        , _vtyWidgetCtx_height = _vtyWidgetCtx_height ctx
      }
  (result, images) <- lift . lift $ runVtyWidget ctx' child
  tellImages images
  return result

mainPFWidget :: forall t m. (MonadWidget t m)
  => VtyWidget t m (Event t ())
mainPFWidget = mdo
  -- external inputs
  currentTime <- liftIO $ getCurrentTime

  -- note tickEv triggers 2 ticks
  --tickEv <- tickLossy 1 currentTime
  --ticks <- foldDyn (+) (0 :: Int) (fmap (const 1) tickEv)

  flowInput <- input
  postBuildEv <- getPostBuild

  -- force redraw of handles, needed in some cases
  tickOnEvent flowInput

  let
    pfctx = PFWidgetCtx {
        _pFWidgetCtx_attr_default = constDyn lg_default
        , _pFWidgetCtx_attr_manipulator = constDyn lg_manip
        -- TODO don't do this, we need to break out individual changes instead so we can take advantage of holdUniqDyn
        , _pFWidgetCtx_goatWidget = everythingW
        , _pFWidgetCtx_pFState = fmap goatState_pFState $ _goatWidget_DEBUG_goatState everythingW
        , _pFWidgetCtx_initialPFState = pfstate_basic1
      }


  -- ::save/load file potato::
  -- TODO
  --performEvent_ $ ffor (_pfo_saved pfo) $ \spf -> do
  --  liftIO $ Aeson.encodeFile "potato.flow" spf

  mLoadFileEv <- performEvent $ ffor postBuildEv $ \_ -> do
    mspf :: Maybe SPotatoFlow <- liftIO $ Aeson.decodeFileStrict "potato.flow"
    return $ mspf >>= return . (,emptyControllerMeta)

  let
    goatWidgetConfig = GoatWidgetConfig {
        _goatWidgetConfig_initialState = _pFWidgetCtx_initialPFState pfctx
        , _goatWidgetConfig_load = fmapMaybe id mLoadFileEv

        -- canvas direct input
        , _goatWidgetConfig_mouse = leftmostWarn "mouse" [_layerWidget_mouse layersW, _canvasWidget_mouse canvasW]
        , _goatWidgetConfig_keyboard = keyboardEv

        , _goatWidgetConfig_selectTool = _toolWidget_setTool toolsW
        , _goatWidgetConfig_paramsEvent = _paramsWidget_paramsEvent paramsW
        , _goatWidgetConfig_canvasSize = never

        -- debugging/deprecated stuff
        , _goatWidgetConfig_setDebugLabel = never
      }

  everythingW <- holdGoatWidget goatWidgetConfig


  -- main panels
  let
    leftPanel = col $ do
      fixed 1 $ row $ do
        stretch $ do
          text "save"
          click <- mouseDown V.BLeft
          let saveEv = tag (current $ _goatWidget_DEBUG_goatState everythingW) click
          performEvent_ $ ffor saveEv $ \gs -> do
             let spf = pFState_to_sPotatoFlow . _pFWorkspace_pFState . _goatState_pFWorkspace $ gs
             --liftIO $ Aeson.encodeFile "potato.flow" spf
             liftIO $ LBS.writeFile "potato.flow" $ PrettyAeson.encodePretty spf
        stretch $ text "|"
        stretch $ do
          text "print"
          click <- mouseDown V.BLeft
          let saveEv = tag (current $ _goatWidget_renderedCanvas everythingW) click
          performEvent_ $ ffor saveEv $ \rc -> do
             let t = renderedCanvasToText rc
             liftIO $ T.writeFile "potato.txt" t
      fixed 5 $ debugStream [
        never
        ]
      tools' <- fixed 10 $ holdToolsWidget $  ToolWidgetConfig {
          _toolWidgetConfig_pfctx = pfctx
          , _toolWidgetConfig_tool =  _goatWidget_tool everythingW
        }

      -- TODO Layout stuff messes up your mouse assumptions. You need to switch Layout to use pane2 D:
      layers' <- stretch $ holdLayerWidget $ LayerWidgetConfig {
            _layerWidgetConfig_pfctx              = pfctx
            , _layerWidgetConfig_layers = _goatWidget_layers everythingW

            , _layerWidgetConfig_selection = _goatWidget_selection  everythingW
          }
      _ <- fixed 5 $ holdInfoWidget $ InfoWidgetConfig {
          _infoWidgetConfig_selection = _goatWidget_selection everythingW
        }
      params' <- fixed 10 $ holdParamsWidget $ ParamsWidgetConfig {
          _paramsWidgetConfig_pfctx = pfctx
        }
      return (layers', tools', params')

    rightPanel = holdCanvasWidget $ CanvasWidgetConfig {
        _canvasWidgetConfig_pfctx = pfctx
        , _canvasWidgetConfig_pan = _goatWidget_pan everythingW
        , _canvasWidgetConfig_broadPhase = _goatWidget_broadPhase everythingW
        , _canvasWidgetConfig_renderedCanvas = _goatWidget_renderedCanvas everythingW
        , _canvasWidgetConfig_canvas = _goatWidget_canvas everythingW
        , _canvasWidgetConfig_handles = _goatWidget_handlerRenderOutput everythingW
      }

  (keyboardEv, ((layersW, toolsW, paramsW), canvasW)) <- captureInputEvents (That inputCapturedByPopupBeh) $ do
    inp <- input
    stuff <- splitHDrag 35 (fill '*') leftPanel rightPanel

    -- TODO capture from params or whatever
    kb <- captureInputEvents (This never) $ do
      inp <- input
      return $ fforMaybe inp $ \case
        V.EvKey k mods -> convertKey k >>= (\kbd -> return $ KeyboardData kbd (convertModifiers mods))
        V.EvPaste bs -> Just $ KeyboardData (KeyboardKey_Paste (T.decodeUtf8 bs)) []
        _ -> Nothing

    return (kb, stuff)


  -- render various popups
  (_, popupStateDyn1) <- popupPaneSimple def (postBuildEv $> welcomeWidget)

  let
    inputCapturedByPopupBeh = current . fmap getAny . mconcat . fmap (fmap Any) $ [popupStateDyn1]



  -- handle escape event
  return $ fforMaybe flowInput $ \case
    V.EvKey (V.KChar 'q') [V.MCtrl] -> Just ()
    _ -> Nothing
