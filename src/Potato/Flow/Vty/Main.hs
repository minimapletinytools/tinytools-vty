{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Main (
  potatoMainWidget
  , mainPFWidget
  , mainPFWidgetWithBypass
  , MainPFWidgetConfig(..)
  , somedefaultpfcfg
  , tinytoolsConfigDir
) where
import           Relude


import           Potato.Flow
import           Potato.Flow.TestStates
import           Potato.Flow.Vty.Canvas
import           Potato.Flow.Vty.Input
import           Potato.Flow.Vty.Layer
import           Potato.Flow.Vty.Params
import           Potato.Flow.Vty.PotatoReader
import           Potato.Flow.Vty.Tools
import           Potato.Flow.Vty.Left
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget.Popup
import           Potato.Reflex.Vty.Widget
import qualified Potato.Reflex.Vty.Host
import Potato.Flow.Vty.SaveAsWindow
import Potato.Flow.Vty.OpenWindow
import Potato.Flow.Vty.Alert
import Potato.Flow.Vty.AppKbCmd
import Potato.Flow.Vty.Attrs

import System.Console.ANSI (hSetTitle)
import qualified System.FilePath as FP
import qualified System.Directory as FP

import           Control.Concurrent
import           Control.Monad.NodeId
import Control.Exception (handle)
import qualified Data.Aeson                        as Aeson
import qualified Data.Aeson.Encode.Pretty as PrettyAeson
import           Data.Maybe
import           Data.Default
import qualified Data.Text.Encoding                as T
import qualified Data.Text.Lazy                    as LT
import qualified Data.Text.Lazy.Encoding           as LT
import qualified Data.Text.IO as T
import           Data.Time.Clock
import qualified Data.ByteString.Lazy as LBS
import Data.These


import           Network.HTTP.Simple

import qualified Graphics.Vty                      as V
--import qualified Graphics.Vty.UnicodeWidthTable.IO as V
import           Reflex
import           Reflex.Potato.Helpers
import           Reflex.Vty



-- TODO move all this into Potato.Reflex.Vty.Host or something whatever
-- | Sets up the top-level context for a 'VtyWidget' and runs it with that context
potatoMainWidgetWithHandle
  :: V.Vty
  -> (forall t m. (Potato.Reflex.Vty.Host.MonadVtyApp t m
      , HasImageWriter t m
      , MonadNodeId m
      , HasDisplayRegion t m
      , HasFocusReader t m
      , HasInput t m
      , HasTheme t m) => m (Event t ()))
  -> IO ()
potatoMainWidgetWithHandle vty child =
  Potato.Reflex.Vty.Host.runVtyAppWithHandle vty $ \dr0 inp -> do
    size <- holdDyn dr0 $ fforMaybe inp $ \case
      V.EvResize w h -> Just (w, h)
      _ -> Nothing
    let inp' = fforMaybe inp $ \case
          V.EvResize {} -> Nothing
          x -> Just x
    (shutdown, imgs) <- runThemeReader (constant lg_default) $
      runFocusReader (pure True) $
        runDisplayRegion (fmap (\(w, h) -> Region 0 0 w h) size) $
          runImageWriter $
            runNodeIdT $
              runInput inp' $ do
                tellImages . ffor (current size) $ \(w, h) -> [V.charFill lg_default ' ' w h]
                child
    return $ Potato.Reflex.Vty.Host.VtyResult
      { _vtyResult_picture = fmap (V.picForLayers . reverse) imgs
      , _vtyResult_shutdown = shutdown
      }


tinytoolsConfigDir :: IO FP.FilePath
tinytoolsConfigDir = do
  homedir <- FP.getHomeDirectory
  return $ (homedir FP.</> ".tinytools/")


-- | run a VtyWidget using term width map written to disk with write-term-width for the current terminal
-- uses default if the file does not exist
potatoMainWidget
  :: (forall t m. (Potato.Reflex.Vty.Host.MonadVtyApp t m
      , HasImageWriter t m
      , MonadNodeId m
      , HasDisplayRegion t m
      , HasFocusReader t m
      , HasInput t m
      , HasTheme t m) => m (Event t ()))
  -> IO ()
potatoMainWidget child = do
  cfg'' <- V.standardIOConfig
  configDir <- tinytoolsConfigDir
  let
    mTermName = V.termName cfg''
    widthMapFile = case mTermName of
      Nothing -> ""
      Just termName -> configDir FP.</> (termName <> "_termwidthfile")
  doesWidthMapFileExist <- FP.doesFileExist widthMapFile
  if doesWidthMapFileExist
    then putStrLn $ "attempting to load unicode width table file " <> widthMapFile
    else putStrLn $ "could not find unicode width table file " <> widthMapFile <> " please run --widthtable to generate unicode width table file"
  let
    cfg' = cfg'' { V.mouseMode = Just True }
    cfg = if doesWidthMapFileExist
      then cfg' {
          V.allowCustomUnicodeWidthTables = Just True
          , V.termWidthMaps = [(fromJust mTermName, widthMapFile)]
        }
      else cfg'
  vty <- V.mkVty cfg
  potatoMainWidgetWithHandle vty child


-- | tick once (redraw widgets) upon event firing
tickOnEvent :: (Adjustable t m) => Event t a -> m ()
tickOnEvent ev = void $ runWithReplace (return ()) (ev $> return ())

-- | TODO move to ReflexHelpers
fanMaybe :: (Reflex t) => Event t (Maybe a) -> (Event t a, Event t ())
fanMaybe ev = (fmapMaybe id ev, fmapMaybe fmapfn ev) where
  fmapfn ma = case ma of
    Nothing -> Just ()
    _ -> Nothing

-- TODO move to Data.Either.Extra
maybeLeft :: Either a b -> Maybe a
maybeLeft (Left a) = Just a
maybeLeft _ = Nothing

somedefaultpfcfg :: MainPFWidgetConfig
somedefaultpfcfg = def {
    --_mainPFWidgetConfig_initialFile = Just "potato.flow"
    _mainPFWidgetConfig_initialState = (owlpfstate_newProject, emptyControllerMeta)
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
  resp <- httpLBS "https://raw.githubusercontent.com/pdlla/tinytools-vty/potato/MOTD.txt"
  return $ LT.toStrict $ LT.decodeUtf8 (getResponseBody resp)

fetchMOTDAsync :: forall t m. (MonadWidget t m) => Event t () -> m (Event t Text)
fetchMOTDAsync ev = performEventAsync $ ffor ev $ const $ \f -> liftIO $ do
    forkIO $ do
      motd <- fetchMOTD
      f motd
    return ()

-- NOTE, this will query welcome message each time you recreate this
welcomeWidget :: forall t m. (MonadWidget t m)
  => m (Event t ())
welcomeWidget = do
  postBuildEv <- getPostBuild
  welcomeMessageEv <- fetchMOTDAsync postBuildEv
  welcomeMessageDyn <- holdDyn "loading..." welcomeMessageEv
  boxTitle (constant def) "😱😱😱" $ do
    initLayout $ col $ do
      (grout . stretch) 1 $ text (current welcomeMessageDyn)
      (grout . fixed) 3 $ textButton def (constant "bye")




-- TODO DELETE OR MOVE UNUSED
-- | toggle the focus of a widget
-- also forces unfocused widget to ignore mouse inputs
focusWidgetNoMouse :: forall t m a. (MonadWidget t m)
  => Dynamic t Bool -- ^ whether widget should be focused or not, note events that change focus are not captured!
  -> m a
  -> m a
focusWidgetNoMouse f child = do
  localFocus (liftA2 (&&) f) $
    localInput (gate (current f)) $
      child

-- TODO DELETE OR MOVE UNUSED
-- | ignores mouse input unless widget is focused
ignoreMouseUnlessFocused :: forall t m a. (MonadWidget t m)
  => m a
  -> m a
ignoreMouseUnlessFocused child = do
  f <- focus
  focusWidgetNoMouse f child



-- | block all or some input events, always focused if parent is focused
captureInputEvents :: forall t m a. (MonadWidget t m)
  => These (Event t ()) (Behavior t Bool) -- ^ This ev is event indicating input should be capture. That beh is behavior gating input (true means captured)
  -> m a
  -> m a
captureInputEvents capture child = do
  let
    (ev, beh) = fromThese never (constant False) capture
  localInput (\inp -> difference (gate (fmap not beh) inp) ev) $
    child

data MainPFWidgetConfig = MainPFWidgetConfig {
  _mainPFWidgetConfig_initialFile :: Maybe FP.FilePath
  , _mainPFWidgetConfig_homeDirectory :: FP.FilePath
  -- should this include controller meta too?
  , _mainPFWidgetConfig_initialState :: (OwlPFState, ControllerMeta) -- ^ will be overriden by initialFile if set
}

instance Default MainPFWidgetConfig where
  def = MainPFWidgetConfig {
      _mainPFWidgetConfig_initialFile = Nothing
      --, _mainPFWidgetConfig_homeDirectory = "/"
      , _mainPFWidgetConfig_homeDirectory = "/home/minimaple/kitchen/faucet/potato-flow-vty"
      , _mainPFWidgetConfig_initialState = (emptyOwlPFState, emptyControllerMeta)
    }

mainPFWidget :: forall t m. (MonadWidget t m)
  => MainPFWidgetConfig
  -> m (Event t ())
mainPFWidget cfg = mainPFWidgetWithBypass cfg never

mainPFWidgetWithBypass :: forall t m. (MonadWidget t m)
  => MainPFWidgetConfig
  -> Event t WSEvent
  -> m (Event t ())
mainPFWidgetWithBypass MainPFWidgetConfig {..} bypassEvent = mdo
  -- external inputs
  --currentTime <- liftIO $ getCurrentTime

  -- note tickEv triggers 2 ticks
  --tickEv <- tickLossy 1 currentTime
  --ticks <- foldDyn (+) (0 :: Int) (fmap (const 1) tickEv)

  --flowInput <- input >>= return . traceEvent "input: "
  flowInput <- input
  postBuildEv <- getPostBuild

  -- need this to force redraw of handles in some cases
  tickOnEvent (updated . _goatWidget_selection $ everythingW)

  let
    -- load file on start
    -- TODO load file from open file dialog
    tryOpenFileEv = leftmost [fforMaybe postBuildEv (const _mainPFWidgetConfig_initialFile), openFileEv]

  -- load file on start
  mLoadFileEv <- performEvent $ ffor tryOpenFileEv
    $ \fp -> do
      absfp <- liftIO $ FP.makeAbsolute fp
      mspf :: Maybe (SPotatoFlow, ControllerMeta) <- liftIO $ Aeson.decodeFileStrict absfp
      return (mspf, absfp)

  -- empty project event
  let
      -- a little silly to route a new empty project through the load file event but it's easy whatever
      newEmptyFileEv = fmap (const (owlPFState_to_sPotatoFlow owlpfstate_newProject, emptyControllerMeta)) _saveBeforeActionOutput_new


  -- set the title
  let
    setOpenFileStateEv = updated $ ffor2 currentOpenFileDyn (_goatWidget_unsavedChanges everythingW) (,)
  performEvent_ $ ffor setOpenFileStateEv $ \(mfn, dirty) -> do
    -- this only seems to sometimes work 🤷🏼‍♀️
    liftIO $ hSetTitle stdout $ fromMaybe "<>" mfn <> (if dirty then "*" else "")


  let
    performSaveEv = attach (current $ _goatWidget_DEBUG_goatState everythingW) $ leftmost [saveAsEv, clickSaveEv]
    saveSuccessEv = snd (fanEither finishSaveEv)
  finishSaveEv <- performEvent $ ffor performSaveEv $ \(gs,fn) -> liftIO $ do
    let
      spf = owlPFState_to_sPotatoFlow . _owlPFWorkspace_owlPFState . _goatState_workspace $ gs
      cm = ControllerMeta {
          _controllerMeta_pan      = _goatState_pan gs
          , _controllerMeta_layers = _layersState_meta . _goatState_layersState $ gs
        }
    handle (\(SomeException e) -> return . Left $ "ERROR, Could not save to file " <> show fn <> " got exception \"" <> show e <> "\"") $ do
      --liftIO $ Aeson.encodeFile "potato.flow" spf
      --print $ "wrote to file: " <> fn
      LBS.writeFile fn $ PrettyAeson.encodePretty (spf, cm)
      --LBS.writeFile fn $ Aeson.encode (spf, cm)
      return $ Right fn

  -- debug stuff (temp)
  let
    debugKeyEv' = fforMaybe flowInput $ \case
      V.EvKey (V.KPageDown) [] -> Just ()
      _ -> Nothing
    debugKeyEv = attach (current . fmap _goatState_handler . _goatWidget_DEBUG_goatState $ everythingW) debugKeyEv'
  performEvent_ $ ffor debugKeyEv $ \(handler, _) -> do
    liftIO $ do
      T.hPutStr stderr $ pHandlerDebugShow handler
      hFlush stderr

  -- application level hotkeys
  AppKbCmd {..} <- captureInputEvents (That inputCapturedByPopupBeh) holdAppKbCmd

  -- setup PotatoConfig
  currentOpenFileDyn <- holdDyn Nothing $ fmap Just $ leftmost [saveSuccessEv, fmap snd mLoadFileEv]
  let
    potatoConfig = PotatoConfig {
        _potatoConfig_style = constant def
        , _potatoConfig_appCurrentOpenFile = current currentOpenFileDyn
        , _potatoConfig_appCurrentDirectory = fmap (maybe _mainPFWidgetConfig_homeDirectory FP.takeDirectory) $ current currentOpenFileDyn
        -- TODO
        , _potatoConfig_appPrintFile = constant Nothing
      }

    goatWidgetConfig = GoatWidgetConfig {
        _goatWidgetConfig_initialState = _mainPFWidgetConfig_initialState
        , _goatWidgetConfig_load = leftmost [fmapMaybe fst mLoadFileEv, newEmptyFileEv]

        -- canvas direct input
        , _goatWidgetConfig_mouse = leftmostWarn "mouse" [(_layerWidget_mouse (_leftWidget_layersW leftW)), (_canvasWidget_mouse canvasW)]
        , _goatWidgetConfig_keyboard = keyboardEv

        , _goatWidgetConfig_canvasRegionDim = _canvasWidget_regionDim canvasW

        , _goatWidgetConfig_selectTool = _toolWidget_setTool (_leftWidget_toolsW leftW)
        , _goatWidgetConfig_paramsEvent = _paramsWidget_paramsEvent (_leftWidget_paramsW leftW)
        , _goatWidgetConfig_canvasSize = _paramsWidget_canvasSizeEvent (_leftWidget_paramsW leftW)
        , _goatWidgetConfig_newFolder = _layerWidget_newFolderEv (_leftWidget_layersW leftW)
        , _goatWidgetConfig_setPotatoDefaultParameters = _paramsWidget_setDefaultParamsEvent (_leftWidget_paramsW leftW)
        , _goatWidgetConfig_markSaved = void saveSuccessEv

        , _goatWidgetConfig_setFocusedArea = _leftWidget_setFocusEvent leftW

        -- TODO
        --, _goatWidgetConfig_unicodeWidthFn =

        -- debugging stuff
        , _goatWidgetConfig_setDebugLabel = never
        , _goatWidgetConfig_bypassEvent = bypassEvent
      }

  everythingW <- holdGoatWidget goatWidgetConfig



  -- define main panels
  let
    leftPanel = holdLeftWidget LeftWidgetConfig {
        _layersWidgetConfig_goatW = everythingW
      }

    rightPanel = do
      dreg' <- askRegion
      let dreg = fmap (\region -> region { _region_left = 0, _region_top = 0}) dreg'
      f <- focus
      pane dreg f $ holdCanvasWidget $ CanvasWidgetConfig {
          _canvasWidgetConfig_pan = _goatWidget_pan everythingW
          , _canvasWidgetConfig_broadPhase = _goatWidget_broadPhase everythingW
          , _canvasWidgetConfig_renderedCanvas = _goatWidget_renderedCanvas everythingW
          , _canvasWidgetConfig_renderedSelection = _goatWidget_renderedSelection everythingW
          , _canvasWidgetConfig_canvas = _goatWidget_canvas everythingW
          , _canvasWidgetConfig_handles = _goatWidget_handlerRenderOutput everythingW
        }

  -- render main panels
  (keyboardEv, (leftW, canvasW)) <- flip runPotatoReader potatoConfig $
    captureInputEvents (These _appKbCmd_capturedInput inputCapturedByPopupBeh) $ do
      stuff <- splitHDrag 35 (fill (constant '*')) leftPanel rightPanel
      -- left panel may capture inputs, rigth panel never captures inputs
      kb <- captureInputEvents (This (_paramsWidget_captureInputEv (_leftWidget_paramsW leftW))) $ do
        inp <- input
        return $ fforMaybe inp $ \case
          V.EvKey k mods -> convertKey k >>= (\kbd -> return $ KeyboardData kbd (convertModifiers mods))
          V.EvPaste bs -> Just $ KeyboardData (KeyboardKey_Paste (T.decodeUtf8 bs)) []
          _ -> Nothing

      return (kb, stuff)

  let
    (clickSaveEv, nothingClickSaveEv)  = fanMaybe $ tag (_potatoConfig_appCurrentOpenFile potatoConfig) $ leftmost [_menuButtonsWidget_saveEv . _leftWidget_menuButtonsW $ leftW, _appKbCmd_save, _saveBeforeActionOutput_save]
    clickSaveAsEv = leftmost $ [_menuButtonsWidget_saveAsEv . _leftWidget_menuButtonsW $ leftW, nothingClickSaveEv, _saveBeforeActionOutput_saveAs]

  -- TODO probably have some sort of PopupManager -__-
  -- 1 welcome popup
  --(_, popupStateDyn1) <- popupPaneSimple def (postBuildEv $> welcomeWidget)
  (_, popupStateDyn1) <- popupPaneSimple def (never $> welcomeWidget)

  -- 2 save as popup
  (saveAsEv, popupStateDyn2) <- flip runPotatoReader potatoConfig $ popupSaveAsWindow $ SaveAsWindowConfig (tag (_potatoConfig_appCurrentDirectory potatoConfig) clickSaveAsEv)

  -- TODO alert if mLoadFileEv fails (Nothing)
  -- 3 alert popup
  let
    saveFailAlertEv = fmapMaybe maybeLeft finishSaveEv
  popupStateDyn3 <- flip runPotatoReader potatoConfig $ popupAlert saveFailAlertEv

  -- 4 unsaved changes on action popup
  (SaveBeforeActionOutput {..}, popupStateDyn4) <- flip runPotatoReader potatoConfig $ popupSaveBeforeExit $
    SaveBeforeActionConfig {
        _saveBeforeActionConfig_unsavedChangesBeh = current $ _goatWidget_unsavedChanges everythingW
        , _saveBeforeActionConfig_open = leftmost [_appKbCmd_open, _menuButtonsWidget_openEv . _leftWidget_menuButtonsW $ leftW]
        , _saveBeforeActionConfig_new = leftmost [_appKbCmd_new, _menuButtonsWidget_newEv . _leftWidget_menuButtonsW $ leftW]
        , _saveBeforeActionConfig_exit = leftmost [_appKbCmd_quit, _menuButtonsWidget_quitEv . _leftWidget_menuButtonsW $ leftW]
        , _saveBeforeActionConfig_saveOutcomeEv = finishSaveEv
      }

  -- 5 open popup
  (openFileEv, popupStateDyn5) <- flip runPotatoReader potatoConfig $ popupOpenWindow $ OpenWindowConfig (tag (_potatoConfig_appCurrentDirectory potatoConfig) _saveBeforeActionOutput_open)



  let
    -- TODO assert that we never have more than 1 popup open at once
    -- block input if any popup is currently open
    inputCapturedByPopupBeh = current . fmap getAny . mconcat . fmap (fmap Any) $ [popupStateDyn1, popupStateDyn2, popupStateDyn3, popupStateDyn4, popupStateDyn5]


  -- handle escape event
  return $ leftmost [_appKbCmd_forceQuit, _saveBeforeActionOutput_exit]
