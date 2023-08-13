-- TODO someday we will do dockable widget manager, but this is what yo uget for now

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Left (
  LeftWidgetConfig(..)
  , holdLeftWidget
  , LeftWidget(..)
  , MenuButtonsWidget(..)
) where
import           Relude


import           Potato.Flow
import           Potato.Flow.Vty.Info
import           Potato.Flow.Vty.Layer
import           Potato.Flow.Vty.Params
import           Potato.Flow.Vty.PotatoReader
import           Potato.Flow.Vty.Tools
import           Potato.Flow.Vty.ToolOptions
import Potato.Flow.Vty.Common
import           Potato.Reflex.Vty.Helpers
import qualified Data.Text.IO as T

import           Reflex
import           Reflex.Vty
import qualified Graphics.Vty as V


-- could put into a different file but whatever
data MenuButtonsWidget t = MenuButtonsWidget {
  _menuButtonsWidget_newEv :: Event t ()
  , _menuButtonsWidget_openEv :: Event t ()
  , _menuButtonsWidget_saveEv :: Event t ()
  , _menuButtonsWidget_saveAsEv :: Event t ()
  , _menuButtonsWidget_exportEv :: Event t ()
  , _menuButtonsWidget_quitEv :: Event t ()
  , _menuButtonsWidget_aboutEv :: Event t ()
}

data LeftWidgetConfig t = LeftWidgetConfig {
  -- TODO rename to _leftWidgetConfig_goatW
  _layersWidgetConfig_goatW :: GoatWidget t
  -- TODO other stuff
}

data LeftWidget t = LeftWidget {
  _leftWidget_layersW :: LayerWidget t
  , _leftWidget_toolsW :: ToolWidget t
  , _leftWidget_paramsW :: ParamsWidget t
  , _leftWidget_menuButtonsW :: MenuButtonsWidget t
  , _leftWidget_setFocusEvent :: Event t GoatFocusedArea
  -- TODO other stuff
}


hdivider :: forall t m. (MonadWidget t m, HasLayout t m) => m ()
hdivider = (grout. fixed) 1 $ fill (constant '-')

-- | Mouse down events for a particular mouse button
mouseFocus
  :: (Reflex t, Monad m, HasInput t m)
  => m (Event t Bool)
mouseFocus = do
  i <- input
  return $ fforMaybe i $ \case
    V.EvMouseDown _ _ _ _ -> Just True
    _ -> Nothing


holdLeftWidget :: forall t m. (MonadWidget t m, HasPotato t m)
  => LeftWidgetConfig t
  -> m (LeftWidget t)
holdLeftWidget LeftWidgetConfig {..} = do

  widthDyn <- displayWidth
  -- this will toggle true/fales as you click between left/canvas panels
  -- this only works because you use splitHDrag to split the Left/Canvas panels (it toggles the focus)
  focusDyn <- focus
  let

    loseFocusEv = void $ ffilter (not . id) $ updated focusDyn

  initLayout $ col $ mdo


    -- Menu
    -- TODO height should be dynamic but not sure if there's away to do this dynamically because width (from which buttonsHeightDyn) is derived depends on `grout . fixed`. You need to pull width from outside of the `grout . fixed` call to make this work right...
    (menuButtons, menuFocusEv, buttonsHeightDyn) <- (grout . fixed) buttonsHeightDyn $ row $ do

      (buttonsEv, heightDyn) <- buttonList (constDyn ["about", "new", "open", "save", "save as", "export to \"potato.txt\"", "quit"]) (Just widthDyn)
      let
        exportEv = ffilterButtonIndex 5 buttonsEv
        menuButtons' = MenuButtonsWidget {
            _menuButtonsWidget_aboutEv = ffilterButtonIndex 0 buttonsEv
            , _menuButtonsWidget_newEv = ffilterButtonIndex 1 buttonsEv
            , _menuButtonsWidget_openEv = ffilterButtonIndex 2 buttonsEv
            , _menuButtonsWidget_saveEv = ffilterButtonIndex 3 buttonsEv
            , _menuButtonsWidget_saveAsEv = ffilterButtonIndex 4 buttonsEv
            , _menuButtonsWidget_exportEv = exportEv
            , _menuButtonsWidget_quitEv = ffilterButtonIndex 6 buttonsEv
          }
        clickPrintEv = tag (current $ _goatWidget_renderedCanvas _layersWidgetConfig_goatW) (void exportEv)
      -- TODO don't do this here cmon...
      performEvent_ $ ffor clickPrintEv $ \rc -> do
         let t = renderedCanvasToText rc
         -- TODO at least use filename...
         liftIO $ T.writeFile "potato.txt" t
      menuFocusEv' <- mouseFocus
      return (menuButtons', menuFocusEv', heightDyn)



    hdivider

    -- Tools
    (tools, toolsFocusEv) <- (grout . fixed) (_toolWidget_heightDyn tools) $ do
      tools' <- holdToolsWidget $ ToolWidgetConfig {
          _toolWidgetConfig_tool =  _goatWidget_tool _layersWidgetConfig_goatW
          , _toolWidgetConfig_widthDyn = widthDyn
        }
      toolsFocusEv' <- mouseFocus
      return (tools', toolsFocusEv')

    -- ToolsOptions
    -- doesn't do anything right now, just a stub
    (toolOptions, toolsOptionsFocusEv) <- (grout . fixed) (_toolOptionsWidget_heightDyn toolOptions) $ do
      toolsOptions' <- holdToolOptionsWidget $ ToolOptionsWidgetConfig {
          _toolOptionsWidgetConfig_tool =  _goatWidget_tool _layersWidgetConfig_goatW
          , _toolOptionsWidgetConfig_widthDyn = widthDyn
        }
      toolsOptionsFocusEv' <- mouseFocus
      return (toolsOptions', toolsOptionsFocusEv')

    hdivider

    -- Layers
    (layers, layersFocusEv) <- (grout . stretch) 1 $ do
      layers' <- holdLayerWidget $ LayerWidgetConfig {
          _layerWidgetConfig_layers = _goatWidget_layers _layersWidgetConfig_goatW
          , _layerWidgetConfig_layersView = _goatWidget_layersHandlerRenderOutput _layersWidgetConfig_goatW
          , _layerWidgetConfig_selection = _goatWidget_selection  _layersWidgetConfig_goatW
        }
      layersFocusEv' <- mouseFocus
      return (layers', layersFocusEv')

    hdivider

    
{-
    -- Info
    infoFocusEv <- (grout . fixed) 5 $ do
      holdInfoWidget $ InfoWidgetConfig {
          _infoWidgetConfig_selection = _goatWidget_selection _layersWidgetConfig_goatW
        }
      mouseFocus

    hdivider
-}
    let infoFocusEv = never

    -- Params
    (params, paramsFocusEv) <- (grout . fixed) (_paramsWidget_widgetHeight params) $ do
      params' <- holdParamsWidget $ ParamsWidgetConfig {
          _paramsWidgetConfig_selectionDyn = _goatWidget_selection _layersWidgetConfig_goatW
          , _paramsWidgetConfig_canvasDyn = _goatWidget_canvas _layersWidgetConfig_goatW
          , _paramsWidgetConfig_defaultParamsDyn = _goatWidget_potatoDefaultParameters _layersWidgetConfig_goatW
          , _paramsWidgetConfig_toolDyn = _goatWidget_tool _layersWidgetConfig_goatW
          , _paramsWidgetConfig_loseFocusEv = loseFocusEv
        }
      paramsFocusEv' <- mouseFocus
      return (params', paramsFocusEv')

    let 
      refinedFocusEv = leftmost
        [ fmap (const "menu") menuFocusEv
        , fmap (const "tools") toolsFocusEv
        , fmap (const "toolsOptions") toolsOptionsFocusEv
        , fmap (const "layers") layersFocusEv
        , fmap (const "info") infoFocusEv
        , fmap (const "params") paramsFocusEv 
        ]

    refinedFocusDyn <- holdDyn "none" refinedFocusEv

    let      
      setFocusDyn' = ffor2 focusDyn refinedFocusDyn $ \f1 f2 -> case (f1, f2) of
        (True, "layers") -> Just GoatFocusedArea_Layers
        (True, _)        -> Just GoatFocusedArea_Other
        -- right now, canvas is the only other choice, eventually there migh be more, in which case change this to Nothing
        -- NOTE changing this to Nothing will break the rename via layers -> click on canvas case as currently Goat does not know how to resolve the layer rename handler without an explicit focus change
        (False, _)       -> Just GoatFocusedArea_Canvas

    setFocusDyn <- holdUniqDyn setFocusDyn'


    return LeftWidget {
        _leftWidget_layersW = layers
        , _leftWidget_toolsW = tools
        , _leftWidget_paramsW = params
        , _leftWidget_menuButtonsW = menuButtons
        , _leftWidget_setFocusEvent = fmapMaybe id (updated setFocusDyn)
      }
