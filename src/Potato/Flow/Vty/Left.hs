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

-- could put into a different file but whatever
data MenuButtonsWidget t = MenuButtonsWidget {
  _menuButtonsWidget_saveEv :: Event t ()
  , _menuButtonsWidget_saveAsEv :: Event t ()
  , _menuButtonsWidget_exportEv :: Event t ()
  , _menuButtonsWidget_quitEv :: Event t ()
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
  -- TODO other stuff
}


hdivider :: forall t m. (MonadWidget t m, HasLayout t m) => m ()
hdivider = (grout. fixed) 1 $ fill (constant '-')

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


    -- TODO consider
    -- TODO height should be dynamic but not sure if there's away to do this dynamically because width (from which buttonsHeightDyn) is derived depends on `grout . fixed`. You need to pull width from outside of the `grout . fixed` call to make this work right...
    (menuButtons, buttonsHeightDyn) <- (grout . fixed) buttonsHeightDyn $ row $ do

      (buttonsEv, heightDyn) <- buttonList (constDyn ["save", "save as", "export to \"potato.txt\"", "quit"]) (Just widthDyn)
      let
        exportEv = ffilterButtonIndex 2 buttonsEv
        menuButtons' = MenuButtonsWidget {
            _menuButtonsWidget_saveEv = ffilterButtonIndex 0 buttonsEv
            , _menuButtonsWidget_saveAsEv = ffilterButtonIndex 1 buttonsEv
            , _menuButtonsWidget_exportEv = exportEv
            , _menuButtonsWidget_quitEv = ffilterButtonIndex 3 buttonsEv
          }
        clickPrintEv = tag (current $ _goatWidget_renderedCanvas _layersWidgetConfig_goatW) (void exportEv)
      -- TODO don't do this here cmon...
      performEvent_ $ ffor clickPrintEv $ \rc -> do
         let t = renderedCanvasToText rc
         -- TODO at least use filename...
         liftIO $ T.writeFile "potato.txt" t
      return (menuButtons', heightDyn)



    hdivider

    tools <- (grout . fixed) (_toolWidget_heightDyn tools) $ holdToolsWidget $ ToolWidgetConfig {
        _toolWidgetConfig_tool =  _goatWidget_tool _layersWidgetConfig_goatW
        , _toolWidgetConfig_widthDyn = widthDyn
      }

    -- doesn't do anything right now, just a stub
    toolOptions <- (grout . fixed) (_toolOptionsWidget_heightDyn toolOptions) $ holdToolOptionsWidget $ ToolOptionsWidgetConfig {
        _toolOptionsWidgetConfig_tool =  _goatWidget_tool _layersWidgetConfig_goatW
        , _toolOptionsWidgetConfig_widthDyn = widthDyn
      }

    hdivider

    -- TODO Layout stuff messes up your mouse assumptions. You need to switch Layout to use pane2 D:
    layers <- (grout . stretch) 1 $ holdLayerWidget $ LayerWidgetConfig {
          _layerWidgetConfig_layers = _goatWidget_layers _layersWidgetConfig_goatW
          , _layerWidgetConfig_layersView = _goatWidget_layersHandlerRenderOutput _layersWidgetConfig_goatW
          , _layerWidgetConfig_selection = _goatWidget_selection  _layersWidgetConfig_goatW
        }

    hdivider

    _ <- (grout . fixed) 5 $ holdInfoWidget $ InfoWidgetConfig {
        _infoWidgetConfig_selection = _goatWidget_selection _layersWidgetConfig_goatW
      }

    hdivider

    params <- (grout . fixed) (_paramsWidget_widgetHeight params) $ holdParamsWidget $ ParamsWidgetConfig {
        _paramsWidgetConfig_selectionDyn = _goatWidget_selection _layersWidgetConfig_goatW
        , _paramsWidgetConfig_canvasDyn = _goatWidget_canvas _layersWidgetConfig_goatW
        , _paramsWidgetConfig_defaultParamsDyn = _goatWidget_potatoDefaultParameters _layersWidgetConfig_goatW
        , _paramsWidgetConfig_toolDyn = _goatWidget_tool _layersWidgetConfig_goatW
        , _paramsWidgetConfig_loseFocusEv = loseFocusEv
      }
    return LeftWidget {
        _leftWidget_layersW = layers
        , _leftWidget_toolsW = tools
        , _leftWidget_paramsW = params
        , _leftWidget_menuButtonsW = menuButtons
      }
