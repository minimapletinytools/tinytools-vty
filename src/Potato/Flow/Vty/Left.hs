-- TODO someday we will do dockable widget manager, but this is what yo uget for now

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Left (
  LeftWidgetConfig(..)
  , holdLeftWidget
  , LeftWidget(..)
) where
import           Relude


import           Potato.Flow
import           Potato.Flow.Vty.Info
import           Potato.Flow.Vty.Layer
import           Potato.Flow.Vty.Params
import           Potato.Flow.Vty.PotatoReader
import           Potato.Flow.Vty.Tools
import Potato.Flow.Vty.Common
import           Potato.Reflex.Vty.Helpers
import qualified Data.Text.IO as T

import           Reflex
import           Reflex.Vty

-- could put into a different file but whatever
data MenuButtonsWidget t = MenuButtonsWidget {
  _menuButtonsWidget_saveEv :: Event t ()
  , _menuButtonsWidget_saveAsEv :: Event t ()
  --, _menuButtonsWidget_exportEv ::
}

data LeftWidgetConfig t = LeftWidgetConfig {
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
  initLayout $ col $ mdo


    -- TODO consider
    -- TODO height should be dynamic but not sure if there's away to do this dynamically because width (from which buttonsHeightDyn) is derived depends on `grout . fixed`. You need to pull width from outside of the `grout . fixed` call to make this work right...
    (clickSaveEv_d1, clickSaveAsEv_d1, buttonsHeightDyn) <- (grout . fixed) 2 $ row $ do

      (buttonsEv, heightDyn) <- buttonList (constDyn ["save", "save as", "export to \"potato.txt\""]) Nothing
      let
        clickSaveEv_d2 = ffilterButtonIndex 0 buttonsEv
        clickSaveAsEv_d2 = ffilterButtonIndex 1 buttonsEv
        exportEv = ffilterButtonIndex 2 buttonsEv
        clickPrintEv = tag (current $ _goatWidget_renderedCanvas _layersWidgetConfig_goatW) (void exportEv)
      performEvent_ $ ffor clickPrintEv $ \rc -> do
         let t = renderedCanvasToText rc
         liftIO $ T.writeFile "potato.txt" t
      return (clickSaveEv_d2, clickSaveAsEv_d2, heightDyn)
    let
      menuButtons = MenuButtonsWidget {
          _menuButtonsWidget_saveEv = clickSaveEv_d1
          , _menuButtonsWidget_saveAsEv = clickSaveAsEv_d1
          --, _menuButtonsWidget_exportEv ::
        }


    hdivider

    tools <- (grout . fixed) 3 $ holdToolsWidget $  ToolWidgetConfig {
        _toolWidgetConfig_tool =  _goatWidget_tool _layersWidgetConfig_goatW
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

    params <- (grout . fixed) 10 $ holdParamsWidget $ ParamsWidgetConfig {
        _paramsWidgetConfig_selectionDyn = _goatWidget_selection _layersWidgetConfig_goatW
        , _paramsWidgetConfig_canvasDyn = _goatWidget_canvas _layersWidgetConfig_goatW
      }
    return LeftWidget {
        _leftWidget_layersW = layers
        , _leftWidget_toolsW = tools
        , _leftWidget_paramsW = params
        , _leftWidget_menuButtonsW = menuButtons
      }
