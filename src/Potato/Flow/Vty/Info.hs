{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Info (
  InfoWidgetConfig(..)
  , InfoWidget(..)
  , holdInfoWidget
) where

import           Relude

import           Potato.Flow
import           Potato.Reflex.Vty.Helpers

import           Reflex
import           Reflex.Network
import           Reflex.Vty



data InfoWidgetConfig t = InfoWidgetConfig {
  _infoWidgetConfig_selection :: Dynamic t Selection
}

data InfoWidget t = InfoWidget {
}

holdInfoWidget :: forall t m. (MonadWidget t m)
  => InfoWidgetConfig t
  -> m (InfoWidget t)
holdInfoWidget InfoWidgetConfig {..} = do
  let
    -- TODO read canvasSelection and figure out what the preset is
    infoDyn = ffor _infoWidgetConfig_selection $ \selection@(SuperOwlParliament sowls) -> case () of
      _ | isParliament_length selection == 0 -> return ()
      _ | isParliament_length selection > 1 -> text "many"
      _ -> do
        let
          sowl = selectionToSuperOwl (CanvasSelection sowls)
          rid = _superOwl_id sowl
          label = hasOwlItem_name sowl
          selt = superOwl_toSElt_hack sowl
        initLayout $ col $ do
          (grout . fixed) 1 $ text (constant ("rid: " <> show rid <> " name: " <> label))
          case selt of
            SEltBox SBox {..} -> (grout . fixed) 1 $ text (constant (_sBoxText_text _sBox_text))
            _                 -> (grout . fixed) 1 $ text (constant "something else")

  networkView infoDyn

  return InfoWidget {}
