{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Info (
  InfoWidgetConfig(..)
  , InfoWidget(..)
  , holdInfoWidget
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Vty.Common
import           Potato.Flow.Vty.Manipulator.Types
import           Potato.Flow.Vty.PFWidgetCtx
import           Potato.Reflex.Vty.Helpers

import           Control.Monad.Fix
import           Control.Monad.NodeId
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as T

import qualified Graphics.Vty                      as V
import           Reflex
import           Reflex.Network
import           Reflex.Vty



data InfoWidgetConfig t = InfoWidgetConfig {
  _infoWidgetConfig_pfctx       :: PFWidgetCtx t
  , _infoWidgetConfig_selection :: Dynamic t Selection
}

data InfoWidget t = InfoWidget {
}

holdInfoWidget :: forall t m. (MonadWidget t m)
  => InfoWidgetConfig t
  -> VtyWidget t m (InfoWidget t)
holdInfoWidget InfoWidgetConfig {..} = do
  let
    -- TODO read canvasSelection and figure out what the preset is
    infoDyn = ffor _infoWidgetConfig_selection $ \selection -> case () of
      _ | Seq.length selection == 0 -> return ()
      _ | Seq.length selection > 1 -> text "many"
      _ -> do
        let
          (rid,lp,SEltLabel label selt) = selectionToSuperSEltLabel selection
        col $ do
          fixed 1 $ text (constant ("rid: " <> show rid <> " lp: " <> show lp <> " name: " <> label))
          case selt of
            SEltBox SBox {..} -> fixed 1 $ text (constant (_sBoxText_text _sBox_text))
            _                 -> fixed 1 $ text (constant "something else")

  networkView infoDyn

  return InfoWidget {}
