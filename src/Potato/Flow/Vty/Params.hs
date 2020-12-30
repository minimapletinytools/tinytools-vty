{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Params (
  ParamsWidgetConfig(..)
  , ParamsWidget(..)
  , holdParamsWidget
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Vty.Common
import           Potato.Flow.Vty.Manipulator.Types
import           Potato.Flow.Vty.PFWidgetCtx
import           Potato.Reflex.Vty.Helpers

import           Control.Monad.Fix
import           Control.Monad.NodeId
import qualified Data.Text                         as T

import qualified Graphics.Vty                      as V
import           Reflex
import           Reflex.Network
import           Reflex.Vty



data SEltParams = SEltParams {
    --_sEltParams_sBox =
  }

data ParamsWidgetConfig t = ParamsWidgetConfig {
  _paramsWidgetConfig_pfctx              :: PFWidgetCtx t
}

data ParamsWidget t = ParamsWidget {
  _paramsWidget_consumingKeyboard :: Behavior t Bool
  , _paramsWidget_modify          :: Event t ControllersWithId

  , _paramsWidget_defaults        :: Behavior t ()
}

presetStyles = ["╔╗╚╝║═█","****|-*"]


holdParamsWidget :: forall t m. (MonadWidget t m)
  => ParamsWidgetConfig t
  -> VtyWidget t m (ParamsWidget t)
holdParamsWidget ParamsWidgetConfig {..} = do
  let
    -- TODO read canvasSelection and figure out what the preset is


  typeChoice <- radioListSimple 0 ["presets", "custom"]
  networkView $ ffor typeChoice $ \case
    0 -> fmap (const ()) $ col $ do
      fixed 1 (return ())
      forM presetStyles $ \s -> fixed 1 $ row $ stretch $ (text (constant (T.pack s)))
    1 -> return ()
  --fill '#'



  return ParamsWidget {
    -- TODO
    _paramsWidget_consumingKeyboard = constant False
  }
