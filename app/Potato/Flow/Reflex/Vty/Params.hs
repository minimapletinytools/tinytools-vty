{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Params (
  ParamsWidgetConfig(..)
  , ParamsWidget(..)
  , holdParamsWidget
) where

import           Relude

import           Potato.Flow.Reflex.Vty.PFWidgetCtx

import           Control.Monad.Fix
import           Control.Monad.NodeId

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Vty

data ParamsWidgetConfig t = ParamsWidgetConfig {
  _paramsWidgetConfig_pfctx        :: PFWidgetCtx t
}

data ParamsWidget t = ParamsWidget {
  _paramsWidget_consumingKeyboard :: Behavior t Bool
}

holdParamsWidget :: forall t m. (PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => ParamsWidgetConfig t
  -> VtyWidget t m (ParamsWidget t)
holdParamsWidget ParamsWidgetConfig {..} = row $ do

  return ParamsWidget {
    -- TODO
    _paramsWidget_consumingKeyboard = constant False
  }
