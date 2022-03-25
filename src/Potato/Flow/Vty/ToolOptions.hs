{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.ToolOptions (
  ToolOptionsWidgetConfig(..)
  , ToolOptionsWidget(..)
  , holdToolOptionsWidget
) where

import           Relude

import           Potato.Flow.Controller
import           Potato.Flow.Vty.Common
import           Potato.Reflex.Vty.Helpers

import           Control.Monad.Fix
import           Control.Monad.NodeId

import           Reflex


data ToolOptionsWidgetConfig t = ToolOptionsWidgetConfig {
  _toolOptionsWidgetConfig_tool :: Dynamic t Tool
  , _toolOptionsWidgetConfig_widthDyn :: Dynamic t Int
}

data ToolOptionsWidget t = ToolOptionsWidget {
  _toolOptionsWidget_setOption :: Event t ()
  , _toolOptionsWidget_heightDyn :: Dynamic t Int
}


holdToolOptionsWidget :: forall t m. (PostBuild t m, MonadWidget t m)
  => ToolOptionsWidgetConfig t
  -> m (ToolOptionsWidget t)
holdToolOptionsWidget ToolOptionsWidgetConfig {..} = mdo


  return ToolOptionsWidget {
    _toolOptionsWidget_setOption = never
    , _toolOptionsWidget_heightDyn = constDyn 0
  }
