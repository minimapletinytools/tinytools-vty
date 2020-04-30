{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Tools (
  Tool(..)
  , tool_cursorState
  , ToolWidget(..)
  , holdToolsWidget
) where
import           Relude

import           Potato.Flow
import           Reflex.Potato.Helpers
import Potato.Flow.Reflex.Vty.CanvasPane

import           Control.Monad.Fix
import           Control.Monad.NodeId
import qualified Data.IntMap.Strict as IM
import Data.These

import           Reflex
import           Reflex.Vty

data Tool = TPan | TBox | TNothing deriving (Eq, Show)

tool_cursorState :: Tool -> CursorState
tool_cursorState TPan = CSPan
tool_cursorState TBox = CSBox
tool_cursorState _    = CSSelecting

data ToolWidget t = ToolWidget {
  _toolWidget_tool :: Event t Tool
}

holdToolsWidget :: forall t m. (Reflex t, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => VtyWidget t m (ToolWidget t)
holdToolsWidget = row $ do
  pan <- fixed 5 $ textButton def "PAN"
  box <- fixed 5 $ textButton def "BOX"
  return ToolWidget {
    _toolWidget_tool = leftmost [TPan <$ pan, TBox <$ box]
  }
