{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Tools (
  Tool(..)
  , tool_cursorState
  , ToolWidgetConfig(..)
  , ToolWidget(..)
  , holdToolsWidget
) where

import           Relude

import           Potato.Flow.Reflex.Vty.CanvasPane
import           Potato.Flow.Reflex.Vty.PFWidgetCtx

import           Control.Monad.Fix
import           Control.Monad.NodeId

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Vty

data Tool = TSelect | TPan | TBox | TLine | TText deriving (Eq, Show)

tool_cursorState :: Tool -> CursorState
tool_cursorState TPan = CSPan
tool_cursorState TBox = CSBox
tool_cursorState _    = CSSelecting

data ToolWidgetConfig t = ToolWidgetConfig {
  _toolWidgetConfig_pfctx        :: PFWidgetCtx t
  , _toolWidgetConfig_setDefault :: Event t ()
}

data ToolWidget t = ToolWidget {
  _toolWidget_tool :: Event t Tool
}

holdToolsWidget :: forall t m. (PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => ToolWidgetConfig t
  -> VtyWidget t m (ToolWidget t)
holdToolsWidget ToolWidgetConfig {..} = row $ do

  let
    -- TODO active tool should use a different style, but that means every button needs its own config...
    -- or make your own button method
    bc = ButtonConfig (pure singleBoxStyle) (pure thickBoxStyle)
    --bc = ButtonConfig (pure roundedBoxStyle) (pure roundedBoxStyle)

  selectB <- fixed 3 $ textButton bc "s"
  panB <- fixed 3 $ textButton bc "╬"
  boxB <- fixed 3 $ textButton bc "□"
  lineB <- fixed 3 $ textButton bc "/"
  textB <- fixed 3 $ textButton bc "T"

  let
    -- TODO this is incorrect, we need our own notion of focus that is different than the pane focus one
    -- probaby just a single `globalFocus :: Dynamic t Bool` in pfctx will do.
    keyPressEv k = (flip fmapMaybe) (_pFWidgetCtx_ev_input _toolWidgetConfig_pfctx) $ \case
      V.EvKey (V.KChar k') [] | k' == k -> Just ()
      _ -> Nothing


  -- TODO remove key press events, see comment above
  return ToolWidget {
    _toolWidget_tool = leftmost
      [TSelect <$ leftmost [selectB, _pFWidgetCtx_ev_cancel _toolWidgetConfig_pfctx, _toolWidgetConfig_setDefault]
      , TPan <$ leftmost [panB, keyPressEv ' ']
      , TBox <$ leftmost [boxB, keyPressEv 'b']
      , TLine <$ leftmost [lineB, keyPressEv 'l']
      , TText <$ leftmost [textB, keyPressEv 't']]
  }
