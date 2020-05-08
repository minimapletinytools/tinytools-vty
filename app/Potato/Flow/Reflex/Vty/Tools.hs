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
  _toolWidgetConfig_pfctx               :: PFWidgetCtx t
  , _toolWidgetConfig_setDefault        :: Event t ()
  , _toolWidgetConfig_consumingKeyboard :: Behavior t Bool
}

data ToolWidget t = ToolWidget {
  _toolWidget_tool :: Event t Tool
}


onlyIfBeh :: (Reflex t) => Event t a -> Behavior t Bool -> Event t a
onlyIfBeh ev beh = fmapMaybe (\(b,e) -> if b then Just e else Nothing) $ attach beh ev


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
    allowKB = fmap not _toolWidgetConfig_consumingKeyboard
    keyPressEv' k = (flip fmapMaybe) (_pFWidgetCtx_ev_input _toolWidgetConfig_pfctx) $ \case
      V.EvKey (V.KChar k') [] | k' == k -> Just ()
      _ -> Nothing
    keyPressEv k = onlyIfBeh (keyPressEv' k) allowKB

  return ToolWidget {
    _toolWidget_tool = leftmost
      [TSelect <$ leftmost
        [ selectB
        , onlyIfBeh (_pFWidgetCtx_ev_cancel _toolWidgetConfig_pfctx) allowKB
        , _toolWidgetConfig_setDefault
        , keyPressEv 'v']
      , TPan <$ leftmost [panB, keyPressEv ' ']
      , TBox <$ leftmost [boxB, keyPressEv 'b']
      , TLine <$ leftmost [lineB, keyPressEv 'l']
      , TText <$ leftmost [textB, keyPressEv 't']]
  }

  {-
      keyPressEv k = flip push (_pFWidgetCtx_ev_input _toolWidgetConfig_pfctx) $ \vtyev -> do
        consuming <- sample _toolWidgetConfig_consumingKeyboard
        return $ if not consuming
          then case vtyev of
            V.EvKey (V.KChar k') [] | k' == k -> Just ()
            _                                 -> Nothing
          else Nothing
          -}
