{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Tools (
  Tool(..)
  , ToolWidgetConfig(..)
  , ToolWidget(..)
  , holdToolsWidget
) where

import           Relude

import           Potato.Flow.Controller
import           Potato.Flow.Reflex.Vty.Attrs
import           Potato.Flow.Reflex.Vty.Common
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Reflex.Vty.Helpers

import           Control.Monad.Fix
import           Control.Monad.NodeId
import qualified Data.List.Index                    as L
import qualified Data.Text                          as T

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Vty


data ToolWidgetConfig t = ToolWidgetConfig {
  _toolWidgetConfig_pfctx  :: PFWidgetCtx t
  , _toolWidgetConfig_tool :: Dynamic t Tool
}

data ToolWidget t = ToolWidget {
  _toolWidget_setTool :: Event t Tool
}


onlyIfBeh :: (Reflex t) => Event t a -> Behavior t Bool -> Event t a
onlyIfBeh ev beh = fmapMaybe (\(b,e) -> if b then Just e else Nothing) $ attach beh ev


holdToolsWidget :: forall t m. (PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => ToolWidgetConfig t
  -> VtyWidget t m (ToolWidget t)
holdToolsWidget ToolWidgetConfig {..} = mdo

  radioEvs <- radioList (constDyn ["s","╬","□","/","T"]) (fmap ((:[]) . fromEnum) _toolWidgetConfig_tool)
  let
    selectB = void $ ffilter (==0) radioEvs
    panB = void $ ffilter (==1) radioEvs
    boxB = void $ ffilter (==2) radioEvs
    lineB = void $ ffilter (==3) radioEvs
    textB = void $ ffilter (==4) radioEvs

  let
    -- TODO DELETE, we don't do key press anymore
    {-
    allowKB = constant True
    keyPressEv' k = (flip fmapMaybe) (_pFWidgetCtx_ev_input _toolWidgetConfig_pfctx) $ \case
      V.EvKey (V.KChar k') [] | k' == k -> Just ()
      _ -> Nothing
    keyPressEv k = onlyIfBeh (keyPressEv' k) allowKB
    -}
    keyPressEv k = never

    setTool = leftmost
      [Tool_Select <$ leftmost [ selectB, keyPressEv 'v']
      , Tool_Pan <$ leftmost [panB, keyPressEv ' ']
      , Tool_Box <$ leftmost [boxB, keyPressEv 'b']
      , Tool_Line <$ leftmost [lineB, keyPressEv 'l']
      , Tool_Text <$ leftmost [textB, keyPressEv 't']]

  vLayoutPad 4 $ debugStream [
    never
    , fmapLabelShow "radio" $ radioEvs
    , fmapLabelShow "selected" $ fmap ((:[]) . fromEnum) (updated _toolWidgetConfig_tool)
    ]


  return ToolWidget {
    _toolWidget_setTool = setTool
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
