
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Layer (
  LayerWidgetConfig(..)
  , LayerWidget(..)
  , holdLayerWidget
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.Attrs
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget


import           Control.Monad.Fix
import           Data.Align
import           Data.Dependent.Sum                 (DSum ((:=>)))
import qualified Data.IntMap.Strict                 as IM
import qualified Data.List                          as L
import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T
import           Data.Text.Zipper
import qualified Data.Text.Zipper                   as TZ
import           Data.These

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

--moveChar :: Char
--moveChar = '≡'
hiddenChar :: Char
hiddenChar = '-'
visibleChar :: Char
visibleChar = 'e'
lockedChar :: Char
lockedChar = '@'
unlockedChar :: Char
unlockedChar = 'a'
expandChar :: Char
expandChar = '»'
closeChar :: Char
closeChar = '«'

{-# INLINE if' #-}
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y



data LayerWidgetConfig t = LayerWidgetConfig {
  _layerWidgetConfig_pfctx              :: PFWidgetCtx t
}

data LayerWidget t = LayerWidget {
  _layerWidget_select              :: Event t (Bool, LayerPos)
  , _layerWidget_changeName        :: Event t ControllersWithId
  -- TODO expand to support multi-move
  , _layerWidget_move              ::Event t (LayerPos, LayerPos)
  , _layerWidget_consumingKeyboard :: Behavior t Bool

  -- TODO
  --, _layerWidget_layerWidgetTestOutput :: LayerWidgetTestOutput t
}

holdLayerWidget :: forall t m. (Adjustable t m, PostBuild t m, NotReady t m,  MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
holdLayerWidget _ = return $ LayerWidget never never never (constant False)
