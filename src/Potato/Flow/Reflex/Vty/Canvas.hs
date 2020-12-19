{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Canvas (
  CanvasWidgetConfig(..)
  , CanvasWidget(..)
  , holdCanvasWidget
) where


import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget
import           Reflex.Potato.Helpers

import           Control.Lens
import qualified Data.IntMap.Strict                 as IM
import           Data.These

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Vty


data CanvasWidgetConfig t = CanvasWidgetConfig {
  _canvasWidgetConfig_pfctx :: PFWidgetCtx t
}

data CanvasWidget t = CanvasWidget {
  _canvasWidget_isManipulating      :: Dynamic t Bool

  , _canvasWidget_addSEltLabel      :: Event t (Bool, (LayerPos, SEltLabel))
  , _canvasWidget_modify            :: Event t (Bool, ControllersWithId)

  , _canvasWidget_consumingKeyboard :: Behavior t Bool
  , _canvasWidget_select            :: Event t (Bool, Either [REltId] [REltId]) -- ^ (left is select single, right is select many)
  , _canvasWidget_undo              :: Event t ()
}

holdCanvasWidget :: forall t m. (MonadWidget t m)
  => CanvasWidgetConfig t
  -> VtyWidget t m (CanvasWidget t)
holdCanvasWidget CanvasWidgetConfig {..} = mdo
  return CanvasWidget {
      -- TODO
      _canvasWidget_isManipulating = constDyn False
      , _canvasWidget_addSEltLabel = never
      , _canvasWidget_modify = never
      , _canvasWidget_select = never
      -- TODO
      , _canvasWidget_consumingKeyboard = constant False
      , _canvasWidget_undo = never

    }
