{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Canvas (
  CanvasWidgetConfig(..)
  , CanvasWidget(..)
  , holdCanvasWidget
) where


import           Relude

import           Potato.Flow
import           Potato.Flow.Controller
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Math
import           Potato.Flow.Vty.Input
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget
import           Reflex.Potato.Helpers
import Potato.Flow.Vty.PotatoReader

import           Control.Lens
import qualified Data.IntMap.Strict             as IM
import           Data.These

import qualified Graphics.Vty                   as V
import           Reflex
import           Reflex.Vty

-- TODO this needs to come from Potato.Flow
defaultCanvasLBox :: LBox
defaultCanvasLBox = LBox (V2 0 0) (V2 100 50)

dynLBox_to_dynRegion :: (Reflex t) => Dynamic t LBox -> Dynamic t Region
dynLBox_to_dynRegion dlb = ffor dlb $ \(LBox (V2 x y) (V2 w h)) -> Region x y w h

translate_dynRegion :: (Reflex t) => Dynamic t XY -> Dynamic t Region -> Dynamic t Region
translate_dynRegion dpos dr = ffor2 dpos dr $ \(V2 x y) region -> region {
    _region_left = _region_left region + x
    , _region_top = _region_top region + y
  }

pan_lBox :: XY -> LBox -> LBox
pan_lBox pan (LBox p s) = LBox (p+pan) s

data CanvasWidgetConfig t = CanvasWidgetConfig {
  _canvasWidgetConfig_pan            :: Dynamic t XY
  -- TODO DELETE
  , _canvasWidgetConfig_broadPhase     :: Dynamic t BroadPhaseState
  , _canvasWidgetConfig_renderedCanvas :: Dynamic t RenderedCanvas
  , _canvasWidgetConfig_canvas         :: Dynamic t SCanvas
  , _canvasWidgetConfig_handles        :: Dynamic t HandlerRenderOutput
}

data CanvasWidget t = CanvasWidget {
  _canvasWidget_mouse :: Event t LMouseData
}

holdCanvasWidget :: forall t m. (MonadWidget t m, HasPotato t m)
  => CanvasWidgetConfig t
  -> m (CanvasWidget t)
holdCanvasWidget CanvasWidgetConfig {..} = mdo

  potatostylebeh <- fmap _potatoConfig_style askPotato
  potatostyle <- sample potatostylebeh

  -- ::draw the canvas::
  let
    renderedCanvas = _canvasWidgetConfig_renderedCanvas
    canvasRegion' = ffor2 _canvasWidgetConfig_pan renderedCanvas $ \pan rc -> pan_lBox pan (renderedCanvas_box rc)
    canvasRegion = dynLBox_to_dynRegion canvasRegion'
    --canvasRegion = translate_dynRegion _canvasWidgetConfig_pan $ dynLBox_to_dynRegion (fmap renderedCanvas_box renderedCanvas)
  fill (constant '░')
  -- TODO render out of bounds stuff with gray background or whatveer
  pane canvasRegion (constDyn True) $ do
    text $ current (fmap renderedCanvasToText renderedCanvas)
  -- TODO proper handle Attr
  tellImages $ ffor3 (current _canvasWidgetConfig_handles) (fmap _potatoStyle_canvasCursor potatostylebeh) (current canvasRegion')
    $ \(HandlerRenderOutput hs) attr (LBox (V2 px py) _)-> fmap (\(LBox (V2 x y) (V2 w h)) -> V.translate (x+px) (y+py) $ V.charFill attr 'X' w h) hs

  inp <- makeLMouseDataInputEv 0 False
  return $ CanvasWidget inp
