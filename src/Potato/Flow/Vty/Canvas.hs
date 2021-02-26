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

import           Control.Lens
import qualified Data.IntMap.Strict             as IM
import           Data.These

import qualified Graphics.Vty                   as V
import           Reflex
import           Reflex.Vty

-- TODO this needs to come from Potato.Flow
defaultCanvasLBox :: LBox
defaultCanvasLBox = LBox (V2 0 0) (V2 100 50)

dynLBox_to_dynRegion :: (Reflex t) => Dynamic t LBox -> DynRegion t
dynLBox_to_dynRegion dlb = r where
  x' = flip fmap dlb $ \(LBox (V2 x _) _) -> x
  y' = flip fmap dlb $ \(LBox (V2 _ y) _) -> y
  w' = flip fmap dlb $ \(LBox _ (V2 w _)) -> w
  h' = flip fmap dlb $ \(LBox _ (V2 _ h)) -> h
  r = DynRegion x' y' w' h'

translate_dynRegion :: (Reflex t) => Dynamic t XY -> DynRegion t -> DynRegion t
translate_dynRegion pos dr = dr {
    _dynRegion_left = liftA2 (+) (_dynRegion_left dr) (fmap getx pos)
    , _dynRegion_top = liftA2 (+) (_dynRegion_top dr) (fmap gety pos)
  } where
    getx (V2 x _) = x
    gety (V2 _ y) = y

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

holdCanvasWidget :: forall t m. (MonadWidget t m)
  => CanvasWidgetConfig t
  -> VtyWidget t m (CanvasWidget t)
holdCanvasWidget CanvasWidgetConfig {..} = mdo
  -- ::draw the canvas::
  let
    renderedCanvas = _canvasWidgetConfig_renderedCanvas
    canvasRegion' = ffor2 _canvasWidgetConfig_pan renderedCanvas $ \pan rc -> pan_lBox pan (renderedCanvas_box rc)
    canvasRegion = dynLBox_to_dynRegion canvasRegion'
    --canvasRegion = translate_dynRegion _canvasWidgetConfig_pan $ dynLBox_to_dynRegion (fmap renderedCanvas_box renderedCanvas)
  fill '░'
  -- TODO render out of bounds stuff with gray background or whatveer
  pane canvasRegion (constDyn True) $ do
    text $ current (fmap renderedCanvasToText renderedCanvas)
  -- TODO proper handle Attr
  tellImages $ ffor3 (current _canvasWidgetConfig_handles) (constant V.defAttr) (current canvasRegion')
    $ \(HandlerRenderOutput hs) attr (LBox (V2 px py) _)-> fmap (\(LBox (V2 x y) (V2 w h)) -> V.translate (x+px) (y+py) $ V.charFill attr 'X' w h) hs

  inp <- makeLMouseDataInputEv 0 False
  return $ CanvasWidget inp
