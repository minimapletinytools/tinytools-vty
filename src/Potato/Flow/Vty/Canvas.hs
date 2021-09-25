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

lBox_to_region :: LBox -> Region
lBox_to_region (LBox (V2 x y) (V2 w h)) = Region x y w h

region_to_lBox :: Region -> LBox
region_to_lBox (Region x y w h) = (LBox (V2 x y) (V2 w h))

dynLBox_to_dynRegion :: (Reflex t) => Dynamic t LBox -> Dynamic t Region
dynLBox_to_dynRegion dlb = ffor dlb $ lBox_to_region

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
  , _canvasWidgetConfig_renderedCanvas :: Dynamic t RenderedCanvasRegion
  , _canvasWidgetConfig_canvas         :: Dynamic t SCanvas
  , _canvasWidgetConfig_handles        :: Dynamic t HandlerRenderOutput
}


data CanvasWidget t = CanvasWidget {
  _canvasWidget_mouse :: Event t LMouseData
  , _canvasWidget_regionDim :: Event t XY
}

holdCanvasWidget :: forall t m. (MonadWidget t m, HasPotato t m)
  => CanvasWidgetConfig t
  -> m (CanvasWidget t)
holdCanvasWidget CanvasWidgetConfig {..} = mdo

  potatostylebeh <- fmap _potatoConfig_style askPotato

  dh <- displayHeight
  dw <- displayWidth

  -- ::draw the canvas::
  let
    -- the screen region
    screenRegion' = ffor2 dw dh (\w h -> LBox 0 (V2 w h))
    -- the screen region in canvas space
    canvasScreenRegion' = fmap _renderedCanvasRegion_box _canvasWidgetConfig_renderedCanvas

    -- true region is the canvas region cropped to the panned screen (i.e. the intersection of screen and canvas in canvas space)
    maybeCropAndPan pan scanvas screen = maybe (LBox 0 0) (pan_lBox pan) $ intersect_lBox screen (_sCanvas_box scanvas)
    trueRegion' = ffor3 _canvasWidgetConfig_pan _canvasWidgetConfig_canvas canvasScreenRegion' maybeCropAndPan
    trueRegion = dynLBox_to_dynRegion trueRegion'
    oobRegions' = ffor2 screenRegion' trueRegion' $ \sc tr -> substract_lBox sc tr
    oobRegions = fmap (fmap lBox_to_region) oobRegions'

    -- reg is in screen space so we need to translate back to canvas space by undoing the pan
    renderRegionFn pan reg rc = renderedCanvasRegionToText (pan_lBox (-pan) (region_to_lBox reg)) rc

    renderRegion dreg = pane dreg (constDyn False) $ do
      text . current . ffor3 _canvasWidgetConfig_pan dreg _canvasWidgetConfig_renderedCanvas $ renderRegionFn

  -- TODO use correct theme
  localTheme (const (fmap _potatoStyle_softSelected potatostylebeh)) $ do
    fill (constant ' ')
    simpleList oobRegions renderRegion

  renderRegion trueRegion


  let
    makerhimage attr' (LBox (V2 px py) _) rh = r where
      LBox (V2 x y) (V2 w h) = _renderHandle_box rh
      rc = fromMaybe ' ' $ _renderHandle_char rh
      attr = attr' -- TODO eventually RenderHandle may be styled somehow
      r = V.translate (x+px) (y+py) $ V.charFill attr rc w h

  tellImages $ ffor3 (current _canvasWidgetConfig_handles) (fmap _potatoStyle_canvasCursor potatostylebeh) (current trueRegion')
    $ \(HandlerRenderOutput hs) attr reg -> fmap (makerhimage attr reg) hs

  inp <- makeLMouseDataInputEv 0 False
  postBuildEv <- getPostBuild

  let
    regionDimDyn = ffor2 dw dh V2
    regionDimEv = updated regionDimDyn
    forceDimEv = pushAlways (\_ -> sample . current $ regionDimDyn) postBuildEv


  return $ CanvasWidget inp (leftmost [regionDimEv, forceDimEv])
