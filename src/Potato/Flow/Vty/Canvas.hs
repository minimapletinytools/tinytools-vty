{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Canvas (
  CanvasWidgetConfig(..)
  , CanvasWidget(..)
  , holdCanvasWidget
) where


import           Relude

import           Potato.Flow
import           Potato.Flow.Vty.Input
import           Potato.Reflex.Vty.Helpers
import Potato.Flow.Vty.PotatoReader

import qualified Data.Text as T
import qualified Data.List.Index as L
import Data.Tuple.Extra (thd3)

import qualified Graphics.Vty                   as V
import           Reflex
import           Reflex.Vty


-- alternative text rendering methods that don't show spaces
textNoRenderSpaces
  :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m)
  => Behavior t Text
  -> m ()
textNoRenderSpaces t = do
  bt <- theme
  let img = (\a s -> [makeimages a s])
        <$> bt
        <*> t
  tellImages (fmap join img)
  where
    -- revout is of type [(Text, Int)] where the int is offset from BoL
    foldlinefn (offset, spaces, revout) c = (offset+1, newspaces, newrevout) where
      (newspaces, newrevout) = if c == ' '
        then (spaces+1, revout)
        else if spaces /= 0
          then (0, ([c], offset):revout)
          else case revout of
            (x,n):xs -> (0, (c:x,n):xs)
            -- first character case
            [] -> (0, [([c], 0)])
    makeimages th =
      -- (\x -> traceShow (length x) x) .
      join
      . L.imap (\i -> fmap (V.translateY i)) -- for each line, offset the image vertically
      . fmap (fmap (\(txt,offset) -> V.translateX offset $  V.string th (reverse txt))) -- for each chunk and offset, convert to image
      . fmap (thd3 . foldl' foldlinefn (0,0,[]) . T.unpack) -- for each line, split into chunks with offset
      . T.split (=='\n') -- split into lines

lBox_to_region :: LBox -> Region
lBox_to_region (LBox (V2 x y) (V2 w h)) = Region x y w h

region_to_lBox :: Region -> LBox
region_to_lBox (Region x y w h) = (LBox (V2 x y) (V2 w h))

dynLBox_to_dynRegion :: (Reflex t) => Dynamic t LBox -> Dynamic t Region
dynLBox_to_dynRegion dlb = ffor dlb $ lBox_to_region

{- DELETE ME
translate_dynRegion :: (Reflex t) => Dynamic t XY -> Dynamic t Region -> Dynamic t Region
translate_dynRegion dpos dr = ffor2 dpos dr $ \(V2 x y) region -> region {
    _region_left = _region_left region + x
    , _region_top = _region_top region + y
  }
-}

pan_lBox :: XY -> LBox -> LBox
pan_lBox pan (LBox p s) = LBox (p+pan) s

data CanvasWidgetConfig t = CanvasWidgetConfig {
  _canvasWidgetConfig_pan            :: Dynamic t XY
  -- TODO DELETE
  , _canvasWidgetConfig_broadPhase     :: Dynamic t BroadPhaseState
  , _canvasWidgetConfig_renderedCanvas :: Dynamic t RenderedCanvasRegion
  , _canvasWidgetConfig_renderedSelection :: Dynamic t RenderedCanvasRegion
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
    screenRegion = dynLBox_to_dynRegion screenRegion'
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


    -- same as renderRegionFn
    debugRenderRegionFn pan reg rc = r where
      txt = renderedCanvasRegionToText (pan_lBox (-pan) (region_to_lBox reg)) rc
      --r = trace (T.unpack txt) txt
      r = txt

  -- 1. render out of bounds region
  -- TODO use correct theme
  localTheme (const (fmap _potatoStyle_softSelected potatostylebeh)) $ do
    fill (constant ' ')
    simpleList oobRegions renderRegion
    return ()

  -- 2. render the canvas region
  renderRegion trueRegion

  -- 3. render the selection
  -- TODO use correct theme
  localTheme (const (fmap _potatoStyle_selected potatostylebeh)) $ do
    textNoRenderSpaces . current . ffor3 _canvasWidgetConfig_pan screenRegion _canvasWidgetConfig_renderedSelection $ debugRenderRegionFn
    return ()





  let
    makerhimage attr' (LBox (V2 px py) _) rh = r where
      LBox (V2 x y) (V2 w h) = _renderHandle_box rh
      rc = fromMaybe ' ' $ _renderHandle_char rh
      attr = attr' -- TODO eventually RenderHandle may be styled somehow
      r = V.translate (x+px) (y+py) $ V.charFill attr rc w h

  tellImages $ ffor3 (current _canvasWidgetConfig_handles) (fmap _potatoStyle_makeCanvasManipulator potatostylebeh) (current trueRegion')
    $ \(HandlerRenderOutput hs) attrfn reg -> fmap (\rh -> makerhimage (attrfn (_renderHandle_color rh)) reg rh) hs

  inp <- makeLMouseDataInputEv (constDyn (0,0)) False
  postBuildEv <- getPostBuild

  let
    regionDimDyn = ffor2 dw dh V2
    regionDimEv = updated regionDimDyn
    forceDimEv = pushAlways (\_ -> sample . current $ regionDimDyn) postBuildEv


  return $ CanvasWidget inp (leftmost [regionDimEv, forceDimEv])
