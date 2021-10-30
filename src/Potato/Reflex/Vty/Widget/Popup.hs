{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Reflex.Vty.Widget.Popup (
  popupPane
  , popupPaneSimple
) where

import           Relude

import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget

import qualified Graphics.Vty.Input.Events as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

import           Data.Default

data PopupPaneSize = PopupPaneSize {
    _popupPaneSize_minWidth      :: Int
    ,_popupPaneSize_minHeight    :: Int
    , _popupPaneSize_widthRatio  :: Float
    , _popupPaneSize_heightRatio :: Float
  }

instance Default PopupPaneSize where
  def = PopupPaneSize 0 0 0.5 0.5

mulRatio :: Int -> Float -> Int
mulRatio i r =  ceiling . (*r) . fromIntegral $ i

type PopupInputWidget t m a =
  Event t () -- ^ escape button pressed
  -> Event t () -- ^ click outside box
  -> m (Event t (), Event t a) -- ^ (close event, output event)

-- TODO reduce constraints
popupPaneInternal :: forall t m a. (MonadWidget t m)
  => PopupPaneSize
  -> PopupInputWidget t m a -- ^ widget to be displayed in the popup
  -> m (Event t a, Event t ()) -- ^ (inner widget event, closed event)
popupPaneInternal PopupPaneSize {..} widgetFnEv = do
  screenWidthDyn <- displayWidth
  screenHeightDyn <- displayHeight
  let
    widthDyn = ffor screenWidthDyn (\sw -> max (mulRatio sw _popupPaneSize_widthRatio) _popupPaneSize_minWidth)
    heightDyn = ffor screenHeightDyn (\sh -> max (mulRatio sh _popupPaneSize_heightRatio) _popupPaneSize_minHeight)
    regionDyn = ffor2 ((,) <$> screenWidthDyn <*> screenHeightDyn) ((,) <$> widthDyn <*> heightDyn) $ \(sw,sh) (w,h) -> Region {
        _region_left = (sw - w) `div` 2
        , _region_top = (sh - h) `div` 2
        , _region_width = w
        , _region_height = h
      }
  escapeEv <- key V.KEsc
  outsideMouseEv <- mouseDown V.BLeft
  (outputEv, closeEv) <- pane regionDyn (constDyn True) $ do
    insideMouseEv <- mouseDown V.BLeft
    (closeEv', outputEv') <- widgetFnEv (void escapeEv) (void $ difference outsideMouseEv insideMouseEv)
    return (outputEv', closeEv')
  return (outputEv, closeEv)

-- TODO reduce constraints
-- | popupPane can only emit a single event before closing itself
-- clicking outside the popup closes the popup and emits no events (conisder disabling this as default behavior?)
popupPane :: forall t m a. (MonadWidget t m)
  => PopupPaneSize
  -> Event t (PopupInputWidget t m a)
  -> m (Event t a, Dynamic t Bool) -- ^ (inner widget event, popup state)
popupPane size widgetEv = mdo
  let
    emptyPopupWidget _ _ = return (never, never)
    inputEv = leftmost [widgetEv, canceledEv $> emptyPopupWidget]
  innerDynEv :: Dynamic t (Event t a, Event t ())
    <- networkHold (return (never, never)) (fmap (popupPaneInternal size) inputEv)
  let
    innerWidgetEv = switchDyn (fmap fst innerDynEv)
    canceledEv = switchDyn (fmap snd innerDynEv)
  outputStateDyn <- holdDyn False $ leftmostWarn "popupOverride" [widgetEv $> True, canceledEv $> False]
  return (innerWidgetEv, outputStateDyn)


-- | a simple popup pane
-- the inner popup pane event closes the popup pane (e.g. notification dialog box with "ok" button)
-- clicking outside or pressing escape closes the popup and emits no events
popupPaneSimple :: forall t m a. (MonadWidget t m)
  => PopupPaneSize
  -> Event t (m (Event t a)) -- ^ when inner event fires, popup is disabled
  -> m (Event t a, Dynamic t Bool) -- ^ (inner widget event, popup state)
popupPaneSimple size widgetEv = popupPane size fancyWidgetEv where
  fmapfn w = \escEv clickOutsideEv -> fmap (\outputEv -> (leftmost [escEv, clickOutsideEv, void outputEv], outputEv)) w
  fancyWidgetEv = fmap fmapfn widgetEv
