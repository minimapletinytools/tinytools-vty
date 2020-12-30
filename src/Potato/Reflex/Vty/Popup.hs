{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Reflex.Vty.Popup (
  popupWidget
  , popupOverrideWidget
) where

import           Relude

import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget

import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

-- TODO rename to popupPane
-- TODO needs to return some kind of click outside/escape cancel event
popupWidget :: forall t m a. (MonadWidget t m)
  => Int
  -> Int
  -> VtyWidget t m (Event t a)
  -> VtyWidget t m (Event t a)
popupWidget width height widget = do
  screenWidthDyn <- displayWidth
  screenHeightDyn <- displayHeight
  let
    widthDyn = constDyn width
    heightDyn = constDyn height
    regionDyn = DynRegion {
        _dynRegion_left = fmap (flip div 2) $ liftA2 (-) screenWidthDyn widthDyn
        , _dynRegion_top = fmap (flip div 2) $ liftA2 (-) screenHeightDyn heightDyn
        , _dynRegion_width = widthDyn
        , _dynRegion_height = heightDyn
      }
  pane2 regionDyn (constDyn True) widget

popupOverrideWidget :: forall t m a. (MonadWidget t m)
  => Int
  -> Int
  -> Event t (VtyWidget t m (Event t a)) -- ^ when inner event fires, popup is disabled
  -> VtyWidget t m (Event t a, Dynamic t Bool)
popupOverrideWidget width height widgetEv = mdo
  let
    emptyWidget = return never
    inputEv = leftmost [widgetEv, outputEv $> emptyWidget]
  innerDynEv :: Dynamic t (Event t a)
    <- networkHold emptyWidget (fmap (popupWidget width height) inputEv)
  let
    outputEv = switchDyn innerDynEv
  outputStateDyn <- holdDyn False $ leftmostWarn "popupOverride" [widgetEv $> True, outputEv $> False]
  return (outputEv, outputStateDyn)
