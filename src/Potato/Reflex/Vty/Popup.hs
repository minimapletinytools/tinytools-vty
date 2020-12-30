{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Reflex.Vty.Popup (
  popupWidget
  , popupOverrideWidget
) where

import           Relude

import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget

import qualified Graphics.Vty.Input.Events as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

-- TODO rename to popupPaneInternal
-- TODO needs to return some kind of click outside/escape cancel event
popupWidget :: forall t m a. (MonadWidget t m)
  => Int
  -> Int
  -> VtyWidget t m (Event t a)
  -> VtyWidget t m (Event t a, Event t ()) -- ^ (inner widget event, canceled event)
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
  outsideMouseEv <- mouseDown V.BLeft
  (innerWidgetEv, insideMouseEv) <- pane2 regionDyn (constDyn True) $ do
    insideMouseEv' <- mouseDown V.BLeft
    innerWidgetEv' <- widget
    return (innerWidgetEv', insideMouseEv')
  escapeEv <- key V.KEsc
  let
    canceledEv = leftmost [void $ difference outsideMouseEv insideMouseEv, void escapeEv]
  return (innerWidgetEv, canceledEv)


-- TODO rename to popupPane
-- | popup can only emit a single event before closing itself
popupOverrideWidget :: forall t m a. (MonadWidget t m)
  => Int
  -> Int
  -> Event t (VtyWidget t m (Event t a)) -- ^ when inner event fires, popup is disabled
  -> VtyWidget t m (Event t a, Dynamic t Bool) -- ^ (inner widget event, canceled event, popup state)
popupOverrideWidget width height widgetEv = mdo
  let
    inputEv = leftmost [widgetEv, innerWidgetEv $> return never, canceledEv $> return never]
  innerDynEv :: Dynamic t (Event t a, Event t ())
    <- networkHold (return (never, never)) (fmap (popupWidget width height) inputEv)
  let
    innerWidgetEv = switchDyn (fmap fst innerDynEv)
    canceledEv = switchDyn (fmap snd innerDynEv)
  outputStateDyn <- holdDyn False $ leftmostWarn "popupOverride" [widgetEv $> True, innerWidgetEv $> False, canceledEv $> False]
  return (innerWidgetEv, outputStateDyn)
