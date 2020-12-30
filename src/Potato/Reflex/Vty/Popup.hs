{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Reflex.Vty.Popup (
  popupPaneSimple
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

popupPaneSimpleInternal :: forall t m a. (MonadWidget t m)
  => PopupPaneSize
  -> VtyWidget t m (Event t a)
  -> VtyWidget t m (Event t a, Event t ()) -- ^ (inner widget event, canceled event)
popupPaneSimpleInternal PopupPaneSize {..} widget = do
  screenWidthDyn <- displayWidth
  screenHeightDyn <- displayHeight
  let
    widthDyn = ffor screenWidthDyn (\sw -> max (mulRatio sw _popupPaneSize_widthRatio) _popupPaneSize_minWidth)
    heightDyn = ffor screenHeightDyn (\sh -> max (mulRatio sh _popupPaneSize_heightRatio) _popupPaneSize_minHeight)
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


-- | popupPane can only emit a single event before closing itself
popupPaneSimple :: forall t m a. (MonadWidget t m)
  => PopupPaneSize
  -> Event t (VtyWidget t m (Event t a)) -- ^ when inner event fires, popup is disabled
  -> VtyWidget t m (Event t a, Dynamic t Bool) -- ^ (inner widget event, popup state)
popupPaneSimple size widgetEv = mdo
  let
    inputEv = leftmost [widgetEv, innerWidgetEv $> return never, canceledEv $> return never]
  innerDynEv :: Dynamic t (Event t a, Event t ())
    <- networkHold (return (never, never)) (fmap (popupPaneSimpleInternal size) inputEv)
  let
    innerWidgetEv = switchDyn (fmap fst innerDynEv)
    canceledEv = switchDyn (fmap snd innerDynEv)
  outputStateDyn <- holdDyn False $ leftmostWarn "popupOverride" [widgetEv $> True, innerWidgetEv $> False, canceledEv $> False]
  return (innerWidgetEv, outputStateDyn)


type PopupInputWidget t m a =
  Event t () -- ^ escape button pressed
  -> Event t () -- ^ click outside box
  -> VtyWidget t m (Event t (), Event t a) -- ^ (close event, output event)

popupPaneFancy :: forall t m a. (MonadWidget t m)
  => Int -- ^ width
  -> Int -- ^ height
  -> Bool -- ^ allow drag
  -> Event t (PopupInputWidget t m a) -- ^ when inner event fires, popup is disabled
  -> VtyWidget t m (Event t a, Dynamic t Bool) -- ^ (inner widget event, popup state)
popupPaneFancy width height allowDrag = error "TODO"
