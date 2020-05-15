{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}


module Potato.Flow.Reflex.Vty.Canvas.Types (
  TrackedDrag(..)
  , trackDrag
  , captureTrackedDrag
  , switchTrackedDrag
) where

import           Relude

import           Potato.Flow.Reflex.Vty.Tools
import           Potato.Reflex.Vty.Widget

import           Control.Monad.Fix
import           Data.Semialign
import           Data.These
import           Data.Tuple.Extra

import           Potato.Flow
import           Reflex
import           Reflex.Potato.Helpers
import           Reflex.Vty

type CanvasDrag = (Tool, (Int,Int), Drag2)

data TrackedDrag t = TrackedDrag {
  _trackedDrag_drag       :: Event t CanvasDrag -- (tool at start of drag, pan position at start of drag, drag info)
  , _trackedDrag_cancel   :: Event t ()
  , _trackedDrag_dragging :: Dynamic t Bool
}

trackDrag :: (Reflex t, MonadFix m, MonadHold t m)
  => Event t (Tool, (Int, Int), Drag2)
  -> Event t () -- ^ force cancel event
  -> m (TrackedDrag t)
trackDrag dragEv cancelEv = do
  let
    isTrackingDyn_foldfn :: Either DragState () -> Bool -> Maybe Bool
    isTrackingDyn_foldfn (Left DragStart) _ = Just True
    isTrackingDyn_foldfn (Left DragEnd) _   = Just False
    isTrackingDyn_foldfn (Right _) _        = Just False
    isTrackingDyn_foldfn _ _                = Nothing
  isTrackingDyn <- foldDynMaybe isTrackingDyn_foldfn False $ alignEitherAssert "tracking drag" (fmap (_drag2_state . thd3) dragEv) cancelEv
  return $
    TrackedDrag  {
      _trackedDrag_drag = dragEv
      , _trackedDrag_cancel = cancelEv
      , _trackedDrag_dragging = isTrackingDyn
    }

captureTrackedDrag :: forall t m. (Reflex t, MonadFix m, MonadHold t m)
  => TrackedDrag t
  -> Event t () -- ^ captures the whole drag if this event fires at the same time as a DragStart
  -> m (Dynamic t (TrackedDrag t))
captureTrackedDrag orig@TrackedDrag {..} didCaptureEv = do
  let
    neverPass = TrackedDrag {
        _trackedDrag_drag = never
        , _trackedDrag_cancel = never
        , _trackedDrag_dragging = constDyn False
      }

    -- TODO rewrite this to use _trackedDrag_dragging
    isCapturingDyn_foldfn :: These (Either DragState ()) () -> Bool -> Maybe Bool
    -- begin capture on simultaneous DragStart and capture events
    isCapturingDyn_foldfn (These (Left DragStart) _) _ = Just True
    -- end capture on DragEnd events
    isCapturingDyn_foldfn (These (Left DragEnd) _) _   = Just False
    isCapturingDyn_foldfn (This (Left DragEnd)) _      = Just False
    -- end capture on cancel events
    isCapturingDyn_foldfn (These (Right _) _) _        = Just False
    isCapturingDyn_foldfn (This (Right _)) _           = Just False
    isCapturingDyn_foldfn _ _                          = Nothing

  isCapturingDyn <- foldDynMaybe isCapturingDyn_foldfn False $ align (alignEitherAssert "capture drag" (fmap (_drag2_state . thd3) _trackedDrag_drag) _trackedDrag_cancel) didCaptureEv
  return $ fmap (\c -> if c then neverPass else orig) isCapturingDyn

switchTrackedDrag :: (Reflex t) => Dynamic t (TrackedDrag t) -> TrackedDrag t
switchTrackedDrag dtd = r where
  dragEv = switchDyn $ fmap _trackedDrag_drag dtd
  cancelEv = switchDyn $ fmap _trackedDrag_cancel dtd
  draggingBeh = join $ fmap _trackedDrag_dragging dtd
  r = TrackedDrag {
      _trackedDrag_drag = dragEv
      , _trackedDrag_cancel = cancelEv
      , _trackedDrag_dragging = draggingBeh
    }
