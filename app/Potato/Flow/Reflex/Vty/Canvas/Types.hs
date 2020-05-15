{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}


module Potato.Flow.Reflex.Vty.Canvas.Types (
  -- * generic stuff, prob should move into Potato.Reflex.Vty.Widget
  TrackedDrag(..)
  , trackDrag
  , neverTrackedDrag
  , captureTrackedDrag
  , alignTrackedDrag

  -- * canvas specific stuff
  , CanvasDrag
  , CanvasTrackedDrag
  , filterCanvasTrackedDrag
) where

import           Relude

import           Potato.Flow.Reflex.Vty.Tools
import           Potato.Reflex.Vty.Widget

import           Control.Lens                 (over, _1)
import           Control.Monad.Fix
import           Data.Semialign
import           Data.These
import           Data.Tuple.Extra

import           Potato.Flow
import           Reflex
import           Reflex.Potato.Helpers
import           Reflex.Vty


data TrackedDrag t a = TrackedDrag {
  -- | (user provided value, pan position at start of drag, drag info)
  -- the use provided value is most likely something produced by drag2AttachOnStart but could be anything
  _trackedDrag_drag       :: Event t (a, Drag2)
  , _trackedDrag_cancel   :: Event t ()
  , _trackedDrag_dragging :: Dynamic t Bool
}

instance (Reflex t) => Functor (TrackedDrag t) where
  fmap f td = td { _trackedDrag_drag = fmap (over _1 f) $ _trackedDrag_drag td }

-- NOTE should not be terrible if we every want to switch to Dynamic t (Dynamic t (a, Drag2)), but I think this way is simpler
trackDrag :: (Reflex t, MonadFix m, MonadHold t m)
  => Event t (a, Drag2)
  -> Event t () -- ^ force cancel event
  -> m (TrackedDrag t a)
trackDrag dragEv cancelEv = do
  let
    isTrackingDyn_foldfn :: Either DragState () -> Bool -> Maybe Bool
    isTrackingDyn_foldfn (Left DragStart) _ = Just True
    isTrackingDyn_foldfn (Left DragEnd) _   = Just False
    isTrackingDyn_foldfn (Right _) _        = Just False
    isTrackingDyn_foldfn _ _                = Nothing
  isTrackingDyn <- foldDynMaybe isTrackingDyn_foldfn False $ alignEitherAssert "tracking drag" (fmap (_drag2_state . snd) dragEv) cancelEv
  return $
    TrackedDrag  {
      _trackedDrag_drag = dragEv
      , _trackedDrag_cancel = cancelEv
      , _trackedDrag_dragging = isTrackingDyn
    }

neverTrackedDrag :: (Reflex t) => TrackedDrag t a
neverTrackedDrag = TrackedDrag {
    _trackedDrag_drag = never
    , _trackedDrag_cancel = never
    , _trackedDrag_dragging = constDyn False
  }

-- | chain these together to ensure each drag has only one consumer
captureTrackedDrag' :: forall t m a. (Reflex t, MonadFix m, MonadHold t m)
  => TrackedDrag t a
  -> Event t () -- ^ captures the whole drag if this event fires at the same time as a DragStart
  -> m (Dynamic t (TrackedDrag t a, TrackedDrag t a)) -- ^ (uncaptured, captured)
captureTrackedDrag' orig@TrackedDrag {..} didCaptureEv = do
  let
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

  isCapturingDyn <- foldDynMaybe isCapturingDyn_foldfn False $ align (alignEitherAssert "capture drag" (fmap (_drag2_state . snd) _trackedDrag_drag) _trackedDrag_cancel) didCaptureEv
  return $ fmap (\c -> if c then (neverTrackedDrag, orig) else (orig, neverTrackedDrag)) isCapturingDyn

switchTrackedDrag :: (Reflex t) => Dynamic t (TrackedDrag t a) -> TrackedDrag t a
switchTrackedDrag dtd = r where
  dragEv = switchDyn $ fmap _trackedDrag_drag dtd
  cancelEv = switchDyn $ fmap _trackedDrag_cancel dtd
  draggingBeh = join $ fmap _trackedDrag_dragging dtd
  r = TrackedDrag {
      _trackedDrag_drag = dragEv
      , _trackedDrag_cancel = cancelEv
      , _trackedDrag_dragging = draggingBeh
    }

captureTrackedDrag :: forall t m a. (Reflex t, MonadFix m, MonadHold t m)
  => TrackedDrag t a
  -> Event t () -- ^ captures the whole drag if this event fires at the same time as a DragStart
  -> m (TrackedDrag t a)
captureTrackedDrag orig didCaptureEv = do
  tddyn <- captureTrackedDrag' orig didCaptureEv
  return $ switchTrackedDrag (fmap fst tddyn)

alignTrackedDrag :: (Reflex t)
  => TrackedDrag t a
  -> Event t (Either (a ,Drag2) ())
alignTrackedDrag TrackedDrag {..} = alignEitherAssert "alignTrackedDrag" _trackedDrag_drag _trackedDrag_cancel


-- canvas specific stuff
type CanvasDrag = ((Tool, (Int,Int)), Drag2)
type CanvasTrackedDrag t = TrackedDrag t (Tool, (Int,Int))

-- potato filter for tracked drag events, does not pl
filterCanvasTrackedDrag :: forall t m. (Reflex t, MonadFix m, MonadHold t m)
  => Maybe Tool -- ^ tool state to select for
  -> CanvasTrackedDrag t
  -> m (CanvasTrackedDrag t , TrackedDrag t (Int,Int)) -- ^ (drags that were not caught by filter, filtered drag with Tool removed)
filterCanvasTrackedDrag c' trackedDrag = do
  let
    captureEv = fmapMaybe (\((c,_),_) -> if maybe True (c ==) c' then Just () else Nothing) (_trackedDrag_drag trackedDrag)
  tddyn <- captureTrackedDrag' trackedDrag captureEv
  let
    uncaptured = switchTrackedDrag (fmap fst tddyn)
    captured = switchTrackedDrag (fmap (fmap snd . snd) tddyn)
  return (uncaptured, captured)
