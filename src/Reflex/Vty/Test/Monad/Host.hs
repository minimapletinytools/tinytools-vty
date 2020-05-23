{-# LANGUAGE UndecidableInstances #-}

module Reflex.Vty.Test.Monad.Host
  (
  )
where

import           Relude

import Control.Monad.Ref
import qualified Data.List.NonEmpty as NE

import           Reflex
import           Reflex.Host.Class
import           Reflex.Test.Monad.Host
import           Reflex.Vty
import qualified Graphics.Vty as V


type ReflexVtyTestT t uintref uout m = ReflexTestT t (uintref, ReflexTriggerRef t m VtyEvent) (uout, Behavior t [V.Image]) m

-- | queue a 'VtyEvent'
queueVtyEvent :: (Monad m, MonadRef m) => VtyEvent -> ReflexVtyTestT t uintref uout m ()
queueVtyEvent vtyev = do
  (_, vtytref) <- inputTriggerRefs
  queueEventTriggerRef vtytref vtyev

-- | obtain vty inputs
vtyInputTriggerRefs :: (Monad m, MonadRef m) => ReflexVtyTestT t uintref uout m (ReflexTriggerRef t m VtyEvent)
vtyInputTriggerRefs = do
  (_, vtytrefs) <- inputTriggerRefs
  return vtytrefs

-- | obtain user defined inputs
userInputTriggerRefs :: (Monad m, MonadRef m) => ReflexVtyTestT t uintref uout m uintref
userInputTriggerRefs = do
  (usertrefs, _) <- inputTriggerRefs
  return usertrefs

-- | obtain user defined outputs
userOutputs :: (Monad m, MonadRef m) => ReflexVtyTestT t uintref uout m uout
userOutputs = do
  (useroutputs, _) <- outputs
  return useroutputs

-- | obtain vty outputs
vtyOutputs :: (Monad m, MonadRef m) => ReflexVtyTestT t uintref uout m (Behavior t [V.Image])
vtyOutputs = do
  (_, vtyoutputs) <- outputs
  return vtyoutputs

-- | if (local) mouse coordinates are outside of the (absolute) region, returns False and does not queue any event
queueMouseEventInRegion :: (Reflex t, MonadSample t m, Monad m, MonadRef m) 
  => DynRegion t 
  -> Either MouseDown MouseUp -- ^ mouse coordinates are LOCAL to the input region
  -> ReflexVtyTestT t uintref uout m Bool
queueMouseEventInRegion dr mouse = do
  region <- sample . currentRegion $ dr
  let
    absCoords (Region l t _ _) (x,y) = (x+l, y+t)
    coordinates = case mouse of 
      Left (MouseDown _ c _) -> c
      Right (MouseUp _ c) -> c
    withinRegion (Region _ _ w h) (x,y) = not $ or [ x < 0, y < 0, x >= w, y >= h ]
    --withinRegion (Region l t w h) (x,y) = not $ or [ x < l, y < t, x >= l + w, y >= t + h ]
  
  if withinRegion region coordinates
    then do
      case mouse of 
        Left (MouseDown b c mods) -> queueVtyEvent $ uncurry V.EvMouseDown (absCoords region c) b mods
        Right (MouseUp b c) -> queueVtyEvent $ uncurry V.EvMouseUp (absCoords region c) b
      return True
    else return False

-- | queue and fire a series of mouse events representing a mouse drag
-- returns collected outputs 
queueMouseDrag :: (Monad m, MonadRef m) 
  => V.Button -- ^ button to press
  -> [V.Modifier] -- ^ modifier held during drag
  -> NonEmpty (Int,Int) -- ^ list of drag positions
  -- TODO add something like DragState to this
  -> ((Int,Int) -> ReadPhase m a) -- ^ ReadPhase to run after each normal drag 
  -> ReflexVtyTestT t uintref uout m (NonEmpty [a]) -- ^ collected outputs
queueMouseDrag b mods ps rps = do
  let
    dragPs' = init ps
    -- if there is only 1 elt in ps, then simulate a single click
    dragPs = fromMaybe (pure (head ps)) $ viaNonEmpty id dragPs'  
    endP = last ps
  initas <- forM dragPs $ \p -> do
    queueVtyEvent (uncurry V.EvMouseDown p b mods) 
    fireQueuedEventsAndRead (rps p)
  queueVtyEvent (uncurry V.EvMouseUp endP (Just b))
  lastas <- fireQueuedEventsAndRead (rps endP)
  return $ initas <> (lastas :| [])


{-
-- class variant which I couldn't figure out how to get working...

class MonadReflexVtyTest t m | m -> t where
  type UserInputTriggerRefs m :: Type
  type UserOutputEvents m :: Type
  queueVtyEvent :: VtyEvent -> m ()

newtype ReflexVtyTestT t uintref uout m a = ReflexVtyTestT { unReflexVtyTestT :: ReflexTestT t (uintref, ReflexTriggerRef t m VtyEvent) uout m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (AppState t m))

instance MonadTrans (ReflexVtyTestT t uintref uout) where
  lift = ReflexVtyTestT . lift

instance (r ~ ReflexTriggerRef t m VtyEvent, Monad m) => MonadReader ((uintref, r), uout) (ReflexVtyTestT t uintref uout m) where
  ask :: ReflexVtyTestT t uintref uout m ((uintref, r), uout)
  ask = ReflexVtyTestT ask --(ask :: ReflexTestT t (uintref,r) uout m (uintref,r))
--deriving instance (r ~ ReflexTriggerRef t m VtyEvent) => MonadReader (uintref,r) (ReflexVtyTestT t uintref uout m) --via ReflexTestT t (uintref,r) uout m

instance MonadReflexVtyTest t (ReflexVtyTestT t uintref uout m) where
  type UserInputTriggerRefs (ReflexVtyTestT t uintref uout m) = uintref
  type UserOutputEvents (ReflexVtyTestT t uintref uout m) = uout
  queueVtyEvent vtyev = do
    ((_, vtytref),_)  :: ((uintref, ReflexTriggerRef t m VtyEvent), uout) <- ask
    queueEventTriggerRef vtytref vtyev
-}