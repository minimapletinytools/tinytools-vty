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




type ReflexVtyTestT t uin uout m = ReflexTestT t (uin, ReflexTriggerRef t m VtyEvent) uout m

-- | queue a 'VtyEvent'
queueVtyEvent :: (Monad m, MonadRef m) => VtyEvent -> ReflexVtyTestT t uin uout m ()
queueVtyEvent vtyev = do
  (_, vtytref) <- inputTriggerRefs
  queueEventTriggerRef vtytref vtyev

-- | obtain vty inputs
vtyInputTriggerRefs :: (Monad m, MonadRef m) => ReflexVtyTestT t uin uout m (ReflexTriggerRef t m VtyEvent)
vtyInputTriggerRefs = do
  (_, vtytrefs) <- inputTriggerRefs
  return vtytrefs

-- | obtain user inputs
userInputTriggerRefs :: (Monad m, MonadRef m) => ReflexVtyTestT t uin uout m uin
userInputTriggerRefs = do
  (usertrefs, _) <- inputTriggerRefs
  return usertrefs

-- | if (local) mouse coordinates are outside of the (absolute) region, returns False and does not queue any event
queueMouseEventInRegion :: (Reflex t, MonadSample t m, Monad m, MonadRef m) 
  => DynRegion t 
  -> Either MouseDown MouseUp -- ^ mouse coordinates are LOCAL to the input region
  -> ReflexVtyTestT t uin uout m Bool
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
  -> ReflexVtyTestT t uin uout m (NonEmpty [a]) -- ^ collected outputs
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
class MonadReflexVtyTest t m | m -> t where
  type UserInputTriggerRefs m :: Type
  type UserOutputEvents m :: Type
  queueVtyEvent :: VtyEvent -> m ()

newtype ReflexVtyTestT t uin uout m a = ReflexVtyTestT { unReflexVtyTestT :: ReflexTestT t (uin, ReflexTriggerRef t m VtyEvent) uout m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (AppState t m))

instance MonadTrans (ReflexVtyTestT t uin uout) where
  lift = ReflexVtyTestT . lift

instance (r ~ ReflexTriggerRef t m VtyEvent, Monad m) => MonadReader ((uin, r), uout) (ReflexVtyTestT t uin uout m) where
  ask :: ReflexVtyTestT t uin uout m ((uin, r), uout)
  ask = ReflexVtyTestT ask --(ask :: ReflexTestT t (uin,r) uout m (uin,r))
--deriving instance (r ~ ReflexTriggerRef t m VtyEvent) => MonadReader (uin,r) (ReflexVtyTestT t uin uout m) --via ReflexTestT t (uin,r) uout m

instance MonadReflexVtyTest t (ReflexVtyTestT t uin uout m) where
  type UserInputTriggerRefs (ReflexVtyTestT t uin uout m) = uin
  type UserOutputEvents (ReflexVtyTestT t uin uout m) = uout
  queueVtyEvent vtyev = do
    ((_, vtytref),_)  :: ((uin, ReflexTriggerRef t m VtyEvent), uout) <- ask
    queueEventTriggerRef vtytref vtyev
-}