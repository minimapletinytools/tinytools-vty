{-|
Module: Potato.Reflex.Vty.Host
Description: Potato version of Reflex.Vty.Host where render events are skipped to improve speed

-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Potato.Reflex.Vty.Host
  ( VtyApp
  , VtyResult(..)
  , getDefaultVty
  , runVtyApp
  , runVtyAppWithHandle
  , MonadVtyApp
  , VtyEvent
  ) where

import Prelude

import Control.Concurrent (forkIO, killThread, MVar, newMVar, readMVar, modifyMVar_)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (onException)
import Control.Monad (forM, forM_, forever)
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef, Ref, readRef)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (IORef, readIORef)
import Data.Maybe (catMaybes)

import Reflex
import Reflex.Host.Class
import Reflex.Spider.Orphans ()
import Graphics.Vty (DisplayRegion)
import qualified Graphics.Vty as V





-- | A synonym for the underlying vty event type from 'Graphics.Vty'. This should
-- probably ultimately be replaced by something defined in this library.
type VtyEvent = V.Event

-- | The output of a 'VtyApp'.
data VtyResult t = VtyResult
  { _vtyResult_picture :: Behavior t V.Picture
  -- ^ The current vty output. 'runVtyAppWithHandle' samples this value every time an
  -- event fires and updates the display.
  , _vtyResult_shutdown :: Event t ()
  -- ^ An event that requests application termination.
  }

-- | The constraints necessary to run a 'VtyApp'. See 'runVtyAppWithHandle' for more
-- on why each of these are necessary and how they can be fulfilled.
type MonadVtyApp t m =
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  , PrimMonad (HostFrame t)
  , ReflexHost t
  , MonadIO (HostFrame t)
  , Ref m ~ IORef
  , Ref (HostFrame t) ~ IORef
  , MonadRef (HostFrame t)
  , NotReady t m
  , TriggerEvent t m
  , PostBuild t m
  , PerformEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  , MonadSample t (Performable m)
  , Adjustable t m
  )

-- | A functional reactive vty application.
type VtyApp t m = MonadVtyApp t m
  => DisplayRegion
  -- ^ The initial display size (updates to this come as events)
  -> Event t V.Event
  -- ^ Vty input events.
  -> m (VtyResult t)
  -- ^ The output of the 'VtyApp'. The application runs in a context that,
  -- among other things, allows new events to be created and triggered
  -- ('TriggerEvent'), provides access to an event that fires immediately upon
  -- app instantiation ('PostBuild'), and allows actions to be run upon
  -- occurrences of events ('PerformEvent').

-- | Runs a 'VtyApp' in a given 'Graphics.Vty.Vty'.
-- Same as Reflex.Vty.runVtyAppWithHandle except does some bonus potato stuff
runVtyAppWithHandle
  :: V.Vty
  -- ^ A 'Graphics.Vty.Vty' handle.
  -> (forall t m. VtyApp t m)
  -- ^ A functional reactive vty application.
  -> IO ()
runVtyAppWithHandle vty vtyGuest = flip onException (V.shutdown vty) $

  -- We are using the 'Spider' implementation of reflex. Running the host
  -- allows us to take actions on the FRP timeline. The scoped type signature
  -- specifies that our host runs on the Global timeline.
  -- For more information, see 'Reflex.Spider.Internal.runSpiderHost'.
  (runSpiderHost :: SpiderHost Global a -> IO a) $ do

    -- Create an 'Event' and a "trigger" reference for that event. The trigger
    -- reference can be used to determine whether anyone is "subscribed" to
    -- that 'Event' and, therefore, whether we need to bother performing any
    -- updates when the 'Event' fires.
    -- The 'Event' below will be used to convey vty input events.
    (vtyEvent, vtyEventTriggerRef) <- newEventWithTriggerRef

    -- Create the "post-build" event and associated trigger. This event fires
    -- once, when the application starts.
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef

    -- Create a queue to which we will write 'Event's that need to be
    -- processed.
    events <- liftIO newChan
    triggerEvents <- liftIO newChan
    chanSizeVar :: MVar Int <- liftIO $ newMVar 0

    displayRegion0 <- liftIO $ V.displayBounds $ V.outputIface vty

    -- Run the vty "guest" application, providing the appropriate context. The
    -- result is a 'VtyResult', and a 'FireCommand' that will be used to
    -- trigger events.
    (vtyResult, fc@(FireCommand fire)) <- do
      hostPerformEventT $                 -- Allows the guest app to run
                                          -- 'performEvent', so that actions
                                          -- (e.g., IO actions) can be run when
                                          -- 'Event's fire.

        flip runPostBuildT postBuild $    -- Allows the guest app to access to
                                          -- a "post-build" 'Event'

          flip runTriggerEventT triggerEvents $  -- Allows the guest app to create new
                                          -- events and triggers and writes
                                          -- those triggers to a channel from
                                          -- which they will be read and
                                          -- processed.

            vtyGuest displayRegion0 vtyEvent
                                          -- The guest app is provided the
                                          -- initial display region and an
                                          -- 'Event' of vty inputs.

    -- Reads the current value of the 'Picture' behavior and updates the
    -- display with it. This will be called whenever we determine that a
    -- display update is necessary. In this implementation that is when various
    -- events occur.
    let updateVty =
          sample (_vtyResult_picture vtyResult) >>= \x -> do
            n <- liftIO . readMVar $ chanSizeVar
            if n < 5
              then liftIO . V.update vty $ x
              else return ()

    -- Read the trigger reference for the post-build event. This will be
    -- 'Nothing' if the guest application hasn't subscribed to this event.
    mPostBuildTrigger <- readRef postBuildTriggerRef

    -- When there is a subscriber to the post-build event, fire the event.
    forM_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ return ()

    -- After firing the post-build event, sample the vty result and update
    -- the display.
    updateVty

    -- Subscribe to an 'Event' of that the guest application can use to
    -- request application shutdown. We'll check whether this 'Event' is firing
    -- to determine whether to terminate.
    shutdown <- subscribeEvent $ _vtyResult_shutdown vtyResult

    -- Fork a thread and continuously get the next vty input event, and then
    -- write the input event to our channel of FRP 'Event' triggers.
    -- The thread is forked here because 'nextEvent' blocks.
    nextEventThread <- liftIO $ forkIO $ forever $ do
      -- Retrieve the next input event.
      ne <- V.nextEvent vty
      let -- The reference to the vty input 'EventTrigger'. This is the trigger
          -- we'd like to associate the input event value with.
          triggerRef = EventTriggerRef vtyEventTriggerRef
          -- Create an event 'TriggerInvocation' with the value that we'd like
          -- the event to have if it is fired. It may not fire with this value
          -- if nobody is subscribed to the 'Event'.
          triggerInvocation = TriggerInvocation ne $ return ()
      -- Write our input event's 'EventTrigger' with the newly created
      -- 'TriggerInvocation' value to the queue of events.
      writeChan events [triggerRef :=> triggerInvocation]
      modifyMVar_ chanSizeVar (return . (+1))

    triggerEventThread <- liftIO $ forkIO $ forever $ do
      ev <- readChan triggerEvents
      writeChan events ev
      modifyMVar_ chanSizeVar (return . (+1))


    --numFramesVar :: MVar Int <- liftIO $ newMVar 0

    -- The main application loop. We wait for new events, fire those that
    -- have subscribers, and update the display. If we detect a shutdown
    -- request, the application terminates.
    fix $ \loop -> do
      -- Read the next event (blocking).
      ers <- liftIO $ readChan events
      liftIO $ modifyMVar_ chanSizeVar (return . (+ (-1)))
      stop <- do
        -- Fire events that have subscribers.
        fireEventTriggerRefs fc ers $
          -- Check if the shutdown 'Event' is firing.
          readEvent shutdown >>= \case
            Nothing -> return False
            Just _ -> return True

      -- potato debug logging
      {-
      liftIO $ do
        nFrames <- readMVar numFramesVar
        hPutStrLn stderr $ "frame: " <> show nFrames <> " ticks: " <> show (length stop)
        hFlush stderr
        modifyMVar_ numFramesVar (return . (+1))
      -}

      if or stop
        then liftIO $ do             -- If we received a shutdown 'Event'
          killThread nextEventThread -- then stop reading input events and
          killThread triggerEventThread
          V.shutdown vty             -- call the 'Graphics.Vty.Vty's shutdown command.

        else do                      -- Otherwise, update the display and loop.
          updateVty
          loop
  where
    -- TODO Some part of this is probably general enough to belong in reflex
    -- | Use the given 'FireCommand' to fire events that have subscribers
    -- and call the callback for the 'TriggerInvocation' of each.
    fireEventTriggerRefs
      :: (Monad (ReadPhase m), MonadIO m)
      => FireCommand t m
      -> [DSum (EventTriggerRef t) TriggerInvocation]
      -> ReadPhase m a
      -> m [a]
    fireEventTriggerRefs (FireCommand fire) ers rcb = do
      mes <- liftIO $
        forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          return $ fmap (\e -> e :=> Identity a) me
      a <- fire (catMaybes mes) rcb
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
      return a

-- | Run a 'VtyApp' with a 'Graphics.Vty.Vty' handle with a standard configuration.
runVtyApp
  :: (forall t m. VtyApp t m)
  -> IO ()
runVtyApp app = do
  vty <- getDefaultVty
  runVtyAppWithHandle vty app

-- | Returns the standard vty configuration with mouse mode enabled.
getDefaultVty :: IO V.Vty
getDefaultVty = do
  cfg <- V.standardIOConfig
  V.mkVty $ cfg { V.mouseMode = Just True }
