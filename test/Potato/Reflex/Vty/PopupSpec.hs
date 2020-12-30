{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Potato.Reflex.Vty.PopupSpec
  ( spec
  )
where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit   (fromHUnitTest)
import           Test.HUnit

import           Potato.Reflex.Vty.Popup

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Ref
import           Data.Default
import           Data.Kind
import qualified Data.List                  as L

import qualified Graphics.Vty               as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Vty
import           Reflex.Vty.Test.Monad.Host

data BasicNetworkTest1 t (m :: Type -> Type)

instance (MonadVtyApp t (TestGuestT t m), TestGuestConstraints t m) => ReflexVtyTestApp (BasicNetworkTest1 t m) t m where

  -- I just wanted to try using VtyAppInputEvents/VtyAppInputTriggerRefs
  -- it would have been a lot easier to use a vty event to trigger the popup
  data VtyAppInputTriggerRefs (BasicNetworkTest1 t m) = BasicNetworkTest1_InputTriggerRefs {
      _basicNetworkTest1_InputTriggerRefs_makePopup :: Ref m (Maybe (EventTrigger t ()))
    }
  data VtyAppInputEvents (BasicNetworkTest1 t m) = BasicNetworkTest1_InputEvents {
      _basicNetworkTest1_InputEvents_makePopup :: Event t ()
    }
  data VtyAppOutput (BasicNetworkTest1 t m) =
    BasicNetworkTest1_Output {
        _basicNetworkTest1_Output_cancelEv :: Event t ()
        , _basicNetworkTest1_Output_popupOutEv :: Event t Int
        , _basicNetworkTest1_Output_popupStateDyn :: Dynamic t Bool
      }
  getApp BasicNetworkTest1_InputEvents {..} = do
    let
      someWidget = fmap (const 123) <$> key V.KEnter
      someWidgetEv = fmap (const someWidget) _basicNetworkTest1_InputEvents_makePopup
    -- popup that closes when you press enter
    (popupEv, popupStateDyn) <- popupPaneSimple def someWidgetEv
    -- gotta make the popup look pretty :D
    fill '#'
    return $ BasicNetworkTest1_Output never popupEv popupStateDyn
  makeInputs = do
    (ev, ref) <- newEventWithTriggerRef
    return (BasicNetworkTest1_InputEvents ev, BasicNetworkTest1_InputTriggerRefs ref)

checkSingle :: (HasCallStack, Eq a, Show a) => [a] -> a -> Assertion
checkSingle values a = case nonEmpty values of
  Nothing -> assertFailure "empty list"
  Just x  -> a @=? head x

checkSingleMaybe :: (HasCallStack, Eq a, Show a) => [Maybe a] -> a -> Assertion
checkSingleMaybe values a = case nonEmpty values of
  Nothing -> assertFailure "empty list"
  Just x  -> Just a @=? head x

test_basic :: Test
test_basic = TestLabel "basic" $ TestCase $ runSpiderHost $
  runReflexVtyTestApp @ (BasicNetworkTest1 (SpiderTimeline Global) (SpiderHost Global)) (100,100) $ do

    -- get our app's input triggers
    BasicNetworkTest1_InputTriggerRefs {..} <- userInputTriggerRefs

    -- get our app's output events and subscribe to them
    BasicNetworkTest1_Output {..} <- userOutputs
    popupOutH <- subscribeEvent _basicNetworkTest1_Output_popupOutEv
    popupStateH <- subscribeEvent $ updated _basicNetworkTest1_Output_popupStateDyn

    let
      readPopupState = sample . current $ _basicNetworkTest1_Output_popupStateDyn
      readPopupStateEv = sequence =<< readEvent popupStateH

    -- fire an empty event and ensure there is no popup
    fireQueuedEventsAndRead readPopupState >>= \a -> liftIO (checkSingle a False)

    -- enable the popup
    queueEventTriggerRef _basicNetworkTest1_InputTriggerRefs_makePopup ()
    fireQueuedEventsAndRead readPopupStateEv >>= \a -> liftIO (checkSingleMaybe a True)

    -- fire an empty event and ensure popup is still there
    fireQueuedEventsAndRead readPopupState >>= \a -> liftIO (checkSingle a True)

    -- close the popup
    queueVtyEvent $ V.EvKey V.KEnter []
    fireQueuedEventsAndRead readPopupStateEv >>= \a -> liftIO (checkSingleMaybe a False)

    -- enable the popup
    queueEventTriggerRef _basicNetworkTest1_InputTriggerRefs_makePopup ()
    fireQueuedEventsAndRead readPopupStateEv >>= \a -> liftIO (checkSingleMaybe a True)

    -- click within the popup and ensure it's still there
    queueVtyEvent $ V.EvMouseDown 50 50 V.BLeft []
    fireQueuedEventsAndRead readPopupState >>= \a -> liftIO (checkSingle a True)

    -- drag off and ensure popup is still there
    queueVtyEvent $ V.EvMouseDown 100 100 V.BLeft []
    fireQueuedEventsAndRead readPopupState >>= \a -> liftIO (checkSingle a True)

    -- release the mouse
    queueVtyEvent $ V.EvMouseUp 100 100 Nothing
    fireQueuedEventsAndRead readPopupState >>= \a -> liftIO (checkSingle a True)

    -- click off the popup and check that it got cancelled
    queueVtyEvent $ V.EvMouseDown 100 100 V.BLeft []
    fireQueuedEventsAndRead readPopupStateEv >>= \a -> liftIO (checkSingleMaybe a False)

    -- enable the popup
    queueEventTriggerRef _basicNetworkTest1_InputTriggerRefs_makePopup ()
    fireQueuedEventsAndRead readPopupStateEv >>= \a -> liftIO (checkSingleMaybe a True)

    -- re-enable the popup
    queueEventTriggerRef _basicNetworkTest1_InputTriggerRefs_makePopup ()
    fireQueuedEventsAndRead readPopupStateEv >>= \a -> liftIO (checkSingleMaybe a True)

    -- escape cancel the popup
    queueVtyEvent $ V.EvKey V.KEsc []
    fireQueuedEventsAndRead readPopupStateEv >>= \a -> liftIO (checkSingleMaybe a False)




spec :: Spec
spec = do
  fromHUnitTest test_basic
