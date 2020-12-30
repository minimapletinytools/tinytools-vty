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
import           Data.Kind
import qualified Data.List                  as L

import qualified Graphics.Vty               as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Vty
import           Reflex.Vty.Test.Monad.Host

data BasicNetworkTest1 t (m :: Type -> Type)

instance (MonadVtyApp t (TestGuestT t m), TestGuestConstraints t m) => ReflexVtyTestApp (BasicNetworkTest1 t m) t m where
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
    (popupEv, popupStateDyn) <- popupOverrideWidget 10 10 someWidgetEv
    -- gotta make the popup look pretty :D
    fill '#'
    return $ BasicNetworkTest1_Output never popupEv popupStateDyn
  makeInputs = do
    (ev, ref) <- newEventWithTriggerRef
    return (BasicNetworkTest1_InputEvents ev, BasicNetworkTest1_InputTriggerRefs ref)

checkSingle :: (HasCallStack, Eq a, Show a) => [a] -> a -> Assertion
checkSingle values a = case nonEmpty values of
  Nothing -> assertFailure "empty list"
  Just x  -> head x @=? a

checkSingleMaybe :: (HasCallStack, Eq a, Show a) => [Maybe a] -> a -> Assertion
checkSingleMaybe values a = case nonEmpty values of
  Nothing -> assertFailure "empty list"
  Just x  -> head x @=? Just a

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
    a1 :: [Bool] <- fireQueuedEventsAndRead readPopupState
    liftIO $ checkSingle a1 False

    -- enable the popup
    queueEventTriggerRef _basicNetworkTest1_InputTriggerRefs_makePopup ()
    a2 :: [(Maybe Bool)] <- fireQueuedEventsAndRead readPopupStateEv
    liftIO $ checkSingleMaybe a2 True

    -- fire an empty event and ensure popup is still there
    a1 :: [Bool] <- fireQueuedEventsAndRead readPopupState
    liftIO $ checkSingle a1 True

    -- close the popup
    --queueVtyEvent $ V.EvKey V.KEnter []
    --a3 :: [(Maybe Bool)] <- fireQueuedEventsAndRead readPopupStateEv
    --liftIO $ checkSingleMaybe a3 False

spec :: Spec
spec = do
  fromHUnitTest test_basic
