{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Potato.FlowSpec
  ( spec
  )
where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit   (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow.Vty.Main

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
import Reflex.Vty.Test.Common

data PotatoNetwork t (m :: Type -> Type)

instance (MonadVtyApp t (TestGuestT t m), TestGuestConstraints t m) => ReflexVtyTestApp (PotatoNetwork t m) t m where
  data VtyAppInputTriggerRefs (PotatoNetwork t m) = PotatoNetwork_InputTriggerRefs
  data VtyAppInputEvents (PotatoNetwork t m) = PotatoNetwork_InputEvents
  data VtyAppOutput (PotatoNetwork t m) =
    PotatoNetwork_Output {
        _potatoNetwork_Output_exitEv :: Event t ()
      }
  getApp _ = do
    exitEv <- mainPFWidget
    return PotatoNetwork_Output {
        _potatoNetwork_Output_exitEv = exitEv
      }

  makeInputs =
    -- return dummy inputs since they are both empty
    return (PotatoNetwork_InputEvents, PotatoNetwork_InputTriggerRefs)


test_basic :: Test
test_basic = TestLabel "open and quit" $ TestCase $ runSpiderHost $
  runReflexVtyTestApp @ (PotatoNetwork (SpiderTimeline Global) (SpiderHost Global)) (100,100) $ do

    -- get our app's output events and subscribe to them
    PotatoNetwork_Output {..} <- userOutputs
    exitH <- subscribeEvent _potatoNetwork_Output_exitEv

    let
      readExitEv = sequence =<< readEvent exitH

    -- fire an empty event and ensure there is no quit event
    fireQueuedEventsAndRead readExitEv >>= \a -> liftIO (checkNothing a)

    -- enter quit sequence and ensure there is a quit event
    queueVtyEvent $ V.EvKey (V.KChar 'q') [V.MCtrl]
    fireQueuedEventsAndRead readExitEv >>= \a -> liftIO (checkSingleMaybe a ())


spec :: Spec
spec = do
  fromHUnitTest test_basic
