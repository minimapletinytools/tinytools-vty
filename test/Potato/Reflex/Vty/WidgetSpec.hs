{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}


module Potato.Reflex.Vty.WidgetSpec
  ( spec
  )
where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit       (fromHUnitTest)
import           Test.HUnit

import           Potato.Reflex.Vty.Widget

import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Ref
import           Data.Default
import           Data.Kind
import qualified Data.List                      as L

import qualified Graphics.Vty                   as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Vty
import           Reflex.Vty.Test.Monad.Host
import           Reflex.Vty.Test.Monad.Host.TH

import           Reflex.Vty.Test.Common


$(declareStuff "SingleClickNetwork"
  []
  [("singleClick", [t|Event $(tv) SingleClick|])]
  [|
      do
        singleClickEv <- singleClick V.BLeft
        return $ $(toutputcon "SingleClickNetwork") singleClickEv 
    |]
  )


test_singleClick_basic :: Test
test_singleClick_basic = TestLabel "test_singleClick_basic" $ TestCase $ runSpiderHost $
  runReflexVtyTestApp @(SingleClickNetwork (SpiderTimeline Global) (SpiderHost Global)) (100, 100) $ do


    -- get our app's output events and subscribe to them
    SingleClickNetwork_Output singleClickEv <- userOutputs
    singleClickEvH <-  subscribeEvent singleClickEv

    let
      readSingleClickEv = sequence =<< readEvent singleClickEvH


    -- fire an empty event and ensure there is no singleClick
    fireQueuedEventsAndRead readSingleClickEv >>= \a -> liftIO (checkNothing a)

    let 
      expectedSingleClick = SingleClick  { 
          _singleClick_button      = V.BLeft
          , _singleClick_coordinates = (0, 0)
          , _singleClick_modifiers   = []
          , _singleClick_didDragOff  = False
        }

    -- fire a click event and ensure there is a singleClick
    queueVtyEvent (V.EvMouseDown 0 0 V.BLeft [])
    fireQueuedEventsAndRead readSingleClickEv >>= \a -> liftIO (checkNothing a)
    queueVtyEvent (V.EvMouseUp 0 0 Nothing)
    fireQueuedEventsAndRead readSingleClickEv >>= \a -> liftIO (checkSingleMaybe a expectedSingleClick)

    -- do it again
    queueVtyEvent (V.EvMouseDown 0 0 V.BLeft [])
    fireQueuedEventsAndRead readSingleClickEv >>= \a -> liftIO (checkNothing a)
    queueVtyEvent (V.EvMouseUp 0 0 Nothing)
    fireQueuedEventsAndRead readSingleClickEv >>= \a -> liftIO (checkSingleMaybe a expectedSingleClick)
    



$(declareStuff "DoubleClickNetwork"
  []
  [("doubleClick", [t|Event $(tv) ()|])]
  [|
      do
        doubleClickEv <- doubleClick $ DoubleClickConfig {
            _doubleClickConfig_timeTolerance = 10000
            , _dobuleClickConfig_button      = V.BLeft
          }
        return $ $(toutputcon "DoubleClickNetwork") doubleClickEv 
    |]
  )


test_doubleClick_basic :: Test
test_doubleClick_basic = TestLabel "test_doubleClick_basic" $ TestCase $ runSpiderHost $
  runReflexVtyTestApp @(DoubleClickNetwork (SpiderTimeline Global) (SpiderHost Global)) (100, 100) $ do


    -- get our app's output events and subscribe to them
    DoubleClickNetwork_Output doubleClickEv <- userOutputs
    doubleClickEvH <-  subscribeEvent doubleClickEv

    let
      readDoubleClickEv = sequence =<< readEvent doubleClickEvH


    -- fire an empty event and ensure there is no doubleClick
    fireQueuedEventsAndRead readDoubleClickEv >>= \a -> liftIO (checkNothing a)

    -- fire a click event and ensure there is no doubleClick
    queueVtyEvent (V.EvMouseDown 0 0 V.BLeft []) >> fireQueuedEvents
    queueVtyEvent (V.EvMouseUp 0 0 Nothing)
    fireQueuedEventsAndRead readDoubleClickEv >>= \a -> liftIO (checkNothing a)

    -- fire a click event and ensure there is a doubleClick
    queueVtyEvent (V.EvMouseDown 0 0 V.BLeft []) >> fireQueuedEvents
    queueVtyEvent (V.EvMouseUp 0 0 Nothing)
    -- TODO not possible to test that this because we need to wait for the IO operation tracking time to trigger before we get back the click event.... LOL (so this UT is a little pointless now)
    --fireQueuedEventsAndRead readDoubleClickEv >>= \a -> liftIO (checkSingleMaybe a ())






spec :: Spec
spec = do
  fromHUnitTest test_singleClick_basic
  fromHUnitTest test_doubleClick_basic
