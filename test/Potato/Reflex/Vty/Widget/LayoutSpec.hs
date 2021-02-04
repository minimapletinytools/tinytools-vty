{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Potato.Reflex.Vty.Widget.LayoutSpec
  ( spec
  )
where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit   (fromHUnitTest)
import           Test.HUnit

import           Potato.Reflex.Vty.Widget.Layout


import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Ref
import           Data.Default
import           Data.Kind
import qualified Data.List                  as L

import qualified Graphics.Vty               as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Vty                       hiding (Constraint (..),
                                                   Orientation (..),
                                                   TileConfig (..), col, fixed,
                                                   row, runLayout, stretch,
                                                   tabNavigation, tile)
import           Reflex.Vty.Test.Monad.Host
import Reflex.Vty.Test.Common

data BasicNetworkTest1 t (m :: Type -> Type)

instance (MonadVtyApp t (TestGuestT t m), TestGuestConstraints t m) => ReflexVtyTestApp (BasicNetworkTest1 t m) t m where
  data VtyAppInputTriggerRefs (BasicNetworkTest1 t m) = BasicNetworkTest1_InputTriggerRefs
  data VtyAppInputEvents (BasicNetworkTest1 t m) = BasicNetworkTest1_InputEvents
  data VtyAppOutput (BasicNetworkTest1 t m) =
    BasicNetworkTest1_Output {
        _basicNetworkTest1_Output_focusDyn :: Dynamic t (Maybe Int)
        , _basicNetworkTest1_Output_layoutTreeDyn :: Dynamic t LayoutTree
      }
  getApp _ = do
    LayoutReturnData {..} <- beginLayoutL $ row $ fixedL 39 $ col $ do
      (a1,b1,c1) <- fixedL 3 $ row $ do
        a <- stretch $ textButtonStatic def "POTATO"
        b <- stretch $ textButtonStatic def "TOMATO"
        c <- stretch $ textButtonStatic def "EGGPLANT"
        return (a,b,c)
      (a2,b2,c2) <- fixedL 3 $ row $ do
        a <- stretch $ textButtonStatic def "CHEESE"
        b <- stretch $ textButtonStatic def "BEES"
        c <- stretch $ textButtonStatic def "MY KNEES"
        return (a,b,c)
      (a3,b3,c3) <- fixedL 3 $ row $ do
        a <- stretch $ textButtonStatic def "TIME"
        b <- stretch $ textButtonStatic def "RHYME"
        c <- stretch $ textButtonStatic def "A BIG CRIME"
        return (a,b,c)
      -- NOTE the box will most likely not render correctly once you put emoji's
      -- you need to initialize vty with an updated char width map to fix this
      fixed 7 $ boxTitle (constant def) "CLICK BUTTONS TO DRAW" $ do
        outputDyn <- foldDyn (<>) "" $ mergeWith (<>) [a1 $> "🥔", b1 $> "🍅", c1 $> "🍆", a2 $> "🧀", b2 $> "🐝🐝", c2 $> "💘", a3 $> "⏰", b3 $> "📜", c3 $> "💰🔪🔒"]
        text (current outputDyn)
    return BasicNetworkTest1_Output {
        _basicNetworkTest1_Output_focusDyn = _layoutReturnData_focus
        , _basicNetworkTest1_Output_layoutTreeDyn = _layoutReturnData_tree
      }

  makeInputs =
    -- return dummy inputs since they are both empty
    return (BasicNetworkTest1_InputEvents, BasicNetworkTest1_InputTriggerRefs)



test_basic :: Test
test_basic = TestLabel "basic" $ TestCase $ runSpiderHost $
  runReflexVtyTestApp @ (BasicNetworkTest1 (SpiderTimeline Global) (SpiderHost Global)) (100,100) $ do
    return ()
    -- get our app's output events and subscribe to them
    BasicNetworkTest1_Output {..} <- userOutputs
    --focusDynH <- subscribeDynamic _basicNetworkTest1_Output_focusDyn

    let
      --readFocusDyn = readDynamic focusDynH

      tempReadLayoutTree = sample . current $ _basicNetworkTest1_Output_layoutTreeDyn
{-

    -- fire an empty event and ensure there is no popup
    fireQueuedEventsAndRead readFocusDyn >>= \a -> liftIO (checkSingle a Nothing)

    -- tab a few times
    queueVtyEvent $ V.EvKey (V.KChar '\t') []
    fireQueuedEventsAndRead readFocusDyn >>= \a -> liftIO (checkSingle a (Just 0))
-}

    --resize the window
    queueVtyEvent $ V.EvResize 50 50

    fireQueuedEventsAndRead tempReadLayoutTree >>= \a -> liftIO (print a)
    fireQueuedEventsAndRead tempReadLayoutTree >>= \a -> liftIO (print a)

spec :: Spec
spec = do
  fromHUnitTest test_basic
