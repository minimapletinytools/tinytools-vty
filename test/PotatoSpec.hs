{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module PotatoSpec
  ( spec
  )
where

import           Prelude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit   (fromHUnitTest)
import           Test.HUnit

import           Control.Monad.IO.Class     (liftIO)
import           Data.Kind
import qualified Data.List                  as L

import qualified Graphics.Vty               as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Vty
import           Reflex.Vty.Test.Monad.Host

data BasicPotatoTest1 t (m :: Type -> Type)

instance (MonadVtyApp t (TestGuestT t m), TestGuestConstraints t m) => ReflexVtyTestApp (BasicPotatoTest1 t m) t m where
  data VtyAppInputTriggerRefs (BasicPotatoTest1 t m) = BasicPotatoTest1_InputTriggerRefs
  data VtyAppInputEvents (BasicPotatoTest1 t m) = BasicPotatoTest1_InputEvents
  data VtyAppOutput (BasicPotatoTest1 t m) = BasicPotatoTest1_Output
  getApp _ = do
    return $ BasicPotatoTest1_Output
  makeInputs = do
    -- return dummy inputs since they are both empty
    return (BasicPotatoTest1_InputEvents, BasicPotatoTest1_InputTriggerRefs)

test_basic :: Test
test_basic = TestLabel "basic" $ TestCase $ runSpiderHost $
  runReflexVtyTestApp @ (BasicPotatoTest1 (SpiderTimeline Global) (SpiderHost Global)) (500,500) $ do
    -- get our app's output events and subscribe to them
    BasicPotatoTest1_Output <- userOutputs
    vtyImages <- vtyOutputs
    --vtyH <- subscribeEvent _basicNetworkTest1_Output_vtyEv
    --dwH <- subscribeEvent $ updated _basicNetworkTest1_Output_displayWidth


    -- fire an empty event and ensure there is no output
    -- also check that an image was rendered
    a1 :: [((), [V.Image])] <- fireQueuedEventsAndRead $ do
      --a <- sequence =<< readEvent vtyH
      b <- sample vtyImages
      return ((),b)
    --liftIO $ (fst . L.last $ a1) @?= Nothing
    --liftIO $ (length . snd . L.last $ a1) @?= 1

    -- fire escape event and ensure we can
    --let someEvent = V.EvKey V.KEsc []
    --queueVtyEvent someEvent
    --a2 :: [Maybe VtyEvent] <- fireQueuedEventsAndRead $ sequence =<< readEvent vtyH
    --liftIO $ a2 @?= [Just someEvent]

    -- resize the screen and check that the changes are reflected
    --queueVtyEvent $ V.EvResize 10 10
    --a3  :: [Maybe Int] <- fireQueuedEventsAndRead $ sequence =<< readEvent dwH
    --liftIO $ a3 @?= [Just 10]

    return ()

spec :: Spec
spec = do
  fromHUnitTest test_basic
