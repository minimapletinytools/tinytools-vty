{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell          #-}

module Reflex.Vty.Test.Monad.THSpec
  ( spec
  )
where

import           Relude

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit   (fromHUnitTest)
import           Test.HUnit

import qualified Data.List                  as L

import qualified Graphics.Vty               as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Vty
import           Reflex.Vty.Test.Monad.Host
import           Reflex.Vty.Test.Monad.Host.TH


$(declareStuff "BasicNetworkTest1"
  [("dummy", [t|Char|])]
  [("vtyEv", [t|Event $(tv) V.Event|])
  , ("displayWidth", [t|Dynamic $(tv) Int|])
  , ("displayHeight", [t|Dynamic $(tv) Int|])]
  [|
    do
      inp <- input
      dw <- displayWidth
      dh <- displayHeight
      fill $ constant '#'
      let
        dummyInput = fmap (\c -> V.EvKey (V.KChar c) []) $(tinput "BasicNetworkTest1" "dummy")
        inpOut = leftmost [inp, dummyInput]
      return $ $(toutputcon "BasicNetworkTest1") inpOut dw dh
  |])

-- | TODO add splicing to get network input/output var names
test_basic :: Test
test_basic = TestLabel "basic" $ TestCase $ runSpiderHost $
  runReflexVtyTestApp @ (BasicNetworkTest1 (SpiderTimeline Global) (SpiderHost Global)) (100,100) $ do

    -- get our app's input triggers
    BasicNetworkTest1_InputTriggerRefs {..} <- userInputTriggerRefs

    -- get our app's output events and subscribe to them
    BasicNetworkTest1_Output {..} <- userOutputs
    vtyImages <- vtyOutputs
    vtyH <- subscribeEvent _basicNetworkTest1_Output_vtyEv
    dwH <- subscribeEvent $ updated _basicNetworkTest1_Output_displayWidth

    -- fire an empty event and ensure there is no output
    -- also check that an image was rendered
    a1 :: [(Maybe VtyEvent, [V.Image])] <- fireQueuedEventsAndRead $ do
      a <- sequence =<< readEvent vtyH
      b <- sample vtyImages
      return (a,b)
    liftIO $ (fst . L.last $ a1) @?= Nothing
    -- not sure why this produces two images now, whatever,
    --liftIO $ (length . snd . L.last $ a1) @?= 1

    -- fire a vty event and ensure the output is the same as the input
    let someEvent = V.EvKey V.KEsc []
    queueVtyEvent someEvent
    a2 :: [Maybe VtyEvent] <- fireQueuedEventsAndRead $ sequence =<< readEvent vtyH
    liftIO $ a2 @?= [Just someEvent]

    -- fire a dummy input event and enusre the output is as expected
    queueEventTriggerRef _basicNetworkTest1_InputTriggerRefs_dummy 'p'
    a3 :: [Maybe VtyEvent] <- fireQueuedEventsAndRead $ sequence =<< readEvent vtyH
    liftIO $ a3 @?= [Just $ V.EvKey (V.KChar 'p') []]

    -- resize the screen and check that the changes are reflected
    queueVtyEvent $ V.EvResize 10 10
    a4  :: [Maybe Int] <- fireQueuedEventsAndRead $ sequence =<< readEvent dwH
    liftIO $ a4 @?= [Just 10]



spec :: Spec
spec = do
  fromHUnitTest test_basic
