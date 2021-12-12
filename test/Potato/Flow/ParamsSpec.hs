{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell          #-}

module Potato.Flow.ParamsSpec
  ( spec
  )
where

import           Relude hiding (Type)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit   (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow.Vty.Params
import Potato.Flow
import Potato.Flow.Vty.PotatoReader

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Ref
import           Data.Default
import qualified           Data.Kind
import qualified Data.List                  as L
import qualified Data.Sequence as Seq
import Data.Tuple.Extra (thd3)

import qualified Graphics.Vty               as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Vty
import           Reflex.Vty.Test.Monad.Host
import           Reflex.Vty.Test.Monad.Host.TH
import Reflex.Vty.Test.Common

import Language.Haskell.TH

$(declareStuff "ParamsNetwork"
  [("setSelection", [t|Selection|])
    , ("setCanvas", [t|SCanvas|])]
  [("paramsWidget", [t|ParamsWidget $(tv)|])]
  [|
      do
        setSelectionDyn <- holdDyn isParliament_empty $(tinput "ParamsNetwork" "setSelection")
        setCanvasDyn <- holdDyn (SCanvas (LBox (V2 0 0) (V2 100 100))) $(tinput "ParamsNetwork" "setCanvas")
        paramsWidget <- flip runPotatoReader def $ holdParamsWidget $ ParamsWidgetConfig {
            _paramsWidgetConfig_selectionDyn = setSelectionDyn
            , _paramsWidgetConfig_canvasDyn = setCanvasDyn
            , _paramsWidgetConfig_defaultParamsDyn = constDyn def
            , _paramsWidgetConfig_toolDyn = constDyn Tool_Select
          }
        return $ $(toutputcon "ParamsNetwork") paramsWidget
    |]
  )


test_params_set_canvas_size :: Test
test_params_set_canvas_size = TestLabel "set canvas size" $ TestCase $ runSpiderHost $
  runReflexVtyTestApp @ (ParamsNetwork (SpiderTimeline Global) (SpiderHost Global)) (100,100) $ do

    -- get our app's input triggers
    ParamsNetwork_InputTriggerRefs {..} <- userInputTriggerRefs

    -- get our app's output events and subscribe to them
    ParamsNetwork_Output (ParamsWidget {..}) <- userOutputs
    canvasSizeH <- subscribeEvent _paramsWidget_canvasSizeEvent

    let
      readCanvasSize = sequence =<< readEvent canvasSizeH

    -- fire an empty event and ensure there is no canvas change event
    fireQueuedEventsAndRead readCanvasSize >>= \a -> liftIO (checkNothing a)

    -- set the canvas size and ensure there is no canvas change event
    queueEventTriggerRef _paramsNetwork_InputTriggerRefs_setCanvas (SCanvas (LBox (V2 0 0) (V2 50 50)))
    fireQueuedEventsAndRead readCanvasSize >>= \a -> liftIO (checkNothing a)

    -- we have nothing selected so canvas size should be first thing in ParamsWidget
    queueVtyEvent (V.EvMouseDown 10 0 V.BLeft []) >> fireQueuedEvents
    queueVtyEvent (V.EvMouseUp 10 0 Nothing) >> fireQueuedEvents
    queueVtyEvent (V.EvKey V.KBS []) >> fireQueuedEvents
    queueVtyEvent (V.EvKey V.KBS []) >> fireQueuedEvents
    queueVtyEvent (V.EvKey (V.KChar '2') []) >> fireQueuedEvents
    queueVtyEvent (V.EvKey (V.KChar '0') []) >> fireQueuedEvents
    queueVtyEvent (V.EvKey V.KEnter [])
    fireQueuedEventsAndRead readCanvasSize >>= \a -> liftIO (checkSingleMaybe a (V2 (-30) 0))
    queueVtyEvent (V.EvKey V.KBS []) >> fireQueuedEvents
    queueVtyEvent (V.EvKey V.KBS []) >> fireQueuedEvents
    queueVtyEvent (V.EvKey (V.KChar '3') []) >> fireQueuedEvents
    queueVtyEvent (V.EvKey (V.KChar '0') []) >> fireQueuedEvents
    queueVtyEvent (V.EvKey V.KEnter [])
    fireQueuedEventsAndRead readCanvasSize >>= \a -> liftIO (checkSingleMaybe a (V2 (-30) (-20)))

    -- TODO test other stuff 😥





$(declareStuff "SuperStyleWidgetNetwork"
  [("setSelection", [t|Selection|])]
  [("height", [t|Dynamic $(tv) Int|])
  , ("capture", [t|Event $(tv) ()|])
  , ("output", [t|Event $(tv) (Either ControllersWithId SetPotatoDefaultParameters)|])]
  [|
      initManager_ $ col $ do
        selectionDyn <- holdDyn isParliament_empty $(tinput "SuperStyleWidgetNetwork" "setSelection")

        let
          selectFn s = case selectParamsFromSelection (getSEltLabelSuperStyle . superOwl_toSEltLabel_hack) s of
            Nothing -> (isParliament_empty, Nothing, Tool_Select)
            Just (a,b) -> (a,b,Tool_Select)
          mSuperStyleInputDyn = fmap selectFn selectionDyn
        (heightDyn, captureEv, outputEv) <- flip runPotatoReader def $ networkParamsWidgetOutputDynForTesting (holdSuperStyleWidget (constDyn def) mSuperStyleInputDyn)
        -- TODO consider convert outputEv back to Event t SuperStyle...
        return $ $(toutputcon "SuperStyleWidgetNetwork") heightDyn captureEv outputEv
    |]
  )

test_superStyleWidget_basic :: Test
test_superStyleWidget_basic = TestLabel "set canvas size" $ TestCase $ runSpiderHost $
  runReflexVtyTestApp @ (SuperStyleWidgetNetwork (SpiderTimeline Global) (SpiderHost Global)) (100,100) $ do

    let queueVtyEventAndFire x = queueVtyEvent x >> fireQueuedEvents

    -- get our app's input triggers
    SuperStyleWidgetNetwork_InputTriggerRefs {..} <- userInputTriggerRefs

    -- get our app's output events and subscribe to them
    SuperStyleWidgetNetwork_Output heightDyn captureEv outputEv <- userOutputs
    heightDynH <- subscribeDynamic heightDyn
    captureEvH <- subscribeEvent captureEv
    outputEvH <- subscribeEvent outputEv

    let
      --readHeightDyn = readDynamic heightDynH
      readCaptureEv = sequence =<< readEvent captureEvH
      readOutputEv = sequence =<< readEvent outputEvH

    -- fire an empty event and ensure there is no output or capture ev
    fireQueuedEventsAndRead readCaptureEv >>= \a -> liftIO (checkNothing a)
    fireQueuedEventsAndRead readOutputEv >>= \a -> liftIO (checkNothing a)

    -- set the canvas size and ensure there is no canvas change event
    --queueEventTriggerRef _paramsNetwork_InputTriggerRefs_setCanvas (SCanvas (LBox (V2 0 0) (V2 50 50)))
    --fireQueuedEventsAndRead readCanvasSize >>= \a -> liftIO (checkNothing a)


    -- TODO test other stuff 😥


spec :: Spec
spec = do
  fromHUnitTest test_params_set_canvas_size
  fromHUnitTest test_superStyleWidget_basic
