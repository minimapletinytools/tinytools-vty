{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell          #-}

module Potato.Flow.ParamsSpec
  ( spec
  , PotatoNetwork(..)
  )
where

import           Relude hiding (Type)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit   (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow.Vty.Main
import Potato.Flow

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Ref
import           Data.Default
import qualified           Data.Kind
import qualified Data.List                  as L

import qualified Graphics.Vty               as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Vty
import           Reflex.Vty.Test.Monad.Host
import           Reflex.Vty.Test.Monad.Host.TH
import Reflex.Vty.Test.Common

import Language.Haskell.TH

-- $(declareNetworkData (mkName "PotatoNetwork"))
data PotatoNetwork t (m :: Data.Kind.Type -> Data.Kind.Type)

$(declareNetworkInstance (mkName "PotatoNetwork")
  [("bypassEvent", [t|WSEvent|])
    , ("moop", [t|Int|])]
  [("exitEv", [t|Event $(tv) ()|])]
  [|
      --getApp inputEvs = do
      do
        exitEv <- mainPFWidget $ MainPFWidgetConfig {
            _mainPFWidgetConfig_initialFile = Nothing
            , _mainPFWidgetConfig_initialState = emptyPFState
            , _mainPFWidgetConfig_bypassEvent = $(varE $ mkName "_potatoNetwork_InputEvents_bypassEvent") $(varE $ mkName "inputEvs")
          }
        return ($(conE $ mkName "PotatoNetwork_Output") exitEv)
        --return ($(ConT $ mkName "PotatoNetwork_Output") { $(VarT $ mkName "_potatoNetwork_Output_exitEv") = exitEv })
    |]
  )
-- $(declareOutputs "_potatoNetwork_" [("exitEv", [|Event t ()|])])
{-
instance (MonadVtyApp t (TestGuestT t m), TestGuestConstraints t m) => ReflexVtyTestApp (PotatoNetwork t m) t m where
  data VtyAppInputTriggerRefs (PotatoNetwork t m) = PotatoNetwork_InputTriggerRefs {
      -- force an event bypassing the normal interface
      _potatoNetwork_InputTriggerRefs_bypassEvent :: Ref m (Maybe (EventTrigger t WSEvent))
    }
  data VtyAppInputEvents (PotatoNetwork t m) = PotatoNetwork_InputEvents {
      _potatoNetwork_InputEvents_InputEvents_bypassEvent :: Event t WSEvent
    }

  data VtyAppOutput (PotatoNetwork t m) =
    PotatoNetwork_Output {
        _potatoNetwork_Output_exitEv :: Event t ()
      }

  getApp PotatoNetwork_InputEvents {..} = do
    exitEv <- mainPFWidget $ MainPFWidgetConfig {
        _mainPFWidgetConfig_initialFile = Nothing
        , _mainPFWidgetConfig_initialState = emptyPFState
        , _mainPFWidgetConfig_bypassEvent = _potatoNetwork_InputEvents_bypassEvent
      }
    return PotatoNetwork_Output {
        _potatoNetwork_Output_exitEv = exitEv
      }

  makeInputs = do
    (ev, ref) <- newEventWithTriggerRef
    return (PotatoNetwork_InputEvents ev, PotatoNetwork_InputTriggerRefs ref)

-}

test_basic :: Test
test_basic = TestLabel "open and quit" $ TestCase $ runSpiderHost $
  runReflexVtyTestApp @ (PotatoNetwork (SpiderTimeline Global) (SpiderHost Global)) (100,100) $ do

    -- get our app's input triggers
    PotatoNetwork_InputTriggerRefs {..} <- userInputTriggerRefs

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
  --fromHUnitTest $ TestLabel "Reifying..." $ TestCase $ liftIO $ putStrLn $(stringE . show =<< reifyInstances ''ReflexVtyTestApp [VarT $ mkName "a"])
  fromHUnitTest test_basic
