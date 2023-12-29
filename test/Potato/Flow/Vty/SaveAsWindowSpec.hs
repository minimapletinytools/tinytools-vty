

{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Potato.Flow.Vty.SaveAsWindowSpec
  ( spec
  )
where

import           Relude                        hiding (Type)

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit      (fromHUnitTest)
import           Test.HUnit

import           Potato.Flow
import Potato.Flow.Vty.SaveAsWindow
import           Potato.Flow.Vty.PotatoReader

import           Data.Default
import qualified System.FilePath as FP

import qualified Graphics.Vty                  as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Vty
import           Reflex.Vty.Test.Common
import           Reflex.Vty.Test.Monad.Host
import           Reflex.Vty.Test.Monad.Host.TH


width :: Int
width = 100
height :: Int
height = 100


$(declareStuff "PopupSaveAsWindowNetwork"
  [("saveAsEv", [t|FP.FilePath|])]
  [("popupSaveAsWindowWidget", [t|(Event  $(tv) FP.FilePath, Dynamic  $(tv) Bool)|])]
  [|
      do
        popupSaveAsWindowWidget <- flip runPotatoReader def $ popupSaveAsWindow $ SaveAsWindowConfig {
            _saveAsWindowConfig_saveAs = $(tinput "PopupSaveAsWindowNetwork" "saveAsEv")
          }
        return $ $(toutputcon "PopupSaveAsWindowNetwork") popupSaveAsWindowWidget
    |]
  )



test_popupSaveAsWindow_basic :: Test
test_popupSaveAsWindow_basic = TestLabel "test_popupSaveAsWindow_basic" $ TestCase $ runSpiderHost $
  runReflexVtyTestApp @(PopupSaveAsWindowNetwork (SpiderTimeline Global) (SpiderHost Global)) (width, height) $ do

    -- get our app's input triggers
    PopupSaveAsWindowNetwork_InputTriggerRefs {..} <- userInputTriggerRefs

    -- get our app's output events and subscribe to them
    PopupSaveAsWindowNetwork_Output (saveToFileEv, stateDyn) <- userOutputs
    stateDynH <- subscribeEvent $ updated stateDyn
    saveToFileEvH <- subscribeEvent saveToFileEv

    let
      readState = sample . current $ stateDyn
      readStateEv = sequence =<< readEvent stateDynH
      readSaveToFileEv = sequence =<< readEvent saveToFileEvH

    -- fire an empty event and ensure there is no popup
    fireQueuedEventsAndRead readStateEv >>= \a -> liftIO (checkNothing a)
    fireQueuedEventsAndRead readState >>= \a -> liftIO (checkSingle a False)

    -- fire an empty event and ensure there is no output
    fireQueuedEventsAndRead readSaveToFileEv >>= \a -> liftIO (checkNothing a)

    -- save to file and ensure there is a popup
    queueEventTriggerRef _popupSaveAsWindowNetwork_InputTriggerRefs_saveAsEv ("meowmeowmeow.tt")
    fireQueuedEventsAndRead readStateEv >>= \a -> liftIO (checkSingleMaybe a True)
    fireQueuedEventsAndRead readState >>= \a -> liftIO (checkSingle a True)

    -- press the save button and ensure that there is no saveToFileEv (because there is no filename) and the dialog remains open
    -- NOTE save button position relative to bottom right corner
    queueVtyEvent (V.EvMouseDown (width-2) (height-2) V.BLeft []) >> fireQueuedEvents
    queueVtyEvent (V.EvMouseUp (width-2) (height-2) Nothing)
    fireQueuedEventsAndRead readSaveToFileEv >>= \a -> liftIO (checkNothing a)
    fireQueuedEventsAndRead readState >>= \a -> liftIO (checkSingle a True)

    -- add text to filename and press save button and ensure there is a saveToFileEv and the dialog closes
    -- NOTE filename entry position relative to upper left corner
    queueVtyEvent (V.EvMouseDown 20 1 V.BLeft []) >> fireQueuedEvents
    queueVtyEvent (V.EvMouseUp 20 1 Nothing) >> fireQueuedEvents
    queueVtyEvent (V.EvKey (V.KChar 'm') []) >> fireQueuedEvents
    queueVtyEvent (V.EvKey (V.KChar 'e') []) >> fireQueuedEvents
    queueVtyEvent (V.EvKey (V.KChar 'o') []) >> fireQueuedEvents
    queueVtyEvent (V.EvKey (V.KChar 'w') []) >> fireQueuedEvents
    queueVtyEvent (V.EvMouseDown (width-2) (height-2) V.BLeft []) >> fireQueuedEvents
    queueVtyEvent (V.EvMouseUp (width-2) (height-2) Nothing)
    -- TODO figure out why this fails
    --fireQueuedEventsAndRead readSaveToFileEv >>= \a -> liftIO (checkSingleMaybe a "meow.tt")
    --fireQueuedEventsAndRead readState >>= \a -> liftIO (checkSingle a False)




$(declareStuff "PopupSaveBeforeExitNetwork"
  [
    ("openEv", [t|()|])
    , ("newEv", [t|()|])
    , ("exitEv", [t|()|])
    , ("saveOutcomeEv", [t|Either Text FilePath|])]
  [
    ("popupSaveBeforeExitWidget", [t|SaveBeforeActionOutput  $(tv)|])
    , ("popupState", [t|Dynamic $(tv) Bool|])]
  [|
      do
        (popupSaveBeforeExitWidget, stateDyn) <- flip runPotatoReader def $ popupSaveBeforeExit $ SaveBeforeActionConfig {
            _saveBeforeActionConfig_unsavedChangesBeh = constant True
            , _saveBeforeActionConfig_open = $(tinput "PopupSaveBeforeExitNetwork" "openEv")
            , _saveBeforeActionConfig_new = $(tinput "PopupSaveBeforeExitNetwork" "newEv")
            , _saveBeforeActionConfig_exit = $(tinput "PopupSaveBeforeExitNetwork" "exitEv")
            , _saveBeforeActionConfig_saveOutcomeEv = $(tinput "PopupSaveBeforeExitNetwork" "saveOutcomeEv")
          }
        return $ $(toutputcon "PopupSaveBeforeExitNetwork") popupSaveBeforeExitWidget stateDyn
    |]
  )


test_popupSaveBeforeExit_basic :: Test
test_popupSaveBeforeExit_basic = TestLabel "test_popupSaveBeforeExit_basic" $ TestCase $ runSpiderHost $
  runReflexVtyTestApp @(PopupSaveBeforeExitNetwork (SpiderTimeline Global) (SpiderHost Global)) (width, height) $ do

    -- get our app's input triggers
    PopupSaveBeforeExitNetwork_InputTriggerRefs {..} <- userInputTriggerRefs

    -- get our app's output events and subscribe to them
    PopupSaveBeforeExitNetwork_Output SaveBeforeActionOutput {..} stateDyn <- userOutputs
    stateDynH <- subscribeEvent $ updated stateDyn
    saveEvH <- subscribeEvent _saveBeforeActionOutput_save
    saveAsEvH  <- subscribeEvent _saveBeforeActionOutput_saveAs
    newEvH <-  subscribeEvent _saveBeforeActionOutput_new
    openEvH <-  subscribeEvent _saveBeforeActionOutput_open
    exitEvH <-  subscribeEvent _saveBeforeActionOutput_exit

    let
      readState = sample . current $ stateDyn
      readStateEv = sequence =<< readEvent stateDynH
      --readSaveEv = sequence =<< readEvent saveEvH
      --readSaveAsEv = sequence =<< readEvent saveAsEvH
      readNewEv = sequence =<< readEvent newEvH
      readOpenEv = sequence =<< readEvent openEvH
      readExitEv = sequence =<< readEvent exitEvH


    -- fire an empty event and ensure there is no popup
    fireQueuedEventsAndRead readStateEv >>= \a -> liftIO (checkNothing a)
    fireQueuedEventsAndRead readState >>= \a -> liftIO (checkSingle a False)

    -- fire a new event and ensure there is no new event and there is a popup
    queueEventTriggerRef _popupSaveBeforeExitNetwork_InputTriggerRefs_newEv ()
    fireQueuedEventsAndRead readNewEv >>= \a -> liftIO (checkNothing a)
    fireQueuedEventsAndRead readState >>= \a -> liftIO (checkSingle a True)

    -- cancel and ensure there is no new event but the dialog is gone
    queueVtyEvent (V.EvKey V.KEsc [])
    fireQueuedEventsAndRead readNewEv >>= \a -> liftIO (checkNothing a)
    fireQueuedEventsAndRead readState >>= \a -> liftIO (checkSingle a False)

    -- repeat for open
    queueEventTriggerRef _popupSaveBeforeExitNetwork_InputTriggerRefs_openEv ()
    fireQueuedEventsAndRead readOpenEv >>= \a -> liftIO (checkNothing a)
    fireQueuedEventsAndRead readState >>= \a -> liftIO (checkSingle a True)
    queueVtyEvent (V.EvKey V.KEsc [])
    fireQueuedEventsAndRead readOpenEv >>= \a -> liftIO (checkNothing a)
    fireQueuedEventsAndRead readState >>= \a -> liftIO (checkSingle a False)

    -- repeat for exit
    queueEventTriggerRef _popupSaveBeforeExitNetwork_InputTriggerRefs_exitEv ()
    fireQueuedEventsAndRead readExitEv >>= \a -> liftIO (checkNothing a)
    fireQueuedEventsAndRead readState >>= \a -> liftIO (checkSingle a True)
    queueVtyEvent (V.EvKey V.KEsc [])
    fireQueuedEventsAndRead readExitEv >>= \a -> liftIO (checkNothing a)
    fireQueuedEventsAndRead readState >>= \a -> liftIO (checkSingle a False)

    -- TODO press the save button and check for save event





spec :: Spec
spec = do
  fromHUnitTest test_popupSaveAsWindow_basic
  fromHUnitTest test_popupSaveBeforeExit_basic
