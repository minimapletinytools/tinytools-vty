{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Potato.Reflex.Vty.Widget.LayoutSpec
  ( spec
  )
where

import           Relude
import qualified Relude.Unsafe as Unsafe

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit   (fromHUnitTest)
import           Test.HUnit

import           Potato.Reflex.Vty.Widget.Layout


import Control.Monad.Fix
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Ref
import           Data.Default
import           Data.Kind
import qualified Data.List                  as L
import qualified Data.Tree as Tree
import Data.Maybe (fromJust)

import qualified Graphics.Vty               as V
import           Reflex
import           Reflex.Host.Class
import           Reflex.Vty                        hiding (Constraint (..),
                                                    Orientation (..), col,
                                                    fixed, row, stretch, tile, TileConfig(..), Layout(..))
import           Reflex.Vty.Test.Monad.Host
import Reflex.Vty.Test.Common

data BasicNetworkTest1 t (m :: Type -> Type)


fixedNoFocus
  :: (Reflex t, MonadFix m, MonadNodeId m)
  => Dynamic t Int
  -> VtyWidget t m a
  -> Layout t m a
fixedNoFocus sz = tile_ cfg . clickable where
  cfg = TileConfig {
      _tileConfig_constraint =  Constraint_Fixed <$> sz
      , _tile_Config_focusable = constDyn False
    }

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
      fixedNoFocus 7 $ boxTitle (constant def) "CLICK BUTTONS TO DRAW" $ do
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
    focusDynH <- subscribeDynamic _basicNetworkTest1_Output_focusDyn
    layoutTreeDynH <- subscribeDynamic _basicNetworkTest1_Output_layoutTreeDyn

    let
      readFocusDyn = readDynamic focusDynH
      readLayoutTreeDyn = readDynamic layoutTreeDynH

    -- fire an empty event and ensure that there is no focus
    fireQueuedEventsAndRead readFocusDyn >>= \a -> liftIO (checkSingle a Nothing)

    let nCells = 9 :: Int

    -- tab a bunch of times
    forM [0..99] $ \n -> do
      queueVtyEvent $ V.EvKey (V.KChar '\t') []
      fireQueuedEventsAndRead readFocusDyn >>= \a -> liftIO (checkSingle a (Just (n `mod` nCells)))

    -- shift tab a bunch of times
    forM [1..99] $ \n -> do
      queueVtyEvent $ V.EvKey V.KBackTab []
      fireQueuedEventsAndRead readFocusDyn >>= \a -> liftIO (checkSingle a (Just ((99-n) `mod` nCells)))

    -- TODO clear focus (you need to put the whole thing inside a pane to control focus)

    lt <- fireQueuedEventsAndRead readLayoutTreeDyn >>= return . Unsafe.head
    --liftIO . putStrLn . Tree.drawTree . ffor lt $ \(Region x y _ _) -> show x <> " " <> show y

    let
      testClickCell = forM [0..8] $ \n -> do
        let
          cell = [0, n `div` 3, n `mod` 3]
          (x,y) = fromJust $ layoutTreeCellToPosition cell lt
        -- click on a cell
        queueVtyEvent $ V.EvMouseDown x y V.BLeft []
        fireQueuedEventsAndRead readFocusDyn >>= \a -> liftIO (checkSingle a (Just (n `mod` nCells)))
        -- tab forward
        queueVtyEvent $ V.EvKey (V.KChar '\t') []
        fireQueuedEventsAndRead readFocusDyn >>= \a -> liftIO (checkSingle a (Just ((n+1) `mod` nCells)))

    -- click on cells
    testClickCell

    -- resize the window
    queueVtyEvent $ V.EvResize 50 50

    -- click on cells again
    testClickCell

    return ()


spec :: Spec
spec = do
  fromHUnitTest test_basic
