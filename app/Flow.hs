{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -threaded #-}

module Flow (
  flowMain
) where
import           Relude

import           Potato.Flow
import           Potato.Flow.Testing
import           Reflex.Potato.Helpers
import Potato.Reflex.Vty.Helpers


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Data.Functor.Misc
import qualified Data.Map                as Map
import           Data.Maybe
import qualified Data.Text               as T
import qualified Data.Text.Zipper        as TZ
import           Data.Time.Clock
import qualified Graphics.Vty            as V
import           Reflex
import           Reflex.Class.Switchable
import           Reflex.Network
import           Reflex.Vty

data CursorState = CSPan | CSSelect

dynLBox_to_dynRegion :: (Reflex t) => Dynamic t LBox -> DynRegion t
dynLBox_to_dynRegion dlb = r where
  x' = flip fmap dlb $ \(LBox (LPoint (V2 x y)) (LSize (V2 w h))) -> x
  y' = flip fmap dlb $ \(LBox (LPoint (V2 x y)) (LSize (V2 w h))) -> y
  w' = flip fmap dlb $ \(LBox (LPoint (V2 x y)) (LSize (V2 w h))) -> w
  h' = flip fmap dlb $ \(LBox (LPoint (V2 x y)) (LSize (V2 w h))) -> h
  r = DynRegion x' y' w' h'

translate_dynRegion :: (Reflex t) => Dynamic t (Int, Int) -> DynRegion t -> DynRegion t
translate_dynRegion pos dr = dr {
    _dynRegion_left = liftA2 (-) (_dynRegion_left dr) (fmap fst pos)
    , _dynRegion_top = liftA2 (-) (_dynRegion_top dr) (fmap snd pos)
  }

canvasScreen :: forall t m. (Reflex t, MonadHold t m, MonadFix m, MonadNodeId m)
  => Dynamic t Canvas
  -> VtyWidget t m ()
canvasScreen canvas = do
  pw <- displayWidth
  ph <- displayHeight
  let
    cursorState :: Behavior t CursorState
    cursorState = constant CSPan

    -- position in screen space of upper left corner of this pane
    panePos :: Dynamic t (Int, Int)
    panePos = constDyn (-20,-20)

  dragEv <- drag V.BLeft

  -- fill the background with whatever
  fill '░'

  -- draw the canvas
  -- TODO make this efficient -_-
  let
    canvasRegion = translate_dynRegion panePos $ dynLBox_to_dynRegion (fmap canvas_box canvas)
  pane canvasRegion (constDyn True) $ do
    text $ current (fmap canvasToText canvas)


  -- TODO info pane in bottom right corner


data Layers t = Layers {
  _layers_potatoAdd :: Event t (LayerPos, SEltLabel)
  , _layers_select  :: Event t LayerPos
}

layerScreen :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => Dynamic t SEltTree
  -> VtyWidget t m (Layers t)
layerScreen stree = do
  pw <- displayWidth
  ph <- displayHeight
  addButton <- col $ do
    fixed 1 $ debugFocus
    fixed 1 $ text . current . fmap (show . length)$ stree
    addButton <- fixed 3 $ textButtonStatic def "add"
    stretch $ col $ simpleList (fmap (zip [0..]) stree) $ \ds -> do
      fixed 1 $ text $ current $ fmap (_sEltLabel_name . snd) ds
    return addButton
  return Layers {
    _layers_potatoAdd = fmap (const (0, SEltLabel "meow" (SEltBox simpleSBox))) addButton
    , _layers_select = never
  }



flowMain :: IO ()
flowMain = mainWidget $ mdo
  -- external inputs
  currentTime <- liftIO $ getCurrentTime
  tickEv <- tickLossy 1 currentTime
  ticks <- foldDyn (+) 0 (fmap (const 1) tickEv)
  inp <- input

  -- potato flow stuff
  let
    undoEv = fforMaybe inp $ \case
      V.EvKey (V.KChar 'z') [V.MCtrl] -> Just ()
      _ -> Nothing
    redoEv = fforMaybe inp $ \case
      V.EvKey (V.KChar 'y') [V.MCtrl] -> Just ()
      _ -> Nothing
    pfc = PFConfig {
        _pfc_addElt     = _layers_potatoAdd layers
        , _pfc_removeElt  = never
        , _pfc_manipulate = never
        , _pfc_undo       = undoEv
        , _pfc_redo       = redoEv
        , _pfc_save = never
      }
  pfo <- holdPF pfc
  potatoUpdated <- delayEvent $ _pfo_potato_changed pfo
  let
    layerTree = _pfo_layers pfo
    stateUpdated = tag (_pfo_potato_state pfo) potatoUpdated
    selts = fmap (fmap (_sEltLabel_sElt)) $ _pfo_potato_state pfo
  treeDyn <- holdDyn [] stateUpdated
  --canvas :: Dynamic t Canvas
  canvas <- foldDyn potatoRender (emptyCanvas (LBox (LPoint (V2 0 0)) (LSize (V2 40 30)))) $ tag selts potatoUpdated

  -- compute regions
  dw <- displayWidth
  dh <- displayHeight
  let
    layersWidth = constDyn 30
    layerRegion = DynRegion 0 0 layersWidth dh
    wsRegion = DynRegion layersWidth 0 (liftA2 (-) dw layersWidth) dh

  -- layer pane
  layers <- pane layerRegion (constDyn True) $ layerScreen $ treeDyn


  -- workspace pane
  pane wsRegion (constDyn True) $ do
    canvasScreen canvas
    col $ do
      fixed 1 $ debugFocus
      fixed 5 $ do
        debugSize
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
