{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Potato.Reflex.Vty.Widget
  ( splitH
  , splitHDrag
  , DragState(..)
  , Drag2(..)
  , drag2
  , drag2_start
  ) where

import           Prelude

import           Control.Applicative  (liftA2)
import           Control.Monad.Fix    (MonadFix)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Zipper     as TZ
import           Graphics.Vty         (Image)
import qualified Graphics.Vty         as V

import           Reflex
import           Reflex.Class         ()
import           Reflex.Host.Class    (MonadReflexCreateTrigger)
import           Reflex.Vty.Host
import           Reflex.Vty.Widget


import           Control.Monad.NodeId

-- new stuff

-- | A plain split of the available space into vertically stacked panes.
-- No visual separator is built in here.
splitH :: (Reflex t, Monad m, MonadNodeId m)
       => Dynamic t (Int -> Int)
       -- ^ Function used to determine size of first pane based on available size
       -> Dynamic t (Bool, Bool)
       -- ^ How to focus the two sub-panes, given that we are focused.
       -> VtyWidget t m a
       -- ^ Widget for first pane
       -> VtyWidget t m b
       -- ^ Widget for second pane
       -> VtyWidget t m (a,b)
splitH sizeFunD focD wA wB = do
  dw <- displayWidth
  dh <- displayHeight
  let regA = DynRegion
        { _dynRegion_left = pure 0
        , _dynRegion_top = pure 0
        , _dynRegion_width = sizeFunD <*> dw
        , _dynRegion_height = dh
        }
      regB = DynRegion
        { _dynRegion_left = _dynRegion_width regA
        , _dynRegion_top =  pure 0
        , _dynRegion_width = liftA2 (-) dw (_dynRegion_width regA)
        , _dynRegion_height = dh
        }
  ra <- pane regA (fst <$> focD) wA
  rb <- pane regB (snd <$> focD) wB
  return (ra,rb)


-- | A split of the available space into two parts with a draggable separator.
-- Starts with half the space allocated to each, and the first pane has focus.
-- Clicking in a pane switches focus.
splitHDrag :: (Reflex t, MonadFix m, MonadHold t m, MonadNodeId m)
  => Int -- ^ initial width of left panel
  -> VtyWidget t m ()
  -> VtyWidget t m a
  -> VtyWidget t m b
  -> VtyWidget t m (a,b)
splitHDrag splitter0 wS wA wB = mdo
  dh <- displayHeight
  dw <- displayWidth
  dragE <- drag V.BLeft
  splitterCheckpoint <- holdDyn splitter0 $ leftmost [fst <$> ffilter snd dragSplitter, resizeSplitter]
  splitterPos <- holdDyn splitter0 $ leftmost [fst <$> dragSplitter, resizeSplitter]
  splitterFrac <- holdDyn ((1::Double) / 2) $ ffor (attach (current dh) (fst <$> dragSplitter)) $ \(h, x) ->
    fromIntegral x / (max 1 (fromIntegral h))
  let dragSplitter = fforMaybe (attach (current splitterCheckpoint) dragE) $
        \(splitterX, Drag (fromX, _) (toX, _) _ _ end) ->
          if splitterX == fromX then Just (toX, end) else Nothing
      regA = DynRegion 0 0 splitterPos dh
      regS = DynRegion splitterPos 0 1 dh
      regB = DynRegion (splitterPos + 1) 0 (dw - splitterPos - 1) dh
      resizeSplitter = ffor (attach (current splitterFrac) (updated dw)) $
        \(frac, h) -> round (frac * fromIntegral h)
  focA <- holdDyn True $ leftmost
    [ True <$ mA
    , False <$ mB
    ]
  (mA, rA) <- pane regA focA $ withMouseDown wA
  pane regS (pure False) wS
  (mB, rB) <- pane regB (not <$> focA) $ withMouseDown wB
  return (rA, rB)
  where
    withMouseDown x = do
      m <- mouseDown V.BLeft
      x' <- x
      return (m, x')


-- updated stuff

data DragState = DragStart | Dragging | DragEnd deriving (Eq, Ord, Show)

-- | Information about a drag operation
data Drag2 = Drag2
  { _drag2_from      :: (Int, Int) -- ^ Where the drag began
  , _drag2_to        :: (Int, Int) -- ^ Where the mouse currently is
  , _drag2_button    :: V.Button -- ^ Which mouse button is dragging
  , _drag2_modifiers :: [V.Modifier] -- ^ What modifiers are held
  , _drag2_state     :: DragState -- ^ Whether the drag ended (the mouse button was released)
  }
  deriving (Eq, Ord, Show)

-- | Converts raw vty mouse drag events into an event stream of 'Drag's
drag2
  :: (Reflex t, MonadFix m, MonadHold t m)
  => V.Button
  -> VtyWidget t m (Event t Drag2)
drag2 btn = mdo
  inp <- input
  let f :: Maybe Drag2 -> V.Event -> Maybe Drag2
      f Nothing = \case
        V.EvMouseDown x y btn' mods
          | btn == btn' -> Just $ Drag2 (x,y) (x,y) btn' mods DragStart
          | otherwise   -> Nothing
        _ -> Nothing
      f (Just (Drag2 from _ _ mods st)) = \case
        V.EvMouseDown x y btn' mods'
          | st == DragEnd && btn == btn'  -> Just $ Drag2 (x,y) (x,y) btn' mods' DragStart
          | btn == btn'         -> Just $ Drag2 from (x,y) btn mods' Dragging
          | otherwise           -> Nothing -- Ignore other buttons.
        V.EvMouseUp x y (Just btn')
          | st == DragEnd        -> Nothing
          | btn == btn' -> Just $ Drag2 from (x,y) btn mods DragEnd
          | otherwise   -> Nothing
        V.EvMouseUp x y Nothing -- Terminal doesn't specify mouse up button,
                                -- assume it's the right one.
          | st == DragEnd      -> Nothing
          | otherwise -> Just $ Drag2 from (x,y) btn mods DragEnd
        _ -> Nothing
  let
    newDrag = attachWithMaybe f (current dragD) inp
  dragD <- holdDyn Nothing $ Just <$> newDrag
  return (fmapMaybe id $ updated dragD)

drag2_start :: (Reflex t) => Event t Drag2 -> Event t Drag2
drag2_start = ffilter (\x -> _drag2_state x == DragStart)
