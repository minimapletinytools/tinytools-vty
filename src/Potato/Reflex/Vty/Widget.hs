{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Potato.Reflex.Vty.Widget
  ( splitH
  , splitHDrag
  ) where

import Prelude

import Control.Applicative (liftA2)
import Control.Monad.Fix (MonadFix)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import Graphics.Vty (Image)
import qualified Graphics.Vty as V

import Reflex
import Reflex.Class ()
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Reflex.Vty.Widget
import Reflex.Vty.Host


import Control.Monad.NodeId

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
  => VtyWidget t m ()
  -> VtyWidget t m a
  -> VtyWidget t m b
  -> VtyWidget t m (a,b)
splitHDrag wS wA wB = mdo
  dh <- displayHeight
  dw <- displayWidth
  w0 <- sample $ current dw
  dragE <- drag V.BLeft
  let
    splitter0 = w0 `div` 2
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
