{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


module Potato.Reflex.Vty.Widget
  (
  SingleClick(..)
  , singleClick
  , singleClickNoDragOffSimple
  , singleClickWithDownState
  , DoubleClickConfig(..)
  , doubleClick
  , doubleClickSimple

  , splitHDrag
  , DragState(..)
  , Drag2(..)
  , drag2
  ) where

import           Prelude

import qualified Graphics.Vty                  as V

import           Reflex
import           Reflex.Class                  ()
import           Reflex.Vty.Widget
import           Reflex.Vty.Widget.Input.Mouse



import           Control.Monad.NodeId
import           Control.Monad.Reader
import           System.Clock


-- currently only works for a SINGLE POINT
-- TODO integrate with pane2 so it reports clicks that happen on pane.
data SingleClick = SingleClick
  { _singleClick_button      :: V.Button
  , _singleClick_coordinates :: (Int, Int) -- ^ coordinates of down click
  , _singleClick_modifiers   :: [V.Modifier]
  , _singleClick_didDragOff  :: Bool
  }
  deriving (Eq, Ord, Show)


singleClick :: (Reflex t, MonadHold t m, MonadFix m, HasInput t m) => V.Button -> m (Event t SingleClick)
singleClick btn = do
  let
    -- TODO implement for pane2 instead
    withinBounds (Drag2 (fromX, fromY) (toX, toY) _ _ _) = fromX == toX && fromY == toY
  dragEv <- drag2 btn
  didStayOnDyn <- foldDyn (const . withinBounds) False dragEv
  return $ flip push dragEv $ \d@(Drag2 (fromX, fromY) _ _ mods ds) -> do
    didStayOn <- sample . current $ didStayOnDyn
    return $ if ds == DragEnd && withinBounds d
      then Just $ SingleClick btn (fromX, fromY) mods (not didStayOn)
      else Nothing

singleClickNoDragOffSimple :: (Reflex t, MonadHold t m, MonadFix m, HasInput t m) => V.Button -> m (Event t ())
singleClickNoDragOffSimple btn = do
  ev <- singleClick btn
  return $ fmapMaybe (\sc -> if _singleClick_didDragOff sc then Nothing else Just ()) ev


singleClickWithDownState :: (Reflex t, MonadHold t m, MonadFix m, HasInput t m) => V.Button -> m (Event t SingleClick, Dynamic t Bool)
singleClickWithDownState btn = do
  let
    -- TODO implement for pane2 instead
    withinBounds (Drag2 (fromX, fromY) (toX, toY) _ _ _) = fromX == toX && fromY == toY
  dragEv <- drag2 btn
  downDyn <- foldDyn (\(Drag2 _ _ _ _ ds) _ -> ds /= DragEnd) False dragEv
  didStayOnDyn <- foldDyn (const . withinBounds) False dragEv
  let
    scEv = flip push dragEv $ \d@(Drag2 (fromX, fromY) _ _ mods ds) -> do
      didStayOn <- sample . current $ didStayOnDyn
      return $ if ds == DragEnd && withinBounds d
        then Just $ SingleClick btn (fromX, fromY) mods (not didStayOn)
        else Nothing
  return (scEv, downDyn)

data DoubleClickConfig = DoubleClickConfig  {
    -- TODO lol...
    --_doubleClickConfig_spaceTolerance :: (Int, Int) -- the (x,y) mouse travel tolerance
    _doubleClickConfig_timeTolerance :: Integer -- the time (ms) between click tolerance
    , _dobuleClickConfig_button      :: V.Button
  }

doubleClick :: (Reflex t, MonadHold t m, MonadFix m, PerformEvent t m, MonadIO (Performable m), HasInput t m) => DoubleClickConfig -> m (Event t ())
doubleClick DoubleClickConfig {..} = do
  singleClickEv <- singleClickNoDragOffSimple _dobuleClickConfig_button
  singleClickTimeEv <- performEvent $ ffor singleClickEv $ \_ -> do
    liftIO $ getTime Monotonic
  lastClickTimeDyn <- holdDyn (-1) $ singleClickTimeEv
  (fmap (fmapMaybe id)) $ performEvent $ ffor (tag (current lastClickTimeDyn) singleClickEv) $ \ns -> do
    time <- liftIO $ getTime Monotonic
    return $ if (toNanoSecs $ time - ns) `div` 1000000 < _doubleClickConfig_timeTolerance 
      then Just () 
      else Nothing
  
doubleClickSimple :: (Reflex t, MonadHold t m, MonadFix m, PerformEvent t m, MonadIO (Performable m), HasInput t m) => m (Event t ())
doubleClickSimple = doubleClick DoubleClickConfig {
    --_doubleClickConfig_spaceTolerance = (0,0)
    _doubleClickConfig_timeTolerance = 300
    , _dobuleClickConfig_button = V.BLeft
  }


integralFractionalDivide :: (Integral a, Fractional b) => a -> a -> b
integralFractionalDivide n d = fromIntegral n / fromIntegral d

-- | A split of the available space into two parts with a draggable separator.
-- Starts with half the space allocated to each, and the first pane has focus.
-- Clicking in a pane switches focus.
splitHDrag :: (Reflex t, MonadFix m, MonadHold t m, HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m)
  => Int -- ^ initial width of left panel
  -> m ()
  -> m a
  -> m b
  -> m (a,b)
splitHDrag splitter0 wS wA wB = mdo
  dh <- displayHeight
  dw <- displayWidth
  w0 <- sample . current $ dw
  dragE <- drag V.BLeft
  splitterCheckpoint <- holdDyn splitter0 $ leftmost [fst <$> ffilter snd dragSplitter, resizeSplitter]
  splitterPos <- holdDyn splitter0 $ leftmost [fst <$> dragSplitter, resizeSplitter]
  splitterFrac <- holdDyn (integralFractionalDivide splitter0 w0) $ ffor (attach (current dw) (fst <$> dragSplitter)) $ \(w, x) ->
    fromIntegral x / (max 1 (fromIntegral w))
  let dragSplitter = fforMaybe (attach (current splitterCheckpoint) dragE) $
        \(splitterX, Drag (fromX, _) (toX, _) _ _ end) ->
          if splitterX == fromX then Just (toX, end) else Nothing
      regA = Region 0 0 <$> splitterPos <*> dh
      regS = Region <$> splitterPos <*> 0 <*> 1 <*> dh
      regB = Region <$> (splitterPos + 1) <*> 0 <*> (dw - splitterPos - 1) <*> dh
      resizeSplitter = ffor (attach (current splitterFrac) (updated dw)) $
        \(frac, w) -> round (frac * fromIntegral w)
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



data DragState = DragStart | Dragging | DragEnd deriving (Eq, Ord, Show)

-- | Same as 'Drag' but able to track drag start case
data Drag2 = Drag2
  { _drag2_from      :: (Int, Int) -- ^ Where the drag began
  , _drag2_to        :: (Int, Int) -- ^ Where the mouse currently is
  , _drag2_button    :: V.Button -- ^ Which mouse button is dragging
  , _drag2_modifiers :: [V.Modifier] -- ^ What modifiers are held
  , _drag2_state     :: DragState -- ^ Whether the drag ended (the mouse button was released)
  }
  deriving (Eq, Ord, Show)

-- | Same as 'drag' but returns 'Drag2' which tracks drag start events
drag2
  :: (Reflex t, MonadFix m, MonadHold t m, HasInput t m)
  => V.Button
  -> m (Event t Drag2)
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
